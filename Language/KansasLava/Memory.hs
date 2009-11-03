{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Language.KansasLava.Memory
  (MemOp(..),readMem, writeMem,  bram) where
import Language.KansasLava
import Data.Sized.Unsigned
import Data.Sized.Ix

import qualified Data.Map as M
import Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
import Control.Applicative

data MemOp a d = W a d
               | R a
               deriving (Show,Eq)



type Memory a d = (M.Map a d, [Maybe d])
initMem :: Ord a => Int -> [(a,d)] -> Memory a d
initMem readLatency vals = (m, replicate readLatency Nothing)
  where m = M.fromList vals

enqueue v vs = vs ++ [v]
dequeue (v:vs) = (v,vs)


memop :: Ord a => Memory a d -> Maybe (MemOp a d) -> (Memory a d,Maybe d)
memop (m,ds) Nothing = ((m,vs),v)
  where (v,vs) = dequeue (enqueue Nothing ds)
memop (m,ds) (Just (R a)) = ((m,vs),v)
  where val = M.lookup a m
        (v,vs) = dequeue (enqueue val ds)
memop (m,ds) (Just (W a d)) = ((m',vs),v)
  where m' = M.insert a d m
        (v,vs) = dequeue (enqueue Nothing ds)


mapAccumLS :: (s -> Maybe a -> (s,Maybe b)) -> s -> Seq a -> Seq b
mapAccumLS f acc (a :~ as) = o :~ (mapAccumLS f acc' as)
  where (acc',o) = f acc a
mapAccumLS f acc as@(Constant a) = o :~ mapAccumLS f acc' as
   where (acc',o) = f acc a


-- Copy test moves the values from one memory to another. The ugly part includes
-- the need to line up read requests for the second memory.
counter clk = out'
  where out = delay clk 0 out'
        out' :: Signal Int
        out' =  1 + out



delayN n clock input = foldr ($) input (replicate n (delay clock initVal))

instance F.Foldable Signal where
  foldMap _ _ = error "Foldable.foldMap Signal not defined"

instance T.Traversable Signal where
 traverse f (Signal s d) = Signal <$> traverse f s <*> pure d




-- The Signal implementation...
bram ::  forall a d . (OpType a, OpType d, Ord a) =>
         [(a, d)] ->
         Time ->  -- Clock/Reset
         Signal (MemOp a d) -> -- operation
         Signal d -- output value

bram imap  ~(Time ~(Signal tm tm_w) ~(Signal r r_w))
           ~op@(Signal opShallow opDeep)
        = Signal (mem opShallow)
        $ Port (Var "o")
        $ E
        $ Entity (Name "Lava" "BRAM") [Var "o"]
            [(Var "clk",tm_w), (Var "rst", r_w),
             (Var "we", weDeep),
             (Var "ain",addrDeep),
             (Var "din", dataDeep)
            ]
        types
  where mem = mapAccumLS memop (initMem 2 imap)
        (Signal weShallow weDeep) = bitIndex 0 op
        (Signal _ addrDeep) = bitSliceCast (baseTypeLength addrTy) 1 (BaseTy addrTy) op
        addrTy = bitTypeOf (error "bram:addrType" :: Signal d)
        addrWidth = baseTypeLength addrTy
        dataTy = bitTypeOf (error "bram:dataType" :: Signal a)
        dataWidth = baseTypeLength dataTy
        (Signal _ dataDeep) = bitSliceCast (dataWidth + addrWidth) (addrWidth + 1) (BaseTy dataTy) op


        types = [ [TyVar $ Var "o", BaseTy dataTy]
                , [TyVar $ Var "clk", BaseTy ClkTy]
                , [TyVar $ Var "rst", BaseTy RstTy]
                , [TyVar $ Var "we", BaseTy B]
                , [TyVar $ Var "ain", BaseTy addrTy]
                , [TyVar $ Var "din", BaseTy dataTy]
		]



bitIndex :: forall a . OpType a => Int -> Signal a -> Signal Bool
bitIndex idx (Signal s d)
                   = Signal (error "bitIndex") -- FIXME: Get the slicing down.
                   $ Port (Var "o")
                   $ E
                   $ Entity (Name "Lava" "index")
                       [Var "o"] -- outputs
                       [(Var "i", d)
                       ,(Var "index",Lit (toInteger idx))] -- inputs
                       types
  where types = [[TyVar $ Var "o", BaseTy B]
                ,[TyVar $ Var "i", BaseTy (bitTypeOf (error "bitIndex:types" :: Signal a))]]


bitSlice high low = bitSliceCast high low (BaseTy (U (high - low + 1)))

bitSliceCast :: forall a b . OpType a => Int -> Int -> Ty Var -> Signal a -> Signal b
bitSliceCast high low cast (Signal s d)
                   = Signal (error "bitSliceCast") -- FIXME: Get the slicing down.
                   $ Port (Var "o")
                   $ E
                   $ Entity (Name "Lava" "slice")
                       [Var "o"] -- outputs
                       [(Var "i", d)
                       ,(Var "low",Lit (toInteger low))
                       ,(Var "high",Lit (toInteger high))] -- inputs
                       types
  where types = [[TyVar $ Var "o", cast]
                ,[TyVar $ Var "i", BaseTy (bitTypeOf (error "bitSliceCase:types" :: Signal a))]]




readMem :: forall a d . (OpType a, OpType d) =>
           Signal a -> Signal (MemOp a d)
readMem (Signal addr addrDeep)  =
        Signal (fmap R addr)
            $ Port (Var "o")
            $ E
            $ Entity (Name "Lava" "concat") [Var "o"]
                [(Var "i0", Lit 0),
                 (Var "i1", addrDeep),
                 (Var "i2",Lit 0)
                ]
              types
  where types = [[TyVar (Var "i0"), BaseTy (U 1)]
                ,[TyVar (Var "i1"), BaseTy (bitTypeOf (error "readMem:types" :: Signal a))]
                ,[TyVar (Var "i2"), BaseTy (bitTypeOf (error "readMem:types" :: Signal d))]
                ,[TyVar (Var "o"), BaseTy (U size)]]
        size = baseTypeLength $ bitTypeOf (error "readMem" :: Signal (MemOp a d))




writeMem :: forall a d. (OpType a, OpType d) => Signal a -> Signal d -> Signal (MemOp a d)
writeMem addr@(Signal addrShallow addrDeep) dat@(Signal val valDeep)  =
        Signal (W <$> addrShallow <*> val)
            $ Port (Var "o")
            $ E
            $ Entity (Name "Lava" "concat") [Var "o"]
                [(Var "i0", Lit 0),
                 (Var "i1", addrDeep),
                 (Var "i2",valDeep)
                ]
              types
  where types = [[TyVar (Var "i0"), BaseTy (U 1)]
                ,[TyVar (Var "i1"), BaseTy (bitTypeOf addr)]
                ,[TyVar (Var "i2"), BaseTy (bitTypeOf dat)]
                ,[TyVar (Var "o"), BaseTy (U size)]]
        size = baseTypeLength $ bitTypeOf (error "writeMem" :: Signal (MemOp a d))



instance (OpType a, OpType d) =>  OpType (MemOp a d) where
   bitTypeOf _ = size
    where size = U $  (baseTypeLength (bitTypeOf $ ((error "bitTypeOf (MemOp a d)") :: Signal a))) + (baseTypeLength (bitTypeOf $ ((error "bitTypeOf (MemOp a d)") :: Signal a))) + 1
   op = error "op undefined for MemOp"
   initVal = readMem initVal




type Unsigned16 = Unsigned X16
baseBRAM :: Time -> Signal Unsigned16 -> Signal Unsigned16
baseBRAM clk addr = bram [] clk op
  where op = writeMem addr 0
