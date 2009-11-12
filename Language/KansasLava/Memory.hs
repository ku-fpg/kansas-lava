{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Language.KansasLava.Memory
  (MemOp(..),readMem, writeMem,  bram) where
-- import Language.KansasLava
import Language.KansasLava.Signal
import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Language.KansasLava.Sequential

import Data.Sized.Unsigned
import Data.Sized.Ix

import qualified Data.Map as M
import Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
import Control.Applicative

-- | a 'MemOp' is either a write (with address and data) or read (with address)
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
        $ Port (Var "o0")
        $ E
        $ Entity (Name "Lava" "BRAM") [(Var "o0",dataTy)]
            [(Var "clk",ClkTy,tm_w), (Var "rst",RstTy,r_w),
             (Var "we",B,weDeep),
             (Var "ain",addrTy,addrDeep),
             (Var "din",dataTy,dataDeep)
            ] Nothing


  where mem = mapAccumLS memop (initMem 2 imap)
        (Signal weShallow weDeep) = bitIndex 0 op
        addr :: Signal a
        addr@(Signal _ addrDeep) = bitSlice (baseTypeLength addrTy) 1 op
        addrTy = tyRep (error "bram:addrType" :: d)
        addrWidth = baseTypeLength addrTy
        dataTy = tyRep (error "bram:dataType" :: a)
        dataWidth = baseTypeLength dataTy
        dat :: Signal d
        dat@(Signal _ dataDeep) = bitSlice (dataWidth + addrWidth) (addrWidth + 1) op



bitIndex :: forall a . (OpType a) => Int -> Signal a -> Signal Bool
bitIndex idx (Signal s d)
                   = Signal (error "bitIndex") -- FIXME: Get the slicing down.
                   $ Port (Var "o")
                   $ E
                   $ Entity (Name "Lava" "index")
                       [(Var "o0",B)] -- outputs
                       [(Var "i", iTy,d)
                       ,(Var "index",U 32,Lit (toInteger idx))] -- inputs
                       Nothing

  where iTy = tyRep (error "bitIndex:types" :: a)

bitSlice :: forall a b . (OpType a, OpType b) => Int -> Int ->  Signal a -> Signal b
bitSlice high low (Signal s d)
                   = Signal (error "bitSliceCast") -- FIXME: Get the slicing down.
                   $ Port (Var "o0")
                   $ E
                   $ Entity (Name "Lava" "slice")
                       [(Var "o0",oTy)] -- outputs
                       [(Var "i",iTy, d)
                       ,(Var "low",U 32,Lit (toInteger low))
                       ,(Var "high",U 32,Lit (toInteger high))] -- inputs
                     Nothing
  where iTy = tyRep (error "bitSliceCase:iTy" :: a)
        oTy = tyRep (error "bitSliceCase:oTy" :: b)





readMem :: forall a d . (OpType a, OpType d) =>
           Signal a -> Signal (MemOp a d)
readMem (Signal addr addrDeep)  =
        Signal (fmap R addr)
            $ Port (Var "o0")
            $ E
            $ Entity (Name "Lava" "concat") [(Var "o0",U size)]
                [(Var "i0", U 1, Lit 0),
                 (Var "i1", aTy,addrDeep),
                 (Var "i2",dTy, Lit 0)
                ]
                Nothing
  where size = baseTypeLength $ tyRep (error "readMem" :: (MemOp a d))
        aTy = tyRep (error "readMem:aTy" :: a)
        dTy = tyRep (error "readMem:dTy" :: d)




writeMem :: forall a d. (OpType a, OpType d) => Signal a -> Signal d -> Signal (MemOp a d)
writeMem addr@(Signal addrShallow addrDeep) dat@(Signal val valDeep)  =
        Signal (W <$> addrShallow <*> val)
            $ Port (Var "o0")
            $ E
            $ Entity (Name "Lava" "concat") [(Var "o0",U size)]
                [(Var "i0", U 1, Lit 0),
                 (Var "i1", aTy,addrDeep),
                 (Var "i2",dTy, valDeep)
                ]
                Nothing
  where size = baseTypeLength $ bitTypeOf (error "writeMem" :: Signal (MemOp a d))
        aTy = tyRep (error "writeMem:aTy" :: a)
        dTy = tyRep (error "writeMem:dTy" :: d)


instance (OpType a, OpType d) =>  OpType (MemOp a d) where
   bitTypeOf _ = size
    where size = U $  (baseTypeLength (bitTypeOf $ ((error "bitTypeOf (MemOp a d)") :: Signal a))) + (baseTypeLength (bitTypeOf $ ((error "bitTypeOf (MemOp a d)") :: Signal a))) + 1
   op = error "op undefined for MemOp"
   initVal = readMem initVal

{-
instance (TyRep a, TyRep d) =>  TyRep (MemOp a d) where
   tyRep _ = size
    where size = U $  (baseTypeLength (tyRep $ ((error "tyRep (MemOp a d)") :: a))) +
                      (baseTypeLength (tyRep $ ((error "tyRep (MemOp a d)") :: d))) + 1

-}


type Unsigned16 = Unsigned X16
baseBRAM :: Time -> Signal Unsigned16 -> Signal Unsigned16
baseBRAM clk addr = bram [] clk op
  where op = writeMem addr 0
