{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
-- | The 'Memory' module provides an interface for using synchronous BlockRAM
--   storage within Lava.
module Language.KansasLava.Memory
  (MemOp, readMem, writeMem,  bram) where

import Language.KansasLava.Signal
import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Language.KansasLava.Sequential


import qualified Data.Map as M
import Control.Applicative

-- | A 'MemOp' is either a write (with a ddress and data) or read (with
-- | address). It is exported abstract, so 'readMem' and 'writeMem' functions
-- | should be used to construct values.
data MemOp a d = W a d
               | R a
               deriving (Show,Eq)


type Memory a d = (M.Map a d, [Maybe d])
initMem :: Ord a => Int -> [(a,d)] -> Memory a d
initMem readLatency vals = (m, replicate readLatency Nothing)
  where m = M.fromList vals

enqueue :: a -> [a] -> [a]
enqueue v vs = vs ++ [v]

dequeue :: [a] -> (a,[a])
dequeue (v:vs) = (v,vs)
dequeue [] = error "can't dequeue from empty list"


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



-- The Signal implementation...
-- | 'bram' constructs a BlockRam component.
bram ::  forall a d . (OpType a, OpType d, Ord a) =>
         [(a, d)] ->
         Time ->  -- Clock/Reset
         Signal (MemOp a d) -> -- operation
         Signal d -- output value
bram imap  ~(Time ~(Signal _ tm_w) ~(Signal _ r_w))
           ~memOp@(Signal opShallow _)
        = Signal (mem opShallow)
        $ Port (Var "o0")
        $ E
        $ Entity (Name "Lava" "BRAM") [(Var "o0",dataTy)]
            [(Var "clk",ClkTy,tm_w), (Var "rst",RstTy,r_w),
             (Var "we",B,weDeep),
             (Var "ain",addrTy,addrDeep),
             (Var "din",dataTy,dataDeep)
            ] []


  where mem = mapAccumLS memop (initMem 2 imap)
        (Signal _ weDeep) = bitIndex 0 memOp
        (Signal _ addrDeep) :: Signal a = bitSlice (baseTypeLength addrTy) 1 memOp
        addrTy = tyRep (error "bram:addrType" :: d)
        addrWidth = baseTypeLength addrTy
        dataTy = tyRep (error "bram:dataType" :: a)
        dataWidth = baseTypeLength dataTy
        (Signal _ dataDeep) :: Signal a = bitSlice (dataWidth + addrWidth) (addrWidth + 1) memOp



bitIndex :: forall a . (OpType a) => Int -> Signal a -> Signal Bool
bitIndex idx (Signal _ d)
                   = Signal (error "bitIndex") -- FIXME: Get the slicing down.
                   $ Port (Var "o")
                   $ E
                   $ Entity (Name "Lava" "index")
                       [(Var "o0",B)] -- outputs
                       [(Var "i", iTy,d)
                       ,(Var "index",U 32,Lit (toInteger idx))] -- inputs
                       []

  where iTy = tyRep (error "bitIndex:types" :: a)

bitSlice :: forall a b . (OpType a, OpType b) => Int -> Int ->  Signal a -> Signal b
bitSlice high low (Signal _ d)
                   = Signal (error "bitSliceCast") -- FIXME: Get the slicing down.
                   $ Port (Var "o0")
                   $ E
                   $ Entity (Name "Lava" "slice")
                       [(Var "o0",oTy)] -- outputs
                       [(Var "i",iTy, d)
                       ,(Var "low",U 32,Lit (toInteger low))
                       ,(Var "high",U 32,Lit (toInteger high))] -- inputs
                     []
  where iTy = tyRep (error "bitSliceCase:iTy" :: a)
        oTy = tyRep (error "bitSliceCase:oTy" :: b)




-- | 'readMem' constructs a read memory request from an address.
readMem :: forall a d . (OpType a, OpType d) =>
           Signal a -> Signal (MemOp a d)
readMem (Signal addr addrDeep)  =
        Signal (fmap R addr)
            $ Port (Var "o0")
            $ E
            $ Entity (Name "Lava" "concat") [(Var "o0",U opSize)]
                [(Var "i0", U 1, Lit 0),
                 (Var "i1", aTy,addrDeep),
                 (Var "i2",dTy, Lit 0)
                ]
                []
  where opSize = baseTypeLength $ tyRep (error "readMem" :: (MemOp a d))
        aTy = tyRep (error "readMem:aTy" :: a)
        dTy = tyRep (error "readMem:dTy" :: d)



-- | 'writeMem' constructs a write memory request, given an address and a value to write.
writeMem :: forall a d. (OpType a, OpType d) => Signal a -> Signal d -> Signal (MemOp a d)
writeMem (Signal addrShallow addrDeep) (Signal val valDeep)  =
        Signal (W <$> addrShallow <*> val)
            $ Port (Var "o0")
            $ E
            $ Entity (Name "Lava" "concat") [(Var "o0",U opSize)]
                [(Var "i0", U 1, Lit 0),
                 (Var "i1", aTy,addrDeep),
                 (Var "i2",dTy, valDeep)
                ]
                []
  where opSize = baseTypeLength $ bitTypeOf (error "writeMem" :: Signal (MemOp a d))
        aTy = tyRep (error "writeMem:aTy" :: a)
        dTy = tyRep (error "writeMem:dTy" :: d)


instance (OpType a, OpType d) =>  OpType (MemOp a d) where
   bitTypeOf _ = opSize
    where opSize = U $  (baseTypeLength (bitTypeOf $ ((error "bitTypeOf (MemOp a d)") :: Signal a))) + (baseTypeLength (bitTypeOf $ ((error "bitTypeOf (MemOp a d)") :: Signal a))) + 1
   op = error "op undefined for MemOp"
   initVal = readMem initVal

{-
instance (TyRep a, TyRep d) =>  TyRep (MemOp a d) where
   tyRep _ = size
    where size = U $  (baseTypeLength (tyRep $ ((error "tyRep (MemOp a d)") :: a))) +
                      (baseTypeLength (tyRep $ ((error "tyRep (MemOp a d)") :: d))) + 1

-}

{-
type Unsigned16 = Unsigned X16
baseBRAM :: Time -> Signal Unsigned16 -> Signal Unsigned16
baseBRAM bramClk addr = bram [] bramClk writeOp
  where writeOp = writeMem addr 0
-}