{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,FlexibleContexts,UndecidableInstances #-}
module Language.KansasLava.Matrix where

import Data.Sized.Unsigned as U
import Data.Sized.Signed as S
import Data.Sized.Ix as X
import Data.Sized.Matrix as M
import Data.List
import Data.Maybe

import Language.KansasLava.Signal
import Language.KansasLava.Seq as Seq
import Language.KansasLava.Logic
import Language.KansasLava.Type
import Language.KansasLava.Entity

class BitRep c where
  toBoolMatrix   :: (OpType (Matrix ix (Signal Bool)), Size ix, Enum ix) => Signal (c ix) -> Matrix ix (Signal Bool)
  fromBoolMatrix :: (OpType (Matrix ix Bool),Size ix, Enum ix) => Matrix ix (Signal Bool) -> Signal (c ix)


--
-- To consider, these ops
--	:: Matrix ix (Signal Bool) -> Signal (Matrix ix Bool)
--	:: Signal (Matrix ix Bool) -> Matrix ix (Signal Bool)

fff :: (Size ix) => Matrix ix (Signal Bool) -> Seq (Matrix ix Bool)
fff m = ggg (fmap (\ ~(Signal a _) -> a) m)

ggg :: forall ix a . (Size ix) => Matrix ix (Seq a) -> Seq (Matrix ix a)
ggg m0 = m6
  where
	m1 :: Matrix ix [Maybe a]
 	m1 = fmap Seq.toList m0
	m2 :: [[Maybe a]]
	m2 = M.toList m1
	m3 :: [[Maybe a]]
	m3 = Data.List.transpose m2
	m4 :: [Maybe [a]]
	m4 = [ Just (catMaybes a) | a <- m3 ]
	m5 :: Seq [a]
	m5 = Seq.fromList m4
	m6 :: Seq (Matrix ix a)
	m6 = fmap M.fromList m5


-- signalMatrixBoolToMatrixSignalBool ::
matrixSignalBoolToSignalMatrixBool :: forall ix . (Size ix) => Matrix ix (Signal Bool) -> Signal (Matrix ix Bool)
matrixSignalBoolToSignalMatrixBool m
        = o0
	$ ESignal (fff m)
        $ E
        $ Entity (Name "Matrix" "matrixSignalBoolToSignalMatrixBool")
		 -- output is a matrix of bool, which is the *same* rep as an Unsigned of the same sized
 		 [(Var "o0", U (size (error "matrixSignalBoolToSignalMatrixBool" :: ix)))]

		 (zip3 inVars (repeat B) (map (\ ~(Signal _ w) -> w) (M.toList m)))

   where inVars = [Var ("i" ++ show i) | i <- indices m ]


signalMatrixBoolToSignalUnsigned :: (Size ix,OpType (Matrix ix Bool), Enum ix) => Signal (Matrix ix Bool) -> Signal (Unsigned ix)
signalMatrixBoolToSignalUnsigned  x =
	o0 $ entity1 (Name "Matrix" "signalMatrixBoolToSignalUnsigned") inputs [Var "o0"] fn x
	where allNames = inputs ++ [Var "o0"]
	      inputs   = map Var ["i0"]
	      fn = U.fromMatrix

--instance BitRep Signed where
--instance (Size ix, OpType (Matrix ix Bool)) =>  BitRep Unsigned where
instance   BitRep Unsigned where
  toBoolMatrix sig = forAll $ \ i -> testABit sig (fromEnum i)
  fromBoolMatrix = signalMatrixBoolToSignalUnsigned . matrixSignalBoolToSignalMatrixBool


-- instance Size ix => TyRep (Matrix ix Bool) where
--   tyRep m = U (size (error "TyRep(Matrix ix Bool)" :: ix))

instance Size ix => OpType (Matrix ix Bool) where
  bitTypeOf _ = U (size (undefined :: ix))
  op _ _  = error "OpType (Matrix ix Bool)"
  initVal = error "initVal (Matix ix Bool)"

instance Size ix => OpType (Matrix ix (Signal Bool)) where
  bitTypeOf _ = U (size (undefined :: ix))
  op _ _  = error "OpType (Matrix ix (Signal Bool))"
  initVal = error "initVal (Matix ix (Signal Bool))"