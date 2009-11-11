{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,FlexibleContexts,UndecidableInstances #-}
module Language.KansasLava.Matrix where

import Data.Sized.Unsigned as U
import Data.Sized.Signed as S
import Data.Sized.Ix as X
import Data.Sized.Matrix as M
import Data.List
import Data.Maybe
import Control.Applicative

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
--	:: Signal (Matrix ix Bool) -> Matrix ix (Signal Bool)
--	:: Matrix ix (Signal Bool) -> Signal (Matrix ix Bool)

-- The Seq versions
pushin :: forall i t . Size i => Seq (Matrix i t) -> Matrix i (Seq t)
pushin  m = M.fromList (fmapConst m)
  where fmapConst (Nothing :~ as) = Data.List.zipWith (:~) nothings (fmapConst as)
        fmapConst (Just a :~ as) = Data.List.zipWith (:~) (map Just (M.toList a)) (fmapConst as)
        fmapConst (Constant Nothing) = map Constant nothings
        fmapConst (Constant (Just a)) = map (Constant . Just) (M.toList a)
        nothings = replicate (size (undefined :: i)) Nothing

pullout :: Size i => Matrix i (Seq t) -> Seq (Matrix i t)
pullout m = combine (M.toList m)
  where combine seqs
          | constant && valid = Constant (Just (M.fromList values))
          | constant && not valid = Constant Nothing
          | not constant && valid = Just (M.fromList values) :~ combine tails
          | not constant && not valid = Nothing :~ combine tails
          where heads = map Seq.head seqs
                tails = map Seq.tail seqs
                valid = and $ map isJust heads
                constant = and $ map isConstant seqs
                values = map fromJust heads
                isConstant (Constant _) = True
                isConstant _ = False

signalMatrixBoolToMatrixSignalBool ::
  forall ix. (Size ix) => Matrix ix (Signal Bool) -> Signal (Matrix ix Bool)
signalMatrixBoolToMatrixSignalBool = matrixSignalToSignalMatrix

matrixSignalToSignalMatrix :: forall ix a. (OpType a, Size ix) => Matrix ix (Signal a) -> Signal (Matrix ix a)
matrixSignalToSignalMatrix m
        = o0
	$ ESignal (pullout mSeq)
        $ E
        $ Entity (Name "Lava" "concat")
 		 [(Var "o0", oTy)]
		 (zip3 inVars (repeat aTy) (map (\ ~(Signal _ w) -> w) (M.toList m)))
   where inVars = [Var ("i" ++ show i) | i <- indices m ]
         mSeq = fmap (\ ~(Signal a _) -> a) m
         oTy = U $ (size (undefined :: ix)) * (baseTypeLength aTy)
         aTy = tyRep (error "matrixSignalSignalMatrix" :: a)




signalMatrixBoolToSignalUnsigned :: forall ix. (Enum ix, Size ix) => Signal (Matrix ix Bool) -> Signal (Unsigned ix)
signalMatrixBoolToSignalUnsigned  x =
	o0 $ entity1 (Name "Matrix" "signalMatrixBoolToSignalUnsigned") inputs [Var "o0"] fn x
	where allNames = inputs ++ [Var "o0"]
	      inputs   = map Var ["i0"]
	      fn = U.fromMatrix
              oTy = U (size (undefined :: ix))

--instance BitRep Signed where
instance BitRep Unsigned where
  toBoolMatrix sig = forAll $ \ i -> testABit sig (fromEnum i)
  fromBoolMatrix = signalMatrixBoolToSignalUnsigned . matrixSignalToSignalMatrix


-- instance Size ix => TyRep (Matrix ix Bool) where
--   tyRep m = U (size (error "TyRep(Matrix ix Bool)" :: ix))

instance (OpType a, Size ix) => OpType (Matrix ix a) where
  bitTypeOf _ =  U (size (undefined :: ix) * baseTypeLength (tyRep (undefined :: a)))
  op _ _  = error "OpType (Matrix ix a)"
  initVal = error "initVal (Matix ix a)"

instance (OpType a, Size ix) => OpType (Matrix ix (Signal a)) where
  bitTypeOf _ = U (size (undefined :: ix) * baseTypeLength (tyRep (undefined :: a)))
  op _ _  = error "OpType (Matrix ix (Signal Bool))"
  initVal = error "initVal (Matix ix (Signal Bool))"