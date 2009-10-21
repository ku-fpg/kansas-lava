{-# LANGUAGE ScopedTypeVariables #-}
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
  toBoolMatrix   :: (Size ix, Enum ix) => Signal (c ix) -> Matrix ix (Signal Bool)
  fromBoolMatrix :: (Size ix, Enum ix) => Matrix ix (Signal Bool) -> Signal (c ix)


--
-- To consider, these ops
--	:: Signal (Matrix ix Bool) -> Matrix ix (Signal Bool)
--	:: Matrix ix (Signal Bool) -> Signal (Matrix ix Bool)

-- The Seq versions
pushin :: Size i => Seq (Matrix i t) -> Matrix i (Seq t)
pushin  m = M.fromList (fmapConst m)
  where fmapConst (Nothing :~ as) = Data.List.zipWith (:~) nothings (fmapConst as)
        fmapConst (Just a :~ as) = Data.List.zipWith (:~) (map Just (M.toList a)) (fmapConst as)
        fmapConst (Constant Nothing) = map Constant nothings
        fmapConst (Constant (Just a)) = map (Constant . Just) (M.toList a)
        nothings = replicate (98 {-size (undefined :: ix) -}) Nothing

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


-- signalMatrixBoolToMatrixSignalBool ::
matrixSignalBoolToSignalMatrixBool :: (Size ix) => Matrix ix (Signal Bool) -> Signal (Matrix ix Bool)
matrixSignalBoolToSignalMatrixBool m
        = o0
	$ ESignal (pullout mSeq)
        $ E
        $ Entity (Name "Matrix" "matrixSignalBoolToSignalMatrixBool")
 		 [Var "o0"]
		 (zip inVars (map (\ ~(Signal _ w) -> w) (M.toList m)))
		 -- output is a matrix of bool, which is the *same* rep as an Unsigned of the same sized
		 [ BaseTy B 		 : map TyVar inVars
		 , BaseTy (U (M.length m)) : [TyVar (Var "o0")]
		 ]
   where inVars = [Var ("i" ++ show i) | i <- indices m ]
         mSeq = fmap (\ ~(Signal a _) -> a) m



signalMatrixBoolToSignalUnsigned :: (Enum ix, Size ix) => Signal (Matrix ix Bool) -> Signal (Unsigned ix)
signalMatrixBoolToSignalUnsigned  x =
	o0 $ entity1 (Name "Matrix" "signalMatrixBoolToSignalUnsigned") inputs [Var "o0"] tyeqs fn x
	where allNames = inputs ++ [Var "o0"]
	      tyeqs    = [ map TyVar allNames ]-- TODO: add monomorphism
	      inputs   = map Var ["i0"]
	      fn = U.fromMatrix

--instance BitRep Signed where
instance BitRep Unsigned where
  toBoolMatrix sig = forAll $ \ i -> testABit sig (fromEnum i)
  fromBoolMatrix = signalMatrixBoolToSignalUnsigned . matrixSignalBoolToSignalMatrixBool



