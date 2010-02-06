{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,FlexibleContexts,UndecidableInstances, TypeFamilies  #-}
module Language.KansasLava.Matrix where

import Data.Sized.Unsigned as U
import Data.Sized.Matrix as M
-- import Data.List hiding (tails)
import Data.Maybe


import Language.KansasLava.Signal
import Language.KansasLava.Utils
import Language.KansasLava.K
import Language.KansasLava.Wire
import Language.KansasLava.Seq as Seq
--import Language.KansasLava.Logic
import Language.KansasLava.Type
import Language.KansasLava.Entity
import Control.Monad

{-
---projK :: K w -> K (WIDTH w) -> K (Matrix (WIDTH w) Bool)
--projK = 

mapToBoolMatrix :: forall sig w . (SIGNAL sig, Size (WIDTH w), RepWire w) => sig w -> sig (Matrix (WIDTH w) Bool)
mapToBoolMatrix = liftS1 $ \ (K a d) -> K
	(( optX (liftM fromWireRep ((unX a) :: Maybe w
		    ) :: Maybe (Matrix (WIDTH w) Bool))
	 ) :: X (Matrix (WIDTH w) Bool))	
	(entity1 (Name "Lava" "toBoolMatrix") d)
	
mapFromBoolMatrix :: forall sig w . (SIGNAL sig, Size (WIDTH w), RepWire w) => sig (Matrix (WIDTH w) Bool) -> sig w
mapFromBoolMatrix = liftS1 $ \ (K a d) -> K
	(case unX (a :: X (Matrix (WIDTH w) Bool)) :: Maybe (Matrix (WIDTH w) Bool) of
	     Nothing -> optX (Nothing :: Maybe w)
	     Just r0 -> optX (toWireRep r0 :: Maybe w)
	)
	(entity1 (Name "Lava" "fromBoolMatrix") d)
-}	

{-
((optX $ undefined) :: X (Matrix (WIDTH w) Bool))
		      undefined
-}

--  (K s d) -> 
{-
toBoolMatrix :: forall sig w . (SIGNAL sig, Size (WIDTH w), Wire w) => sig w -> Matrix (WIDTH w) (sig Bool)
toBoolMatrix s = forAll $ \ix -> liftS1 (f ix) s
  where
	f :: WIDTH w -> K w -> K Bool
	f w k = k .!. (pureS w)
-}

-- toBoolMatrix :: (SIGNAL sig, Wire w) => sig (Matrix ix Bool) -> Matrix (SZ w) (sig Bool)
-- :: ix -> K (Matrix ix Bool) -> K Bool

{-
class BitRep c where
  toBoolMatrix   :: (Size ix, Enum ix) => Signal (c ix) -> Matrix ix (Signal Bool)
  fromBoolMatrix :: (OpType (Matrix ix Bool), Size ix, Enum ix) => Matrix ix (Signal Bool) -> Signal (c ix)

--
-- To consider, these ops
--	:: Signal (Matrix ix Bool) -> Matrix ix (Signal Bool)
--	:: Matrix ix (Signal Bool) -> Signal (Matrix ix Bool)

-- The Seq versions
pushin :: forall i t . Size i => Seq (Matrix i t) -> Matrix i (Seq t)
pushin  m = M.fromList (fmapConst m)
  where fmapConst :: Seq (Matrix i t) -> [Seq t]
        fmapConst (Nothing :~ as) = lzw (:~) nothings (fmapConst as)
        fmapConst (Just a :~ as) = lzw (:~) (map Just (M.toList a)) (fmapConst as)
        fmapConst (Constant Nothing) = map Constant nothings
        fmapConst (Constant (Just a)) = map (Constant . Just) (M.toList a)
        nothings = replicate (size (undefined :: i)) Nothing
        -- lzw is a 'lazy' zip with that uses an irrefutable pattern to work.
        lzw f (a:as) ~(b:bs) = f a b:(lzw f as bs)
        lzw _ [] _ = []


pullout :: Size i => Matrix i (Seq t) -> Seq (Matrix i t)
pullout m = combine (M.toList m)
  where combine seqs
          | constant && valid = Constant (Just (M.fromList values))
          | constant && not valid = Constant Nothing
          | not constant && valid = Just (M.fromList values) :~ combine tails
          | not constant && not valid = Nothing :~ combine tails
          | otherwise = error "pullout.combine: unmatched pattern"
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
                 -- FIXME: Need to think about how to handle dynamics better
		 (zip3 inVars (repeat aTy) (map (\ ~(Signal _ w) -> w) (M.toList m))) []
   where inVars = [Var ("i" ++ show i) | i <- indices m ]
         mSeq = fmap (\ ~(Signal a _) -> a) m
         oTy = U $ (size (undefined :: ix)) * (baseTypeLength aTy)
         aTy = tyRep (error "matrixSignalSignalMatrix" :: a)

-- edk
signalMatrixToMatrixSignal :: forall ix a. (OpType a, Size ix) => Signal (Matrix ix a) -> Matrix ix (Signal a)
signalMatrixToMatrixSignal (Signal shallow deep) = res
   where iTy = U (numElements * width)
         aTy = tyRep (error "matrixSignalSignalMatrix" :: a)
         slice driver (l, h) = E $ Entity (Name "Lava" "slice")
             [(Var "o0",aTy)] [(Var "i0",iTy,driver),(Var "low",U 32,l), (Var "high",U 32, h)] []
         numElements = size (undefined :: ix)
         width = baseTypeLength aTy
         ixs = [(Lit$  fromIntegral $ (i-1)*width,Lit $ fromIntegral $ i*width - 1) | i <- [1..numElements]]
         shallow' = pushin shallow
         deep' :: M.Matrix ix E
         deep' = M.fromList (map (slice deep) ixs)
         res = M.zipWith (\s d -> Signal s (Port (Var "o0") d)) shallow' deep'



signalMatrixBoolToSignalUnsigned :: forall ix. (Enum ix, Size ix) => Signal (Matrix ix Bool) -> Signal (Unsigned ix)
signalMatrixBoolToSignalUnsigned  x =
	o0 $ entity1 (Name "Matrix" "signalMatrixBoolToSignalUnsigned") inputs [Var "o0"] fn x
	where inputs   = map Var ["i0"]
	      fn = U.fromMatrix

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


-}

