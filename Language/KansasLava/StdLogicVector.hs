{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}

module Language.KansasLava.StdLogicVector where

import Data.Bits

import Data.Sized.Matrix as M

data StdLogicVector a = StdLogicVector (Matrix a Bool)
	deriving (Eq,Ord)

instance (Size a) => Show (StdLogicVector a) where
	show (StdLogicVector m) = map (\ b -> if b then '1' else '0') $ reverse $ M.toList m

instance (Size ix) => Num (StdLogicVector ix) where
	(+) = error "(+) is undefined for StdLogicVector"
	(-) = error "(-) is undefined for StdLogicVector"
	(*) = error "(*) is undefined for StdLogicVector"
	abs = error "abs is undefined for StdLogicVector"
	signum = error "signum is undefined for StdLogicVector"
	fromInteger n = StdLogicVector $ matrix $ take (size (error "witness" :: ix)) $ map odd $ iterate (`div` 2) n

instance (Integral ix, Size ix) => Bits (StdLogicVector ix) where
	bitSize s = size (error "witness" :: ix)

	complement (StdLogicVector m) = StdLogicVector (fmap not m)
	isSigned _ = False

	(StdLogicVector a) `xor` (StdLogicVector b) = StdLogicVector $ M.zipWith (/=) a b
	(StdLogicVector a) .|. (StdLogicVector b) = StdLogicVector $ M.zipWith (||) a b
	(StdLogicVector a) .&. (StdLogicVector b) = StdLogicVector $ M.zipWith (&&) a b

	shiftL (StdLogicVector m) i = StdLogicVector $ forAll $ off
	  where
		mx = size (error "witness" :: ix)
		off ix | ix' >= mx || ix' < 0 = False
		       | otherwise            = m ! fromIntegral ix'
	            where 
			ix' = fromIntegral ix - i
	shiftR (StdLogicVector m) i = StdLogicVector $ forAll $ off
	  where
		mx = size (error "witness" :: ix)
		off ix | ix' >= mx || ix' < 0 = False
		       | otherwise            = m ! fromIntegral ix'
	            where 
			ix' = fromIntegral ix + i
 	rotate (StdLogicVector m) i = StdLogicVector $ forAll $ off
	  where
		mx = size (error "witness" :: ix)
		off ix | ix' >= mx || ix' < 0 = False
		       | otherwise            = m ! fromIntegral ix'
	            where 
			ix' = fromIntegral ix - i
        testBit (StdLogicVector m) idx = m ! fromIntegral idx


append :: (Size x, Size y, Size (ADD x y)) => StdLogicVector x -> StdLogicVector y -> StdLogicVector (ADD x y)
append (StdLogicVector m1) (StdLogicVector m2) = (StdLogicVector $ M.matrix (M.toList m2 ++ M.toList m1))

{-
splice :: (Integral inp, Integral res, Size high, Size low, Size res, Size inp, res ~ ADD (SUB high low) X1) 
       => high -> low -> StdLogicVector inp -> StdLogicVector res
splice high low inp = StdLogicVector $ forAll $ \ ix -> inp' ! (fromIntegral ix)
  where
	StdLogicVector inp' = shiftR inp (size low)
-}	

splice :: forall inp res . (Integral inp, Integral res, Size res, Size inp)
       => Int -> StdLogicVector inp -> StdLogicVector res
splice low v = coerce $ shiftR v low

-- either take lower bits, or append zeros to upper bits.
coerce :: forall a b . (Size a, Size b) => StdLogicVector a -> StdLogicVector b
coerce (StdLogicVector m) = StdLogicVector 
			  $ M.matrix 
			  $ take (size (error "witness" :: b))
			  $ M.toList m ++ repeat False

