{-# LANGUAGE ScopedTypeVariables #-}

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

