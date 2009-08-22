module Data.Sized.Unsigned 
	( Unsigned
	, toMatrix
	, fromMatrix
	) where
	
import Data.Sized.Matrix as M
import Data.Sized.Ix
import Data.Ix
import Data.List as L
import Control.Applicative
import Data.Bits

newtype Unsigned ix = Unsigned Integer 

toMatrix :: Size ix => Unsigned ix -> Matrix ix Bool
toMatrix s@(Unsigned v) = matrix $ take (bitSize s) $ map odd $ iterate (`div` 2) v

fromMatrix :: Size ix => Matrix ix Bool -> Unsigned ix
fromMatrix m = mkUnsigned $
	  sum [ n	
	      | (n,b) <- zip (iterate (* 2) 1)
			      (M.toList m)
	      , b
	      ]

mkUnsigned :: (Size ix) => Integer -> Unsigned ix
mkUnsigned v = res
   where sz' = 2 ^ fromIntegral bitCount
	 bitCount = bitSize res
	 res = Unsigned (v `mod` sz')

instance (Size ix) => Eq (Unsigned ix) where
	(Unsigned a) == (Unsigned b) = a == b
instance (Size ix) => Ord (Unsigned ix) where
	(Unsigned a) `compare` (Unsigned b) = a `compare` b
instance (Size ix) => Show (Unsigned ix) where
	show (Unsigned a) = show a
instance (Size ix) => Integral (Unsigned ix) where
  	toInteger (Unsigned m) = m
	quotRem (Unsigned a) (Unsigned b) = 
		case quotRem a b of
		   (q,r) -> (mkUnsigned q,mkUnsigned r)
instance (Size ix) => Num (Unsigned ix) where
	(Unsigned a) + (Unsigned b) = mkUnsigned $ a + b
	(Unsigned a) - (Unsigned b) = mkUnsigned $ a - b
	(Unsigned a) * (Unsigned b) = mkUnsigned $ a * b
	abs (Unsigned n) = mkUnsigned $ abs n
	signum (Unsigned n) = mkUnsigned $ signum n
	fromInteger n = mkUnsigned n
instance (Size ix) => Real (Unsigned ix) where
	toRational (Unsigned n) = toRational n
instance (Size ix) => Enum (Unsigned ix) where
	fromEnum (Unsigned n) = fromEnum n
	toEnum n = mkUnsigned (toInteger n)	
instance (Size ix) => Bits (Unsigned ix) where
	bitSize s = f s (maxBound - minBound)
	  where
		f :: (Size a) => Unsigned a -> a -> Int
		f _ ix = size ix
	complement = fromMatrix . fmap not . toMatrix
	isSigned _ = False
	a `xor` b = fromMatrix (M.zipWith (/=) (toMatrix a) (toMatrix b))
	a .|. b = fromMatrix (M.zipWith (||) (toMatrix a) (toMatrix b))
	a .&. b = fromMatrix (M.zipWith (&&) (toMatrix a) (toMatrix b))
		

