{-# LANGUAGE TypeFamilies #-}
module Data.Sized.Matrix where

import Data.Array as A hiding (indices,(!), ixmap)
import qualified Data.Array as A
import Prelude as P
import Control.Applicative


import Data.Sized.Ix -- for testing

-- | A 'Matrix' is an array with the sized determined uniquely by the 
-- @type@ of the index type.

data Matrix i a = Matrix (Array i a)

instance (Show a, Ix i) => Show (Matrix i a) where
	show = show . toList

(!) :: (Ix n) => Matrix n a -> n -> a
(!) (Matrix xs) n = xs A.! n

instance (Ix i) => Functor (Matrix i) where
	fmap f (Matrix xs) = Matrix (fmap f xs)

toList :: (Ix i) => Matrix i a -> [a]
toList (Matrix a) = elems a

fromList :: (Bounded i, Ix i) => [a] -> Matrix i a
fromList xs = check minBound maxBound
    where 
	check low high | length (range (low,high)) == length xs
		       = Matrix $ listArray (low,high) xs
		       | otherwise
		       = error $ "bad length of fromList for Matrix, "
			      ++ "expecting " ++ show (length (range (low,high))) ++ " elements"
			      ++ ", found " ++ show (length xs) ++ " elements."

-- | Unlike the array version of 'indices', this does not need the 'Matrix'
-- argument, because the types determine the contents.

indices :: (Bounded i, Ix i) => [i]
indices = range (minBound,maxBound)

-- Version of 'indices' that takes a type, for forcing the result type using the Matrix type.
indices' :: (Bounded i, Ix i) => Matrix i a -> [i]
indices' _ = indices

assocs :: (Ix i) => Matrix i a -> [(i,a)]
assocs (Matrix a) = A.assocs a

-- I find this useful
coord :: (Bounded i, Ix i) => Matrix i i
coord = fromList Data.Sized.Matrix.indices

zipWith :: (Bounded i, Ix i) => (a -> b -> c) -> Matrix i a -> Matrix i b -> Matrix i c
zipWith f (Matrix a) (Matrix b) = fromList (P.zipWith f (elems a) (elems b))

forAll :: (Bounded i, Ix i) => Matrix i a -> (i -> a -> b) -> Matrix i b
forAll a f = Data.Sized.Matrix.zipWith f coord a

instance (Ix i, Bounded i) => Applicative (Matrix i) where
	pure a = fmap (const a) coord
	(Matrix a) <*> (Matrix b) = fromList (P.zipWith (\ a b -> a b) (elems a) (elems b))

-- 2d matrix operations

mm :: (Num a, Ix m, Ix n, Ix m', Ix n', n ~ m') => Matrix (m,n) a -> Matrix (m',n') a -> Matrix (m,n') a
mm (Matrix x) (Matrix y) =  Matrix $ array resultBounds
                         [((i,j), sum [x A.! (i,k) * y A.! (k,j) | k <- range (lj,uj)])
                                       | i <- range (li,ui),
                                         j <- range (lj',uj') ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: incompatible bounds"
 
transpose :: (Ix x, Ix y, Bounded x, Bounded y) => Matrix (x,y) a -> Matrix (y,x) a
transpose = ixmap $ \ (x,y) -> (y,x)

-- neat defintion!
identity :: (Ix x, Bounded x, Num a) => Matrix (x,x) a
identity = (\ (x,y) -> if x == y then 1 else 0) <$> coord

rows :: (Bounded m, Ix m) => Matrix (m,n) a -> [m]
rows _ = indices
	
columns :: (Bounded n, Ix n) => Matrix (m,n) a -> [n]
columns _ = indices
	
above :: (Ix m, Ix n, Ix m', Ix m'', Bounded m, Bounded n, Bounded m', Bounded m'') 
      => Matrix (m,n) a -> Matrix (m',n) a -> Matrix (m'',n) a
above m1 m2 = fromList (toList m1 ++ toList m2)

beside
  :: (Ix m,
      Ix n,
      Ix n',
      Ix n'',
      Bounded m,
      Bounded n,
      Bounded n',
      Bounded n'') =>
     Matrix (m, n) a -> Matrix (m, n') a -> Matrix (m, n'') a
beside m1 m2 = transpose (transpose m1 `above` transpose m2)

ixmap :: (Bounded i, Ix i, Ix j) => (i -> j) -> Matrix j a -> Matrix i a
ixmap f m = (\ i -> m ! f i) <$> coord

crop1D :: (Bounded i, Num i, Enum i, Ix i, Enum i', Bounded i', Ix i') => i -> Matrix i a -> Matrix i' a
crop1D corner = ixmap (\ i -> (toEnum (fromEnum i) + corner))

-- TODO: clean up the type names here.
crop2D
  :: (Enum t,
      Enum a,
      Num a,
      Enum t1,
      Enum a1,
      Num a1,
      Bounded t,
      Bounded t1,
      Ix t,
      Ix t1,
      Ix a,
      Ix a1) =>
     (a, a1) -> Matrix (a, a1) a2 -> Matrix (t, t1) a2
crop2D (cM,cN) = ixmap (\ (m,n) -> (toEnum (fromEnum m) + cM,toEnum (fromEnum n) + cN))
