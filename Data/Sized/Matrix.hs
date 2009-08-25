{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Data.Sized.Matrix where

import Data.Array as A hiding (indices,(!), ixmap)
import qualified Data.Array as A
import Prelude as P
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.List as L

import Data.Sized.Ix -- for testing

-- | A 'Matrix' is an array with the sized determined uniquely by the 
-- @type@ of the index type.

data Matrix i a = Matrix (Array i a)

{-
instance (Show a, Ix i) => Show (Matrix i a) where
	show = show . toList
-}
(!) :: (Size n) => Matrix n a -> n -> a
(!) (Matrix xs) n = xs A.! n

instance (Size i) => Functor (Matrix i) where
	fmap f (Matrix xs) = Matrix (fmap f xs)

toList :: (Ix i) => Matrix i a -> [a]
toList (Matrix a) = elems a

fromList :: (Size i) => [a] -> Matrix i a
fromList xs = check minBound maxBound
    where 
	check low high | L.length (range (low,high)) == L.length xs
		       = Matrix $ listArray (low,high) xs
		       | otherwise
		       = error $ "bad length of fromList for Matrix, "
			      ++ "expecting " ++ show (L.length (range (low,high))) ++ " elements"
			      ++ ", found " ++ show (L.length xs) ++ " elements."

matrix :: (Size i) => [a] -> Matrix i a
matrix = fromList

-- | Unlike the array version of 'indices', this does not need the 'Matrix'
-- argument, because the types determine the contents.

indices :: (Size i) => [i]
indices = range (minBound,maxBound)

-- Version of 'indices' that takes a type, for forcing the result type using the Matrix type.
indices' :: (Size i) => Matrix i a -> [i]
indices' _ = indices

length :: (Size i) => Matrix i a -> Int
length = L.length . indices'

assocs :: (Size i) => Matrix i a -> [(i,a)]
assocs (Matrix a) = A.assocs a

-- for use only to force typing
sizeOf :: Matrix i a -> i
sizeOf _ = undefined

-- I find this useful
coord :: (Size i) => Matrix i i
coord = fromList Data.Sized.Matrix.indices

zipWith :: (Size i) => (a -> b -> c) -> Matrix i a -> Matrix i b -> Matrix i c
zipWith f (Matrix a) (Matrix b) = fromList (P.zipWith f (elems a) (elems b))

forEach :: (Size i) => Matrix i a -> (i -> a -> b) -> Matrix i b
forEach a f = Data.Sized.Matrix.zipWith f coord a

instance (Size i) => Applicative (Matrix i) where
	pure a = fmap (const a) coord
	(Matrix a) <*> (Matrix b) = fromList (P.zipWith (\ a b -> a b) (elems a) (elems b))

-- 2d matrix operations

mm :: (Size m, Size n, Size m', Size n', n ~ m', Num a) => Matrix (m,n) a -> Matrix (m',n') a -> Matrix (m,n') a
mm (Matrix x) (Matrix y) =  Matrix $ array resultBounds
                         [((i,j), sum [x A.! (i,k) * y A.! (k,j) | k <- range (lj,uj)])
                                       | i <- range (li,ui),
                                         j <- range (lj',uj') ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: incompatible bounds"
 
transpose :: (Size x, Size y) => Matrix (x,y) a -> Matrix (y,x) a
transpose = ixmap $ \ (x,y) -> (y,x)

-- neat defintion!
identity :: (Size x, Num a) => Matrix (x,x) a
identity = (\ (x,y) -> if x == y then 1 else 0) <$> coord
	
above :: (Size m, Size n, Size m', Size m'') 
      => Matrix (m,n) a -> Matrix (m',n) a -> Matrix (m'',n) a
above m1 m2 = fromList (toList m1 ++ toList m2)

beside
  :: (Size m,
      Size n,
      Size n',
      Size n'') =>
     Matrix (m, n) a -> Matrix (m, n') a -> Matrix (m, n'') a
beside m1 m2 = transpose (transpose m1 `above` transpose m2)

ixmap :: (Size i, Size j) => (i -> j) -> Matrix j a -> Matrix i a
ixmap f m = (\ i -> m ! f i) <$> coord

-- grab *part* of a matrix.
crop :: (Index i ~ Index ix, Size i, Size ix) => ix -> Matrix ix a -> Matrix i a
crop corner = ixmap (\ i -> (addIndex corner (toIndex i)))

-- slice into rows
rows :: (Bounded n, Size n, Bounded m, Size m) => Matrix (m,n) a -> Matrix m (Matrix n a)
rows a = (\ m -> matrix [ a ! (m,n) | n <- indices ]) <$> coord

columns :: (Bounded n, Size n, Bounded m, Size m) => Matrix (m,n) a -> Matrix n (Matrix m a)
columns = rows . transpose

joinrows :: (Bounded n, Size n, Bounded m, Size m) => Matrix m (Matrix n a) -> Matrix (m,n) a
joinrows a = (\ (m,n) -> (a ! m) ! n) <$> coord

aRow :: (Size m, Bounded m) => Matrix m a -> Matrix (X1, m) a
aRow = ixmap snd

aColumn :: (Size m, Bounded m) => Matrix m a -> Matrix (m, X1) a
aColumn = ixmap fst

-- very general; required that m and n have the same number of elements.
squash :: (Size n, Size m) => Matrix m a -> Matrix n a
squash = fromList . toList

instance (Size ix) => T.Traversable (Matrix ix) where
  traverse f a = matrix <$> (T.traverse f $ toList a)
 
instance (Size ix) => F.Foldable (Matrix ix) where
  foldMap f m = F.foldMap f (toList m)

showMatrix :: (Show a, Size n, Size m) => Matrix (m, n) a -> String
showMatrix m = joinLines $ map showRow m_rows
	where
		m'	    = forEach m $ \ (x,y) a -> (x == maxBound && y == maxBound,a)
		joinLines   = unlines . L.zipWith (++) ("[":repeat " ") 
		showRow	r   = concat (toList $ Data.Sized.Matrix.zipWith showEle r m_cols_size)
		showEle (f,e) s = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then " ]" else ",")
			where str = show e
		m_cols      = columns m
		m_rows      = toList $ rows m'
		m_cols_size = fmap (maximum . map L.length . map show . toList) m_cols


instance (Show a, Size ix,Size (D1 ix), Size (D2 ix)) => Show (Matrix ix a) where
	show = showMatrix . ixmap seeIn2D

