{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Data.Sized.Matrix where

import Data.Array as A hiding (indices,(!), ixmap, assocs)
import qualified Data.Array as A
import Prelude as P
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.List as L

import Data.Sized.Ix -- for testing

-- | A 'Matrix' is an array with the sized determined uniquely by the 
-- /type/ of the index type, 'ix'. 
data Matrix ix a = Matrix (Array ix a)

-- | '!' looks up an element in the matrix.
(!) :: (Size n) => Matrix n a -> n -> a
(!) (Matrix xs) n = xs A.! n

instance (Size i) => Functor (Matrix i) where
	fmap f (Matrix xs) = Matrix (fmap f xs)

-- | 'toList' turns a matrix into an always finite list.
toList :: (Size i) => Matrix i a -> [a]
toList (Matrix a) = elems a

-- | 'fromList' turns a finite list into a matrix. You often need to give the type of the result.
fromList :: (Size i) => [a] -> Matrix i a
fromList xs = check minBound maxBound
    where 
	check low high | L.length (range (low,high)) == L.length xs
		       = Matrix $ listArray (low,high) xs
		       | otherwise
		       = error $ "bad length of fromList for Matrix, "
			      ++ "expecting " ++ show (L.length (range (low,high))) ++ " elements"
			      ++ ", found " ++ show (L.length xs) ++ " elements."

-- | 'matrix' turns a finite list into a matrix. You often need to give the type of the result.
matrix :: (Size i) => [a] -> Matrix i a
matrix = fromList

-- | Unlike the array version of 'indices', this does not need the 'Matrix'
-- argument, because the types determine the contents.
indices :: (Size i) => [i]
indices = range (minBound,maxBound)

-- | 'indices'' is a version of 'indices' that takes a type, for forcing the result type using the Matrix type.
indices' :: (Size i) => Matrix i a -> [i]
indices' _ = indices


-- | what is the length of a matrix?
length :: (Size i) => Matrix i a -> Int
length = size . sizeOf

-- | 'assocs' extracts the index/value pairs.
assocs :: (Size i) => Matrix i a -> [(i,a)]
assocs (Matrix a) = A.assocs a

-- | 'sizeOf' is for use only to force typing issues, and is undefined.
sizeOf :: Matrix i a -> i
sizeOf _ = undefined

-- | 'coord' returns a matrix filled with indexes.
coord :: (Size i) => Matrix i i
coord = fromList Data.Sized.Matrix.indices

-- | Same as for lists.
zipWith :: (Size i) => (a -> b -> c) -> Matrix i a -> Matrix i b -> Matrix i c
zipWith f a b = forAll $ \ i -> f (a ! i) (b ! i)

-- | 'forEach' takes a matrix, and calls a function for each element, to give a new matrix of the same size.
forEach :: (Size i) => Matrix i a -> (i -> a -> b) -> Matrix i b
forEach a f = Data.Sized.Matrix.zipWith f coord a

-- | 'forAll' creates a matrix out of a mapping from the coordinates.
forAll :: (Size i) => (i -> a) -> Matrix i a
forAll f = fmap f coord

instance (Size i) => Applicative (Matrix i) where
	pure a = fmap (const a) coord	-- possible because we are a fixed size
	a <*> b = forAll $ \ i -> (a ! i) (b ! i)
	
-- | 'mm' is the 2D matrix multiply.
mm :: (Size m, Size n, Size m', Size n', n ~ m', Num a) => Matrix (m,n) a -> Matrix (m',n') a -> Matrix (m,n') a
mm a b = forAll $ \ (i,j) -> sum [ a ! (i,r) * b ! (r,j) | r <- indices ]
 
-- | 'transpose' a 2D matrix.
transpose :: (Size x, Size y) => Matrix (x,y) a -> Matrix (y,x) a
transpose = ixmap $ \ (x,y) -> (y,x)

-- | return the identity for a specific matrix size.
identity :: (Size x, Num a) => Matrix (x,x) a
identity = (\ (x,y) -> if x == y then 1 else 0) <$> coord

-- | stack two matrixes 'above' each other.
above :: (Size m, Size n, Size m', Size m'') 
      => Matrix (m,n) a -> Matrix (m',n) a -> Matrix (m'',n) a
above m1 m2 = fromList (toList m1 ++ toList m2)

-- | stack two matrixes 'beside' each other.
beside
  :: (Size m,
      Size n,
      Size n',
      Size n'') =>
     Matrix (m, n) a -> Matrix (m, n') a -> Matrix (m, n'') a
beside m1 m2 = transpose (transpose m1 `above` transpose m2)

-- | look at a matrix through a lens to another matrix.
ixmap :: (Size i, Size j) => (i -> j) -> Matrix j a -> Matrix i a
ixmap f m = (\ i -> m ! f i) <$> coord

-- | grab /part/ of a matrix.
crop :: (Index i ~ Index ix, Size i, Size ix) => ix -> Matrix ix a -> Matrix i a
crop corner = ixmap (\ i -> (addIndex corner (toIndex i)))

-- | slice a 2D matrix into rows.
rows :: (Bounded n, Size n, Bounded m, Size m) => Matrix (m,n) a -> Matrix m (Matrix n a)
rows a = (\ m -> matrix [ a ! (m,n) | n <- indices ]) <$> coord

-- | slice a 2D matrix into columns.
columns :: (Bounded n, Size n, Bounded m, Size m) => Matrix (m,n) a -> Matrix n (Matrix m a)
columns = rows . transpose

-- | join a matrix of matrixes into a single matrix.
joinrows :: (Bounded n, Size n, Bounded m, Size m) => Matrix m (Matrix n a) -> Matrix (m,n) a
joinrows a = (\ (m,n) -> (a ! m) ! n) <$> coord

-- | generate a 2D single row from a 1D matrix.
aRow :: (Size m, Bounded m) => Matrix m a -> Matrix (X1, m) a
aRow = ixmap snd

-- | generate a 2D single column from a 1D matrix.
aColumn :: (Size m, Bounded m) => Matrix m a -> Matrix (m, X1) a
aColumn = ixmap fst

-- | very general; required that m and n have the same number of elements, rebundle please.
squash :: (Size n, Size m) => Matrix m a -> Matrix n a
squash = fromList . toList


instance (Size ix) => T.Traversable (Matrix ix) where
  traverse f a = matrix <$> (T.traverse f $ toList a)
 
instance (Size ix) => F.Foldable (Matrix ix) where
  foldMap f m = F.foldMap f (toList m)

-- | 'showMatrix' displays a 2D matrix, and is the worker for 'show'.
-- @
--  GHCi> matrix [1..42] :: Matrix (X7,X6) Int
--  [  1,  2,  3,  4,  5,  6,
--     7,  8,  9, 10, 11, 12,
--    13, 14, 15, 16, 17, 18,
--    19, 20, 21, 22, 23, 24,
--    25, 26, 27, 28, 29, 30,
--    31, 32, 33, 34, 35, 36,
--    37, 38, 39, 40, 41, 42 ]
-- @

showMatrix :: (Size n, Size m) => Matrix (m, n) String -> String
showMatrix m = joinLines $ map showRow m_rows
	where
		m'	    = forEach m $ \ (x,y) a -> (x == maxBound && y == maxBound,a)
		joinLines   = unlines . L.zipWith (++) ("[":repeat " ") 
		showRow	r   = concat (toList $ Data.Sized.Matrix.zipWith showEle r m_cols_size)
		showEle (f,str) s = take (s - L.length str) (cycle " ") ++ " " ++ str ++ (if f then " ]" else ",")
		m_cols      = columns m
		m_rows      = toList $ rows m'
		m_cols_size = fmap (maximum . map L.length . toList) m_cols


instance (Show a, Size ix,Size (Row ix), Size (Column ix)) => Show (Matrix ix a) where
	show = showMatrix . ixmap seeIn2D . fmap show

mm2 :: (Size m, Size n, Size m', Size n', n ~ m', Num a) => Matrix (m,n) a -> Matrix (m',n') a -> Matrix (m,n') a
mm2 a b = forAll $ \ (i,j) -> sum [ a ! (i,r) * b ! (r,j) | r <- indices ]
	