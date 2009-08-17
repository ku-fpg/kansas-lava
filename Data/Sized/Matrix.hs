{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Data.Sized.Matrix where

import Data.Array as A hiding (indices,(!), ixmap)
import qualified Data.Array as A
import Prelude as P
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid



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

matrix :: (Bounded i, Ix i) => [a] -> Matrix i a
matrix = fromList


indices :: (Bounded i, Ix i) => [i]
indices = range (minBound,maxBound)

-- Version of 'indices' that takes a type, for forcing the result type using the Matrix type.
indices' :: (Bounded i, Ix i) => Matrix i a -> [i]
indices' _ = indices

assocs :: (Ix i) => Matrix i a -> [(i,a)]
assocs (Matrix a) = A.assocs a


-- for use only to force typing
sizeOf :: Matrix i a -> i
sizeOf _ = undefined

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

-- slice into rows
rows :: (Bounded n, Ix n, Bounded m, Ix m) => Matrix (m,n) a -> Matrix m (Matrix n a)
rows a = (\ m -> matrix [ a ! (m,n) | n <- indices ]) <$> coord

columns :: (Bounded n, Ix n, Bounded m, Ix m) => Matrix (m,n) a -> Matrix n (Matrix m a)
columns = rows . transpose

joinrows :: (Bounded n, Ix n, Bounded m, Ix m) => Matrix m (Matrix n a) -> Matrix (m,n) a
joinrows a = (\ (m,n) -> (a ! m) ! n) <$> coord

aRow :: (Ix m, Bounded m) => Matrix m a -> Matrix (X1,m) a
aRow = ixmap snd

aColumn :: (Ix m, Bounded m) => Matrix m a -> Matrix (m,X1) a
aColumn = ixmap fst

type Sized x = forall a . [a] -> Matrix x a
type SizedX x = forall a . Matrix x a -> Matrix x a

-- very general; required that m and n have the same elements.
squash :: (Bounded n, Ix n, Ix m) => Matrix m a -> Matrix n a
squash = fromList . toList


instance (Ix ix, Bounded ix) => T.Traversable (Matrix ix) where
  traverse f a = matrix <$> (T.traverse f $ toList a)
 
instance (Ix ix) => F.Foldable (Matrix ix) where
  foldMap f m = F.foldMap f (toList m)

{-
class SIZED c where {}
instance SIZED [] where {}
instance SIZED (Matrix ix) where {}
-}

{- idiom. When you can not give the explicit sized type (because for example in a where
   clause, and there is a scoped 'a'), you can used

   a = (matrix :: Sized X8) 
 	  [ 1 .. 8 ]
 -}
