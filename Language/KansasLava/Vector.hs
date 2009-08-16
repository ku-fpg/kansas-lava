{-# LANGUAGE TypeFamilies, FlexibleContexts, TemplateHaskell  #-}
module Language.KansasLava.Vector where

import Language.Haskell.TH
import Language.KansasLava.TH
import Language.KansasLava.X
import Data.Array as A hiding (indices)
import Prelude as P
import Control.Applicative

data	Vector i a = Vector (Array i a)

instance (Show a, Ix i) => Show (Vector i a) where
	show = show . toList

(!) :: (Ix n) => Vector n a -> n -> a
(!) (Vector xs) n = xs A.! n

instance (Ix i) => Functor (Vector i) where
	fmap f (Vector xs) = Vector (fmap f xs)

toList :: (Ix i) => Vector i a -> [a]
toList (Vector a) = elems a

fromList :: (Bounded i, Ix i) => [a] -> Vector i a
fromList xs = check minBound maxBound
    where 
	check low high | length (range (low,high)) == length xs
		       = Vector $ listArray (low,high) xs
		       | otherwise
		       = error "bad length of fromList for Vector"


indices :: (Bounded i, Ix i) => [i]
indices = range (minBound,maxBound)

-- I find this useful
coord :: (Bounded i, Ix i) => Vector i i
coord = fromList indices

zipWith :: (Bounded i, Ix i) => (a -> b -> c) -> Vector i a -> Vector i b -> Vector i c
zipWith f (Vector a) (Vector b) = fromList (P.zipWith f (elems a) (elems b))

forAll :: (Bounded i, Ix i) => Vector i a -> (i -> a -> b) -> Vector i b
forAll a f = Language.KansasLava.Vector.zipWith f coord a

instance (Ix i, Bounded i) => Applicative (Vector i) where
	pure a = fmap (const a) coord
	(Vector a) <*> (Vector b) = fromList (P.zipWith (\ a b -> a b) (elems a) (elems b))

