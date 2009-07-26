module Language.KansasLava.Seq 
        ( Seq(..)
        , Language.KansasLava.Seq.toList
        , fromList 
        ) where

import Data.Traversable
-- import Data.Foldable 
import Control.Applicative
import Control.Monad
import Prelude hiding (zipWith,zipWith3)

infixr 5 :~

-- A clocked sequence of values, which can be undefined (Nothing),  or have a specific value.
data Seq a = Maybe a :~ Seq a
        deriving Show

instance Applicative Seq where
        pure a = Just a :~ pure a
        (h1 :~ t1) <*> (h2 :~ t2) = (h1 `ap` h2) :~ (t1 <*> t2)

instance Functor Seq where
   fmap f (a :~ as) = liftM f a :~ fmap f as

zipWith' :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith' f xs ys = pure f <*> xs <*> ys

fromList :: [Maybe a] -> Seq a
fromList (x : xs) = x :~ fromList xs
fromList []       = error "Seq.fromList"

toList :: Seq a -> [Maybe a]
toList (x :~ xs) = x : toList xs
