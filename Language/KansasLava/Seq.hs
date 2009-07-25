module Language.KansasLava.Seq 
        ( Seq(..)
        , Language.KansasLava.Seq.toList
        , fromList 
        ) where

import Data.Traversable
import Data.Foldable 
import Control.Applicative
import Prelude hiding (foldr)

infixr 5 :~

data Seq a = a :~ Seq a
        deriving Show

instance Applicative Seq where
        pure a = a :~ pure a
        (h1 :~ t1) <*> (h2 :~ t2) = (h1 h2) :~ (t1 <*> t2)

instance Traversable Seq where
  sequenceA = fmap fromList . sequenceA . Data.Foldable.toList

instance Foldable Seq where
  foldr f z (x :~ xs) = f x (foldr f z xs)

instance Functor Seq where
   fmap f (a :~ as) = f a :~ fmap f as

fromList :: [a] -> Seq a
fromList (x : xs) = x :~ fromList xs
fromList []       = error "Seq.fromList"

toList :: Seq a -> [a]
toList = Data.Foldable.toList