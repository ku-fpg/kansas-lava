{-# LANGUAGE DeriveDataTypeable #-}
module Language.KansasLava.Stream where

import Data.Traversable
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Prelude hiding (zipWith,zipWith3, repeat)
import Data.Monoid
import Data.Dynamic

infixr 5 `Cons`

-- A clocked sequence of values, which can be undefined (Nothing),  or have a specific value.
data Stream a = Cons !a (Stream a)
    deriving (Typeable)

instance Show a => Show (Stream a) where
   show = showStream 20

showStream :: Show a => Int -> Stream a -> String
showStream i vs           = unwords [ show x ++ " `Cons` "
                                | x <- take i $ toList vs
                                ] ++ "..."

instance Applicative Stream where
        pure a = a `Cons` pure a
        (h1 `Cons` t1) <*> (h2 `Cons` t2) = (h1 $ h2) `Cons` (t1 <*> t2)

instance Functor Stream where
   fmap f (a `Cons` as) = f a `Cons` fmap f as


head :: Stream a -> a
head (a `Cons` _) = a

tail :: Stream a -> Stream a
tail (_ `Cons` as) = as

repeat :: a -> Stream a
repeat a = a `Cons` repeat a

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x `Cons` xs) (y `Cons` ys) = f x y `Cons` zipWith f xs ys

zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f (x `Cons` xs) (y `Cons` ys) (z `Cons` zs) = f x y z `Cons` zipWith3 f xs ys zs

fromList :: [a] -> Stream a
fromList (x : xs) = x `Cons` fromList xs
fromList []       = error "Stream.fromList"

toList :: Stream a -> [a]
toList (x `Cons` xs) = x : toList xs

instance F.Foldable Stream where
  foldMap f (a `Cons` as) = f a `mappend` F.foldMap f as

instance Traversable Stream where
  traverse f (a `Cons` as) = Cons <$> f a <*> traverse f as

