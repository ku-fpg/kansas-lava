{-# LANGUAGE DeriveDataTypeable #-}
-- | This implementation of the Stream data type. It's defined similarly to
-- other implementation of infinite streams found on hackage, except the
-- elements of the stream are strict to prevent some space leaks.
module Language.KansasLava.Stream where

import Data.Traversable
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Prelude hiding (zipWith,zipWith3, repeat)
import Data.Monoid
import Data.Dynamic

-- | Set the precedence of infix `Cons`.
infixr 5 `Cons`

-- | A stream is an infinite sequence of values.
data Stream a = Cons !a (Stream a) -- ^ Cons takes a head and a tail.
    deriving (Typeable)

instance Show a => Show (Stream a) where
   show = showStream 20

-- | Print out a Stream.
showStream :: Show a => Int -> Stream a -> String
showStream i vs           = unwords [ show x ++ " `Cons` "
                                | x <- take i $ toList vs
                                ] ++ "..."

instance Applicative Stream where
        pure a = a `Cons` pure a
        (h1 `Cons` t1) <*> (h2 `Cons` t2) = h1 h2 `Cons` (t1 <*> t2)

instance Functor Stream where
   fmap f (a `Cons` as) = f a `Cons` fmap f as

-- | Get the first element of the stream.
head :: Stream a -> a
head (a `Cons` _) = a

-- | Get the remainder of the stream.
tail :: Stream a -> Stream a
tail (_ `Cons` as) = as

-- | Lift a value to be a constant stream.
repeat :: a -> Stream a
repeat a = a `Cons` repeat a

-- | Zip two streams together.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x `Cons` xs) (y `Cons` ys) = f x y `Cons` zipWith f xs ys

-- | Zip three streams together.
zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f (x `Cons` xs) (y `Cons` ys) (z `Cons` zs) = f x y z `Cons` zipWith3 f xs ys zs

-- | Convert a list to a stream. If the list is finite, then the last element of
-- the stream will be an error.
fromList :: [a] -> Stream a
fromList (x : xs) = x `Cons` fromList xs
fromList []       = error "Stream.fromList"

-- | Convert a Stream to a lazy list.
toList :: Stream a -> [a]
toList (x `Cons` xs) = x : toList xs

instance F.Foldable Stream where
  foldMap f (a `Cons` as) = f a `mappend` F.foldMap f as

instance Traversable Stream where
  traverse f (a `Cons` as) = Cons <$> f a <*> traverse f as

