{-# LANGUAGE DeriveDataTypeable #-}
-- | 'Stream's are infinite sequences of values. They correspond to lazy lists without '[]'
module Language.KansasLava.Stream where

import Data.Traversable
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Prelude hiding (zipWith,zipWith3)
import Data.Monoid

-- | The stream 'cons'
infixr 5 :~

-- | A sequence of values, strict in the head, with no tail. ( TODO: make head strict )
data Stream a =
  a :~ Stream a -- ^ The stream cons

--    deriving (Typeable)

-- | The show instance will only print the first 20 elements of the 'Stream'.
instance Show a => Show (Stream a) where
   show = showStream 20

-- | 'showStream' shows a given length of the stream.
showStream :: Show a => Int -> Stream a -> String
showStream i vs           = unwords [ show x ++ " :~ "
                                | x <- take i $ toList vs
                                ] ++ "..."

instance Functor Stream where
   fmap f (a :~ as) = f a :~ fmap f as

instance Applicative Stream where
        pure a = a :~ pure a
        (h1 :~ t1) <*> (h2 :~ t2)       = h1  h2 :~ (t1 <*> t2)

instance F.Foldable Stream where
  foldMap f (a :~ as) = f a `mappend` F.foldMap f as

instance Traversable Stream where
  traverse f (a :~ as) = (:~) <$> f a <*> traverse f as


-- | Retrieve the first element of a 'Stream'.
head :: Stream a -> a
head (a :~ _) = a

-- | Retrieve the remainder of a 'Stream'.
tail :: Stream a -> Stream a
tail (_ :~ as) = as

-- | Combine two streams pointwise with the input function @f@.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x :~ xs) (y :~ ys) = f x y :~ zipWith f xs ys

-- | Combine three streams pointwise with the input function @f@.
zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f (x :~ xs) (y :~ ys) (z :~ zs) = f x y z :~ zipWith3 f xs ys zs

-- | Convert a list to a 'Stream'.
fromList :: [a] -> Stream a
fromList = foldr (:~) (error "Stream.fromList")

-- | Convert a stream to an infinite list.
toList :: Stream a -> [a]
toList (x :~ xs) = x : toList xs



