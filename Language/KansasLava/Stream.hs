{-# LANGUAGE DeriveDataTypeable #-}
module Language.KansasLava.Stream where

import Data.Traversable
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Prelude hiding (zipWith,zipWith3)
import Data.Monoid
import Data.Dynamic

infixr 5 :~

-- | A sequence of values, strict in the head, with no tail.
-- ( TODO: make head strict )
data Stream a = a :~ Stream a
--    deriving (Typeable)	

-- | show the first 20 elements of the 'Stream'.
instance Show a => Show (Stream a) where
   show = showStream 20

-- | 'showStream' shows a given length of the stream.
showStream :: Show a => Int -> Stream a -> String
showStream i vs           = unwords [ show x ++ " :~ "
                                | x <- take i $ toList vs
                                ] ++ "..."


instance Applicative Stream where
        pure a = a :~ pure a
        (h1 :~ t1) <*> (h2 :~ t2)       = (h1 $ h2) :~ (t1 <*> t2)

instance Functor Stream where
   fmap f (a :~ as) = f a :~ fmap f as

head :: Stream a -> a
head (a :~ _) = a

tail :: Stream a -> Stream a
tail (_ :~ as) = as

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x :~ xs) (y :~ ys) = f x y :~ zipWith f xs ys

zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f (x :~ xs) (y :~ ys) (z :~ zs) = f x y z :~ zipWith3 f xs ys zs

fromList :: [a] -> Stream a
fromList (x : xs) = x :~ fromList xs
fromList []       = error "Stream.fromList"

toList :: Stream a -> [a]
toList (x :~ xs) = x : toList xs

instance F.Foldable Stream where
  foldMap f (a :~ as) = f a `mappend` F.foldMap f as
  foldMap _ _ = error "Foldable.foldMap(Stream)"

instance Traversable Stream where
  traverse f (a :~ as) = (:~) <$> f a <*> traverse f as
--  traverse f (Constant a) = Constant <$> f a

