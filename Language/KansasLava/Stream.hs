{-# LANGUAGE DeriveDataTypeable #-}
module Language.KansasLava.Stream where

import Data.Traversable
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Prelude hiding (zipWith,zipWith3)
import Data.Monoid
import Data.Dynamic

infixr 5 `Cons`

-- A clocked sequence of values, which can be undefined (Nothing),  or have a specific value.
data Stream a = Cons a (Stream a)
    deriving (Typeable)

instance Show a => Show (Stream a) where
   show = showStream 20

showStream :: Show a => Int -> Stream a -> String
showStream i vs           = unwords [ show x ++ " `Cons` "
                                | x <- take i $ toList vs
                                ] ++ "..."

-- You can not find Eq over streams.
--instance Eq a => Eq (Stream a) where
--   xs == ys = toList xs == toList ys

{-
showV :: (Show a) => Maybe a -> String
showV Nothing = "?"
showV (Just v) = show v
-}
        -- Just a `Cons` pure a

instance Applicative Stream where
        pure a = a `Cons` pure a
--        (Constant h1) <*> (h2 `Cons` t2)    = (h1 $ h2) `Cons` (Constant h1 <*> t2)
--        (h1 `Cons` t1) <*> (Constant h2)    = (h1 $ h2) `Cons` (t1 <*> Constant h2)
        (h1 `Cons` t1) <*> (h2 `Cons` t2)       = (h1 $ h2) `Cons` (t1 <*> t2)
--        (Constant h1) <*> (Constant h2) = Constant (h1 $ h2)
--undefinedStream :: Stream a
--undefinedStream = Constant Nothing


instance Functor Stream where
   fmap f (a `Cons` as) = f a `Cons` fmap f as
--   fmap f (Constant a) = Constant $ f a

{-
fmapWithFail :: (a -> Maybe b) -> Stream a -> Stream b
fmapWithFail f (Nothing `Cons` as) = Nothing `Cons` fmapWithFail f as
fmapWithFail f (Just a `Cons` as) = f a `Cons` fmapWithFail f as
fmapWithFail f (Constant Nothing) =  Constant Nothing
fmapWithFail f (Constant (Just a)) = Constant (f a)
-}

head :: Stream a -> a
--head (Constant a) = a
head (a `Cons` _) = a

tail :: Stream a -> Stream a
--tail (Constant a) = (Constant a)
tail (_ `Cons` as) = as

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x `Cons` xs) (y `Cons` ys) = f x y `Cons` zipWith f xs ys

zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f (x `Cons` xs) (y `Cons` ys) (z `Cons` zs) = f x y z `Cons` zipWith3 f xs ys zs

fromList :: [a] -> Stream a
fromList (x : xs) = x `Cons` fromList xs
fromList []       = error "Stream.fromList"

toList :: Stream a -> [a]
toList (x `Cons` xs) = x : toList xs
--toList (Constant x) = repeat x

{-
-- To revisit (perhaps this was why we had our mux example pausing?

-- unlike using <*>, etc, this allows the unchosen Stream to be undefined at this time.
streamMux :: Stream Bool -> Stream a -> Stream a -> Stream a
streamMux sB sT sF =
	fromList [ case b of
		    True  -> t
		    False -> f
	         | (b,t,f) <- zip3 (toList sB) (toList sT) (toList sF)
	         ]
-}

instance F.Foldable Stream where
  foldMap f (a `Cons` as) = f a `mappend` F.foldMap f as


instance Traversable Stream where
  traverse f (a `Cons` as) = Cons <$> f a <*> traverse f as
--  traverse f (Constant a) = Constant <$> f a

