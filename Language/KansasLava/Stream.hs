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
import qualified Data.List as List
import Data.Dynamic
import Debug.Trace

-- | Set the precedence of infix `Cons`.
infixr 5 `Cons`

-- | A stream is an infinite sequence of values.
data Stream a = Cons !a (Maybe (Stream a)) 
        -- ^ Cons takes a head and an optional tail.
        -- If the tail is empty, then the last value is repeated.
    deriving (Typeable)

instance Show a => Show (Stream a) where
   show (Cons a opt_as) = show a ++ " " ++ maybe "" show opt_as

instance Applicative Stream where
        pure a = a `Cons` Nothing
        (h1 `Cons` t1) <*> (h2 `Cons` t2) = h1 h2 `Cons` (t1 `opt_ap` t2)
           where
                   Nothing  `opt_ap` Nothing  = Nothing
                   Nothing  `opt_ap` (Just x) = Just (pure h1 <*> x)
                   (Just f) `opt_ap` Nothing  = Just (f <*> pure h2)
                   (Just f) `opt_ap` (Just x) = Just (f <*> x)
                   
instance Functor Stream where
   fmap f (a `Cons` opt_as) = f a `Cons` maybe Nothing (Just . fmap f) opt_as

-- | Lift a value to be a constant stream.
--repeat :: a -> Stream a
--repeat a = a `Cons` repeat a

-- | Zip two streams together.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = f <$> xs <*> ys

-- | Zip three streams together.
zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f xs ys zs = f <$> xs <*> ys <*> zs

-- | Convert a list to a stream. If the list is finite, then the last element of
-- the stream will be an error.
fromFiniteList :: [a] -> a -> Stream a
fromFiniteList (x : xs) end = x `Cons` (Just (fromFiniteList xs end))
fromFiniteList []       end = end `Cons` Nothing

fromList :: [a] -> Stream a
fromList xs = fromFiniteList xs (error "found end of infinite list")


-- | Convert a Stream to a lazy list.
toList :: Stream a -> [a]
toList (x `Cons` opt_xs) = x : maybe (List.repeat x) toList opt_xs 

instance F.Foldable Stream where
  foldMap f (a `Cons` opt_as) = f a `mappend` maybe (F.foldMap f (a `Cons` opt_as))
                                                    (F.foldMap f)
                                                    opt_as

instance Traversable Stream where
  traverse f (a `Cons` opt_as) = Cons <$> f a <*> maybe (pure Nothing) (\ as -> Just <$> traverse f as) opt_as

observeStream :: (Show a) => String -> Stream a -> Stream a
observeStream nm (Cons a rest) = trace (show (nm,a)) $ Cons a $
        case rest of
          Nothing -> trace (show (nm,".")) $ Nothing
          Just xs -> Just $ observeStream nm xs
