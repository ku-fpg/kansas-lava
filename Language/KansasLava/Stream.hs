module Language.KansasLava.Stream where

import Data.Traversable
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Prelude hiding (zipWith)
import Data.Monoid

infixr 5 :~

-- A clocked sequence of values, which can be undefined (Nothing),  or have a specific value.
data Stream a = a :~ Stream a

instance Show a => Show (Stream a) where
   show = showStream 20

showStream :: Show a => Int -> Stream a -> String
showStream i vs           = unwords [ show x ++ " :~ "
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
        -- Just a :~ pure a

instance Applicative Stream where
        pure a = a :~ pure a
--        (Constant h1) <*> (h2 :~ t2)    = (h1 $ h2) :~ (Constant h1 <*> t2)
--        (h1 :~ t1) <*> (Constant h2)    = (h1 $ h2) :~ (t1 <*> Constant h2)
        (h1 :~ t1) <*> (h2 :~ t2)       = (h1 $ h2) :~ (t1 <*> t2)
--        (Constant h1) <*> (Constant h2) = Constant (h1 $ h2)
--undefinedStream :: Stream a
--undefinedStream = Constant Nothing


instance Functor Stream where
   fmap f (a :~ as) = f a :~ fmap f as
--   fmap f (Constant a) = Constant $ f a

{-
fmapWithFail :: (a -> Maybe b) -> Stream a -> Stream b
fmapWithFail f (Nothing :~ as) = Nothing :~ fmapWithFail f as
fmapWithFail f (Just a :~ as) = f a :~ fmapWithFail f as
fmapWithFail f (Constant Nothing) =  Constant Nothing
fmapWithFail f (Constant (Just a)) = Constant (f a)
-}

head :: Stream a -> a
--head (Constant a) = a
head (a :~ _) = a

tail :: Stream a -> Stream a
--tail (Constant a) = (Constant a)
tail (_ :~ as) = as

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = pure f <*> xs <*> ys

fromList :: [a] -> Stream a
fromList (x : xs) = x :~ fromList xs
fromList []       = error "Stream.fromList"

toList :: Stream a -> [a]
toList (x :~ xs) = x : toList xs
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
  foldMap f (a :~ as) = f a `mappend` F.foldMap f as
  foldMap _ _ = error "Foldable.foldMap(Stream)"


instance Traversable Stream where
  traverse f (a :~ as) = (:~) <$> f a <*> traverse f as
--  traverse f (Constant a) = Constant <$> f a

