module Language.KansasLava.Seq
        where

import Data.Traversable
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad
import Prelude hiding (zipWith,zipWith3)
import Data.Monoid

infixr 5 :~

-- A clocked sequence of values, which can be undefined (Nothing),  or have a specific value.
data Seq a = Maybe a :~ Seq a
           | Constant (Maybe a)

instance Show a => Show (Seq a) where
   show (Constant v) = showV v
   show vs           = unwords [ showV x ++ " :~ "
                                | x <- take 20 $ toList vs
                                ] ++ "..."


showV :: (Show a) => Maybe a -> String
showV Nothing = "?"
showV (Just v) = show v

        -- Just a :~ pure a

instance Applicative Seq where
        pure a = Constant (Just a)
        (Constant h1) <*> (h2 :~ t2)    = (h1 `ap` h2) :~ (Constant h1 <*> t2)
        (h1 :~ t1) <*> (Constant h2)    = (h1 `ap` h2) :~ (t1 <*> Constant h2)
        (h1 :~ t1) <*> (h2 :~ t2)       = (h1 `ap` h2) :~ (t1 <*> t2)
        (Constant h1) <*> (Constant h2) = Constant (h1 `ap` h2)

undefinedSeq :: Seq a
undefinedSeq = Constant Nothing


instance Functor Seq where
   fmap f (a :~ as) = liftM f a :~ fmap f as
   fmap f (Constant a) = Constant $ liftM f a


head :: Seq a -> Maybe a
head (Constant a) = a
head (a :~ _) = a

tail (Constant a) = (Constant a)
tail (_ :~ as) = as

zipWith' :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith' f xs ys = pure f <*> xs <*> ys

fromList :: [Maybe a] -> Seq a

fromList (x : xs) = x :~ fromList xs
fromList []       = Constant Nothing -- error "Seq.fromList"

toList :: Seq a -> [Maybe a]
toList (x :~ xs) = x : toList xs
toList (Constant x) = repeat x

-- unlike using <*>, etc, this allows the unchosen Seq to be undefined at this time.
seqMux :: Seq Bool -> Seq a -> Seq a -> Seq a
seqMux sB sT sF =
	fromList [ case b of
		    Just True  -> t
		    Just False -> f
		    Nothing    -> Nothing
	         | (b,t,f) <- zip3 (toList sB) (toList sT) (toList sF)
	         ]


instance F.Foldable Seq where
  foldMap f (a :~ as) = F.foldMap f a `mappend` F.foldMap f as


instance Traversable Seq where
  traverse f (a :~ as) = (:~) <$> traverse f a <*> traverse f as
  traverse f (Constant a) = Constant <$> traverse f a

