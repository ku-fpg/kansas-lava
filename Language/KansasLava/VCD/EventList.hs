module Language.KansasLava.VCD.EventList
    ( EventList(..)
    , toList
    , fromList
    , length
    , take
    , drop
    , mergeWith
    , zipWith
    , insert
    , last
    , head
    , snoc
    , append
    ) where

import Control.Monad
import Prelude hiding (take,length,zipWith,last,head,drop)
import qualified Prelude as Prelude

----------------------------------------------------------------------------------------

-- | A list of changes, indexed from 0, stored in reverse order.
-- Obviously, don't try to represent an infinite list.
-- [(6,A),(0,B)]
newtype EventList a = EL { unEL :: [(Int,a)] }
    deriving (Eq,Show,Read)

instance (Ord a) => Ord (EventList a) where
    compare exs eys = compare (toList exs) (toList eys)

instance Functor EventList where
    fmap f (EL evs) = EL [ (i,f v) | (i,v) <- evs ]

{-
instance (Show a) => Show (EventList a) where
    show = show . toList

instance (Eq a, Read a) => Read (EventList a) where
    readsPrec p str = [ (fromList l,r) | (l,r) <- readsPrec p str ]
-}

-- | Convert an event list to a normal list
toList :: EventList a -> [a]
toList (EL xs) = foldr f [] xs
    where f :: (Int,a) -> [a] -> [a]
          f (i,v) l = l ++ replicate (1 + i - Prelude.length l) v

-- | Convert a list to an event list
fromList :: (Eq a) => [a] -> EventList a
fromList = foldl snoc (EL [])

snoc :: (Eq a) => EventList a -> a -> EventList a
snoc el@(EL evs) v = EL $ (length el,v) : dropWhile ((== v) . snd) evs

-- | Insert/update an event in an EventList
-- This implementation is easy to understand, but is probably
-- really inefficient if deforestation isn't miraculous
insert :: (Eq a) => (Int, a) -> EventList a -> EventList a
insert (i,v) el | i >= 0 = append (snoc (take i el) v) (drop (i+1) el)
                | otherwise = error "EventList.insert: negative index"

head :: EventList a -> a
head (EL [])        = error "EventList.head: empty list"
head (EL evs)       = snd $ Prelude.last evs

last :: EventList a -> a
last (EL [])        = error "EventList.last: empty list"
last (EL ((_,v):_)) = v

-- | length for event lists.
length :: EventList a -> Int
length (EL []) = 0
length (EL ((i,_):_)) = i + 1

-- | take for event lists.
take :: Int -> EventList a -> EventList a
take i (EL evs) = EL $ case a of
                        [] -> b
                        _  | length (EL b) == i -> b
                           | otherwise          -> (i-1,snd $ Prelude.last a) : b
    where (a,b) = span ((>= i) . fst) evs

drop :: Int -> EventList a -> EventList a
drop i (EL evs) = EL [ (i'-i,v) | (i',v) <- takeWhile ((>= i) . fst) evs ]

append :: (Eq a) => EventList a -> EventList a -> EventList a
append el@(EL xs) (EL ys) = EL $ [ (i+l,v) | (i,v) <- ys ] ++ (fix xs ys)
    where l = length el

          fix [] _  = []
          fix as [] = as
          fix as bs | snd (Prelude.head as) == snd (Prelude.last bs) = tail as
                    | otherwise = as

-- | zipWith for event lists.
-- zipWith f xs ys = fromList $ zipWith f (toList xs) (toList ys)
zipWith :: (Eq c) => (a -> b -> c) -> EventList a -> EventList b -> EventList c
zipWith f xs ys = EL $ go (ea,eb) (unEL $ take l xs) (unEL $ take l ys)
    where l = min (length xs) (length ys)
          ea = error "zipWith: something is very wrong - a"
          eb = error "zipWith: something is very wrong - b"

          go (pa,_) [] bs = [ (i,f pa b) | (i,b) <- bs ]
          go (_,pb) as [] = [ (i,f a pb) | (i,a) <- as ]
          go (pa,pb) ((i,a):as) ((i',b):bs) | i > i'    = (i ,f a  pb) : go (a,pb) as         ((i',b):bs)
                                            | i == i'   = (i ,f a  b ) : go (a,b ) as         bs
                                            | otherwise = (i',f pa b ) : go (pa,b) ((i,a):as) bs

-- | Like zipWith, but generalized to a list of event lists.
mergeWith :: (Eq a) => (a -> a -> a) -> [EventList a] -> EventList a
mergeWith _ [] = fromList []
mergeWith f ls = foldr1 (zipWith f) ls
