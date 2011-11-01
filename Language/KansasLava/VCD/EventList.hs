module Language.KansasLava.VCD.EventList
    ( EventList(..)
    , toList
    , toList'
    , fromList
    , length
    , take
    , mergeWith
    , zipWith
    , insert
    ) where

import Control.Monad
import Prelude hiding (take,length,zipWith)

----------------------------------------------------------------------------------------

-- | A list of changes, indexed from 0
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
toList = toList' $ error "toList: no initial value!"

-- | Like toList, but accepts initial value for case that
-- first event is not at timestep 0
toList' :: a -> EventList a -> [a]
toList' iv (EL xs) = go (0,iv) xs
    where go _      []          = []
          go (p,px) ((i,x):xs') = replicate (i-p) px ++ go (i,x) xs'

-- | Convert a list to an event list
fromList :: (Eq a) => [a] -> EventList a
fromList xs = EL (go (0,undefined) xs)
    where go :: (Eq a) => (Int,a) -> [a] -> [(Int,a)]
          go (0,_) [] = []
          go (i,p) [] = [(i,p)] -- this tracks length, value is thrown away by toList
          go (i,p) (x:xs') | checkpoint i || p /= x = (i,x) : go (i+1,x) xs'
                           | otherwise              =         go (i+1,x) xs'

          -- to deal with the case of an infinitely repeating list
          -- i.e. fromList $ repeat 1
          -- we record the value every 1000 entries
          checkpoint = (== 0) . (`mod` 1000)

-- | Insert/update an event in an EventList
-- This implementation is easy to understand, but is probably
-- really inefficient if deforestation isn't miraculous
insert :: (Eq a) => (Int, a) -> EventList a -> EventList a
insert (i,v) el = fromList $ b ++ (v : a)
    where (b,a) = splitAt i $ toList el

{- certainly exposes the flaw in having length encoded the way we do.
 - TODO: fix length encoding
insert :: (Eq a) => (Int, a) -> EventList a -> EventList a
insert p@(i,v) (EL evs) = EL $ evs'
    where (b',a') = span ((< i) . fst) evs
          a = dropWhile ((== i) . fst) a' -- an existing event at time i is replaced
          (eq,b) = span ((== v) . snd) $ reverse b'
          -- b: events strictly before i, not equal to v, in reverse order
          -- eq: events strictly before i, equal to v, in reverse order
          -- a: events strictly after i
          evs' = reverse b ++ (if null eq
                                  then if null a
                                          then [p,(i+1,v)]
                                          else [p]
                                  else if null a
                                          then [last eq,(i+1,v)]
                                          else [last eq]) ++ a
-}

-- | length for event lists.
length :: EventList a -> Int
length (EL []) = 0
length (EL xs) = fst $ last xs

-- | take for event lists.
take :: Int -> EventList a -> EventList a
take i (EL evs) = EL $ evs' ++ if null r then [] else final
    where (evs',r) = span ((<= i) . fst) evs
          final = [(i, case evs' of [] -> undefined; _ -> snd $ last evs')]

-- | zipWith for event lists.
-- zipWith f xs ys = fromList $ zipWith f (toList xs) (toList ys)
zipWith :: (Eq c) => (a -> b -> c) -> EventList a -> EventList b -> EventList c
zipWith f xs ys = EL $ go (ea,eb) (unEL $ take l xs) (unEL $ take l ys)
    where l = min (length xs) (length ys)
          ea = error "zipWith: no initial value in a-list"
          eb = error "zipWith: no initial value in b-list"

          go (pa,_) [] bs = [ (i,f pa b) | (i,b) <- bs ]
          go (_,pb) as [] = [ (i,f a pb) | (i,a) <- as ]
          go (pa,pb) ((i,a):as) ((i',b):bs) | i < i'    = (i ,f a  pb) : go (a,pb) as         ((i',b):bs)
                                            | i == i'   = (i ,f a  b ) : go (a,b ) as         bs
                                            | otherwise = (i',f pa b ) : go (pa,b) ((i,a):as) bs

-- | Like zipWith, but generalized to a list of event lists.
mergeWith :: (Eq a) => (a -> a -> a) -> [EventList a] -> EventList a
mergeWith _ [] = fromList []
mergeWith f ls = foldr1 (zipWith f) ls
