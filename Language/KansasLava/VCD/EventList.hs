module Language.KansasLava.VCD.EventList
    ( EventList
    , foldrWithTime
    , toList
    , fromList
    , empty
    , singleton
    , length
    , head
    , last
    , take
    , drop
    , insert
{-
    , snoc
    , append
-}
    , zipWith
    , mergeWith
    ) where

import Control.Monad

import qualified Data.Foldable as F
import qualified Data.IntMap as M
import Data.Maybe

import Prelude hiding (take,length,zipWith,last,head,drop)
import qualified Prelude as Prelude

----------------------------------------------------------------------------------------

-- | A list of changes, indexed from 0, stored in reverse order.
-- Obviously, don't try to represent an infinite list.
-- [(6,A),(0,B)]
newtype EventList a = EL { unEL :: M.IntMap a }
    deriving (Eq,Show,Read)

instance (Ord a) => Ord (EventList a) where
    compare exs eys = compare (toList exs) (toList eys)

instance Functor EventList where
    fmap f (EL evs) = EL $ M.map f evs

instance F.Foldable EventList where
--    foldr :: (a -> b -> b) -> b -> EventList a -> b
    foldr f z (EL m) = M.fold f z m

foldrWithTime :: ((Int,a) -> b -> b) -> b -> EventList a -> b
foldrWithTime f z (EL m) = M.foldWithKey (curry f) z m
{-
instance (Show a) => Show (EventList a) where
    show = show . toList

instance (Eq a, Read a) => Read (EventList a) where
    readsPrec p str = [ (fromList l,r) | (l,r) <- readsPrec p str ]
-}

-- | Convert an event list to a normal list
toList :: EventList a -> [a]
toList (EL evs) = fst $ foldr f ([],Nothing) $ M.toAscList evs
    where f :: (Int,a) -> ([a],Maybe Int) -> ([a],Maybe Int)
          f (i,v) (l,p) = (replicate ((fromMaybe (i+1) p) - i) v ++ l,Just i)

-- | Convert a list to an event list
fromList :: (Eq a) => [a] -> EventList a
fromList = EL . M.fromDistinctAscList . dedupe Nothing . zip [0..]
    where dedupe _ [] = []
          dedupe _ [(i,v)] = [(i,v)] -- always keep the last item for size
          dedupe Nothing ((i,v):r) = (i,v) : dedupe (Just v) r
          dedupe (Just p) ((i,v):r) | v == p = dedupe (Just v) r
                                    | otherwise = (i,v) : dedupe (Just v) r

empty :: EventList a
empty = EL M.empty

singleton :: (Int,a) -> EventList a
singleton = EL . uncurry M.singleton
{-
snoc :: (Eq a) => EventList a -> a -> EventList a
snoc el@(EL evs) v = EL $ (length el,v) : dropWhile ((== v) . snd) evs
-}

-- | Insert/update an event in an EventList
insert :: (Eq a) => (Int, a) -> EventList a -> EventList a
insert (i,v) (EL m) = EL $ maybe (if M.null b || last (EL b) /= v
                                  then M.insert i v m
                                  else m)
                                 (const m) p
    where (b,p,_) = M.splitLookup i m

head :: EventList a -> a
head (EL m) | M.null m = error "EventList.head: empty list"
            | otherwise = Prelude.head $ M.elems m


last :: EventList a -> a
last (EL m) | M.null m = error "EventList.last: empty list"
            | otherwise = Prelude.last $ M.elems m

-- | length for event lists O(n)
length :: EventList a -> Int
length (EL m) = case reverse $ M.keys m of
                    [] -> 0
                    (k:_) -> k + 1

-- | take for event lists.
take :: Int -> EventList a -> EventList a
take i (EL m) | i < 0 = error "EventList.take negative index"
              | i > length (EL m) = EL m
              | otherwise = if length el' == i
                            then el'
                            else EL $ M.insert (i-1) (if M.null b then undefined else last el') b
    where (b,_) = M.split i m
          el' = EL b

-- | drop for event lists. (TODO: this is buggy, fix it)
drop :: Int -> EventList a -> EventList a
drop i (EL m) = EL m'
    where (b,p,a) = M.splitLookup (i+1) m -- if first item is not at zero, add last item in b
          m' = M.fromAscList $ (maybe [] (\v -> [(0,v)]) p)
                            ++ case [ (i'-i,v) | (i',v) <- M.toAscList a ] of
                                [] -> []
                                l@((0,_):_) -> l
                                l -> maybe ((0, if M.null b then undefined else last (EL b)) : l)
                                           (const l) p

{-
drop i (EL evs) = EL [ (i'-i,v) | (i',v) <- takeWhile ((>= i) . fst) evs ]

append :: (Eq a) => EventList a -> EventList a -> EventList a
append el@(EL xs) (EL ys) = EL $ [ (i+l,v) | (i,v) <- ys ] ++ (fix xs ys)
    where l = length el

          fix [] _  = []
          fix as [] = as
          fix as bs | snd (Prelude.head as) == snd (Prelude.last bs) = tail as
                    | otherwise = as
-}
-- | zipWith for event lists.
-- zipWith f xs ys = fromList $ zipWith f (toList xs) (toList ys)
zipWith :: (Eq c) => (a -> b -> c) -> EventList a -> EventList b -> EventList c
zipWith f xs ys = EL $ M.fromList $ go (ea,eb) (lst xs) (lst ys)
    where lst = M.assocs . unEL . take l
          l = min (length xs) (length ys)
          ea = error "zipWith: no initial value in list a"
          eb = error "zipWith: no initial value in list b"

          go (pa,_) [] bs = [ (i,f pa b) | (i,b) <- bs ]
          go (_,pb) as [] = [ (i,f a pb) | (i,a) <- as ]
          go (pa,pb) ((i,a):as) ((i',b):bs) | i < i'    = (i ,f a  pb) : go (a,pb) as         ((i',b):bs)
                                            | i == i'   = (i ,f a  b ) : go (a,b ) as         bs
                                            | otherwise = (i',f pa b ) : go (pa,b) ((i,a):as) bs

-- | Like zipWith, but generalized to a list of event lists.
mergeWith :: (Eq a) => (a -> a -> a) -> [EventList a] -> EventList a
mergeWith _ [] = fromList []
mergeWith f ls = foldr1 (zipWith f) ls
