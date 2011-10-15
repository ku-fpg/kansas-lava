-- | This module is for internal functions used in more than one place
module Language.KansasLava.Internal where

import Data.Maybe  as Maybe
import Language.KansasLava.Stream as Stream
import Prelude hiding (tail, lookup)


takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe = maybe id take

-- surely this exists in the prelude?
mergeWith :: (a -> a -> a) -> [[a]] -> [a]
mergeWith _ [] = []
mergeWith f ls = foldr1 (Prelude.zipWith f) ls

splitLists :: [[a]] -> [Int] -> [[[a]]]
splitLists xs (i:is) = map (take i) xs : splitLists (map (drop i) xs) is
splitLists _  []     = [[]]


-- | Stepify allows us to make a stream element-strict.
class Stepify a where
  stepify :: a -> a

-- | Strictly apply a function to each element of a Stream.
stepifyStream :: (a -> ()) -> Stream a -> Stream a
stepifyStream f (Cons a r) = Cons a (f a `seq` stepifyStream f r)

-- | A 'Radix' is a trie indexed by bitvectors.
data Radix a
  = Res !a -- ^ A value stored in the tree
  | NoRes -- ^ Non-present value
  -- | A split-node, left corresponds to 'True' key bit, right corresponds to 'False' key bit.
  | Choose !(Radix a) !(Radix a)
	deriving Show

-- | The empty tree
empty :: Radix a
empty = NoRes

-- | Add a value (keyed by the list of bools) into a tree
insert :: [Bool] -> a -> Radix a -> Radix a
insert []    y (Res _) = Res $! y
insert []    y NoRes   = Res $! y
insert []    _ (Choose _ _) = error "inserting with short key"
insert xs     y NoRes   = insert xs y (Choose NoRes NoRes)
insert _  _ (Res _) = error "inserting with too long a key"
insert (True:a) y (Choose l r) = Choose (insert a y l) r
insert (False:a) y (Choose l r) = Choose l (insert a y r)


-- | Find a value in a radix tree
find :: [Bool] -> Radix a -> Maybe a
find [] (Res v) = Just v
find [] NoRes   = Nothing
find [] _       = error "find error with short key"
find (_:_) (Res _) = error "find error with long key"
find (_:_) NoRes   = Nothing
find (True:a) (Choose l _) = find a l
find (False:a) (Choose _ r) = find a r


