-- | This module is for internal functions used in more than one place
module Language.KansasLava.Internal where

takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe = maybe id take

-- surely this exists in the prelude?
mergeWith :: (a -> a -> a) -> [[a]] -> [a]
mergeWith _ [] = []
mergeWith f ls = foldr1 (Prelude.zipWith f) ls

splitLists :: [[a]] -> [Int] -> [[[a]]]
splitLists xs (i:is) = map (take i) xs : splitLists (map (drop i) xs) is
splitLists _  []     = [[]]
