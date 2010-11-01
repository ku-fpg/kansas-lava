module Language.KansasLava.Testing.Utils where

import Data.List
import Data.Maybe

-- surely this exists in the prelude?
mergeWith :: (a -> a -> a) -> [[a]] -> [a]
mergeWith fn probes = go probes []
    where go (p:ps) []  = go ps p
          go (p:ps) acc = go ps $ zipWith fn acc p
          go []     acc = acc

-- Seems like this should also exist in the Prelude
splitWith :: Char -> String -> (String,String)
splitWith c s = go s []
     where go [] acc = (reverse acc,[])
           go (i:inp) acc | i == c = (reverse acc,inp)
                          | otherwise = go inp (i:acc)

-- | lookupAll has the crucial property that the results come
-- | back in the same order as the given key list
lookupAll :: Eq a => [a] -> [(a,b)] -> [(a,b)]
lookupAll keys lst = [ (key,val) | (key,Just val) <- filter (isJust . snd)
                                                            [ (k,lookup k lst) | k <- keys ]
                     ]
