{-# LANGUAGE FlexibleInstances, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts, Rank2Types, ExistentialQuantification, TypeFamilies #-}
module Language.KansasLava.Testing.Utils where

import Data.List as L

-- surely this exists in the prelude?
mergeWith :: (a -> a -> a) -> [[a]] -> [a]
mergeWith fn probes = go probes []
    where go (p:ps) []  = go ps p
          go (p:ps) acc = go ps $ L.zipWith fn acc p
          go []     acc = acc
