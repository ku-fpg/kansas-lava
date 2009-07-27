module Language.KansasLava.Reify where

import Data.Reify

import Language.KansasLava.Entity
import Language.KansasLava.Signal

--------------------------------------------------------
-- Grab a signal, give me a graph, please.

reifyCircuit :: ESignal a -> IO (Graph Entity)
reifyCircuit (ESignal _ w) = reifyGraph w


{-
-- You want to observ
instance MuRef E where 
  type DeRef E = Entity
  mapDeRef f (E s) = T.traverse (T.traverse f) s 
-}
