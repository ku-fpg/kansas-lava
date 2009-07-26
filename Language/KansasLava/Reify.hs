module Language.KansasLava.Reify where

import Data.Reify

import Language.KansasLava.Entity
import Language.KansasLava.Signal

--------------------------------------------------------
-- Grab a signal, give me a graph, please.

reifyCircuit :: Signal a -> IO (Graph Entity)
reifyCircuit (Signal _ w) = reifyGraph w

