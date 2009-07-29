module Language.KansasLava.Reify where

import Data.Reify

import Language.KansasLava.Entity
import Language.KansasLava.Signal

--------------------------------------------------------
-- Grab a set of drivers (the outputs), and give me a graph, please.

reifyCircuit :: [(Var, Driver E)] -> IO ([(Unique,Entity Unique)],[(Var,Driver Unique)])
reifyCircuit outputs = do
        (Graph nodes root) <- reifyGraph root
        let nodes1 = [ (u,node) | (u,node) <- nodes, u /= root ]
        let entries = concat [ args | (u,Entity _ _ args) <- nodes, u == root ]
        return (nodes1,entries)
   where root = E $ Entity (Name "$" "ROOT") [] outputs
