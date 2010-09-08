module Language.KansasLava.Circuit (toGraph) where

{-
import Data.Reify
import Data.List as L
import qualified Data.Map as Map

import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Types

import Debug.Trace

-------------------------------------------------------------------------

-}
import Language.KansasLava.Internals

import qualified Data.Graph.Inductive as G
import qualified Data.Reify.Graph as DRG

toGraph :: Circuit -> G.Gr (MuE DRG.Unique) ()
toGraph rc = G.mkGraph (theCircuit rc) [ (n1,n2,())
                                       | (n1,Entity _ _ ins _) <- theCircuit rc
                                       , (_,_,Port _ n2) <- ins ]
