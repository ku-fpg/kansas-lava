-- | This module provides some utility functions for working with probes.
module Language.KansasLava.Circuit (toGraph,mergeProbes,mergeProbesIO,remProbes) where

import Language.KansasLava.Types hiding (probes)

import Data.List

import qualified Data.Graph.Inductive as G
import qualified Data.Reify.Graph as DRG

-- | Convert a 'Circuit' to a fgl graph.
toGraph :: Circuit -> G.Gr (Entity DRG.Unique) ()
toGraph rc = G.mkGraph (theCircuit rc) [ (n1,n2,())
                                       | (n1,Entity _ _ ins) <- theCircuit rc
                                       , (_,_,Port _ n2) <- ins ]

-- | Gives probes their node ids. This should be run after mergeProbes.
addProbeIds :: Circuit -> Circuit
addProbeIds circuit = circuit { theCircuit = newCircuit }
    where newCircuit = [ addId entity | entity <- theCircuit circuit ]
          addId (nid, Entity (TraceVal nms strm) outs ins) = (nid, Entity (TraceVal (map (addToName nid) nms) strm) outs ins)
          addId other = other
          addToName nid (OVar _ nm) = OVar nid nm

-- | Lift the pure 'mergeProbes' function into the 'IO' monad.
mergeProbesIO :: Circuit -> IO Circuit
mergeProbesIO = return . mergeProbes

-- | Rewrites the circuit graph and commons up probes that have the same stream value.
mergeProbes :: Circuit -> Circuit
mergeProbes circuit = addProbeIds $ go (probeList circuit) circuit
    where go ((pid,Entity (TraceVal pnames strm) outs ins@[(_,_,d)]):pl) rc =
                         let others = probesOnAL d pl
                             otherIds = [ k | (k,_) <- others, k /= pid ]
                             newNames = nub $ pnames ++ concatMap snd others
                             updatedNames = updateAL pid (Entity (TraceVal newNames strm) outs ins) $ theCircuit rc
                         in go pl $ replaceWith (f pid)  otherIds $ rc { theCircuit = updatedNames }
          go [] rc = rc
          go other _ = error $ "mergeProbes: " ++ show other
          f pid (Port s _) = Port s pid
          f _ p = p

-- | Removes all probe nodes from the circuit.
remProbes :: Circuit -> Circuit
remProbes circuit = go (probeList circuit) circuit
    where go ((pid,Entity _ _ [(_,_,d)]):pl) rc =
                         let probes = pid : [ ident | (ident,_) <- probesOnAL d pl ]
                         in go pl $ replaceWith (\_ -> d) probes rc
          go [] rc = rc
          go other _ = error $ "remProbes: " ++ show other

-- Below is not exported.

-- Surely this exists somewhere!
updateAL :: (Eq k) => k -> v -> [(k,v)] -> [(k,v)]
updateAL key val list = [ (k,if k == key then val else v) | (k,v) <- list ]

replaceWith :: (Driver DRG.Unique -> Driver DRG.Unique) -> [DRG.Unique] -> Circuit -> Circuit
replaceWith _ [] rc = rc
replaceWith y xs rc = rc { theCircuit = newCircuit, theSinks = newSinks }
    where newCircuit = [ (ident,Entity n o (map change ins))
                       | (ident,Entity n o ins) <- theCircuit rc
                       , ident `notElem` xs ]
          newSinks = map change $ theSinks rc

          change (nm,ty,p@(Port _ i)) | i `elem` xs = (nm,ty,y p)
          change other = other

probeList :: Circuit -> [(DRG.Unique, Entity DRG.Unique)]
probeList rc = [ (n,e) | (n,e@(Entity (TraceVal _ _) _ _)) <- theCircuit rc ]

-- probesOn :: Driver DRG.Unique -> Circuit -> [(DRG.Unique,[ProbeName])]
-- probesOn x rc = probesOnAL x $ theCircuit rc

probesOnAL :: Driver DRG.Unique -> [(DRG.Unique, Entity DRG.Unique)] -> [(DRG.Unique,[OVar])]
probesOnAL x al = [ (ident,nms) | (ident, Entity (TraceVal nms _) _ ins) <- al
                             , (_,_,d) <- ins
                             , d == x ]

