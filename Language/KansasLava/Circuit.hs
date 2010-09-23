module Language.KansasLava.Circuit (toGraph,mergeProbes,mergeProbesIO,remProbes) where

import Language.KansasLava.Internals

import Data.List
import Debug.Trace

import qualified Data.Graph.Inductive as G
import qualified Data.Reify.Graph as DRG

-- In case we want to work with the functional graph algorithms library
toGraph :: Circuit -> G.Gr (MuE DRG.Unique) ()
toGraph rc = G.mkGraph (theCircuit rc) [ (n1,n2,())
                                       | (n1,Entity _ _ ins _) <- theCircuit rc
                                       , (_,_,Port _ n2) <- ins ]

-- Surely this exists somewhere!
updateAL :: (Eq k) => k -> v -> [(k,v)] -> [(k,v)]
updateAL key val list = [ (k,if k == key then val else v) | (k,v) <- list ]

mergeProbesIO :: Circuit -> IO Circuit
mergeProbesIO = return . mergeProbes

-- Rewrites the circuit graph and commons up probes that have the same stream value.
-- Most of debugging gear is written assuming this was run by reifyCircuit.
mergeProbes :: Circuit -> Circuit
mergeProbes circuit = go (probeList circuit) circuit
    where go ((pid,Entity (TraceVal pnames strm) outs ins@[(_,_,d)] attrs):pl) rc =
                         let others = probesOnAL d pl
                             otherIds = [ k | (k,_) <- others, k /= pid ]
                             newNames = nub $ pnames ++ (concatMap snd others)
                             updatedNames = updateAL pid (Entity (TraceVal newNames strm) outs ins attrs) $ theCircuit rc
                         in go pl $ replaceWith (\(Port s _) -> Port s pid) otherIds $ rc { theCircuit = updatedNames }
          go [] rc = rc
          go other _ = error $ "mergeProbes: " ++ show other

replaceWith :: (Driver DRG.Unique -> Driver DRG.Unique) -> [DRG.Unique] -> Circuit -> Circuit
replaceWith _ [] rc = rc
replaceWith y xs rc = rc { theCircuit = newCircuit, theSinks = newSinks }
    where newCircuit = [ (id,Entity n o (map change ins) a)
                       | (id,Entity n o ins a) <- theCircuit rc
                       , id `notElem` xs ]
          newSinks = map change $ theSinks rc

          change (nm,ty,p@(Port s i)) | i `elem` xs = (nm,ty,y p)
          change other = other

probeList :: Circuit -> [(DRG.Unique, MuE DRG.Unique)]
probeList rc = [ (n,e) | (n,e@(Entity (TraceVal _ _) _ _ _)) <- theCircuit rc ]

probesOn :: Driver DRG.Unique -> Circuit -> [(DRG.Unique,[OVar])]
probesOn x rc = probesOnAL x $ theCircuit rc

probesOnAL :: Driver DRG.Unique -> [(DRG.Unique, MuE DRG.Unique)] -> [(DRG.Unique,[OVar])]
probesOnAL x al = [ (id,nms) | (id, Entity (TraceVal nms _) _ ins _) <- al
                             , (_,_,d) <- ins
                             , d == x ]

remProbes :: Circuit -> Circuit
remProbes circuit = go (probeList circuit) circuit
    where go ((pid,Entity _ _ [(_,_,d)] _):pl) rc =
                         let probes = pid : [ id | (id,_) <- probesOnAL d pl ]
                         in go pl $ replaceWith (\_ -> d) probes rc
          go [] rc = rc
          go other _ = error $ "remProbes: " ++ show other
