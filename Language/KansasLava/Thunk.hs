{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Language.KansasLava.Thunk where

import Language.KansasLava.Circuit
import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Stream hiding (head,zipWith)
import Language.KansasLava.Trace
import Language.KansasLava.Type
import Language.KansasLava.Utils
import Language.KansasLava.Wire
import Language.KansasLava.Testing.Probes

import Data.List
import qualified Data.Map as M
import Data.Maybe

import qualified Data.Graph.Inductive as G

import qualified Data.Reify.Graph as DRG

data Thunk b = forall a. (Ports a, Probe a, Run a) => Thunk a (a -> b)

runT :: Thunk b -> b
runT (Thunk circuit fn) = fn circuit

mkTrace :: (Ports a) => Maybe Int -> Thunk a -> IO Trace
mkTrace c (Thunk circuit k) = do
    let probed = probe "wholeCircuit" circuit

    rc <- reifyCircuit [] $ probed
    rc' <- reifyCircuit [] $ k $ probed -- this is essentially what probeCircuit does

    let pdata = M.fromList [(k,v) | (_,Entity _ _ _ attrs) <- theCircuit rc'
                                  , ProbeValue k v <- attrs ]
        entities = [(id,e) | (id,e@(Entity _ _ _ attrs)) <- theCircuit rc
                           , ProbeValue n v <- attrs]
        pnodes = map fst entities
        ins = M.fromList [ (k,fromJust $ M.lookup name pdata)
                         | (_,Entity _ _ [(_,_,Pad k)] attrs) <- entities
                         , ProbeValue name _ <- attrs]
        out = fromJust $ M.lookup
                         (head [k | let order = G.bfsn [id | (_,_,Port _ id) <- theSinks rc] graph
                                  , let sink = head $ intersect order pnodes
                                  , Just (Entity _ _ _ attrs) <- [lookup sink $ theCircuit rc]
                                  , ProbeValue k _ <- attrs ])
                         pdata
        graph :: G.Gr (MuE DRG.Unique) ()
        graph = rcToGraph rc

    return $ Trace { len = c, inputs = ins, outputs = out, probes = pdata }

mkThunk :: forall a b. (Ports a, Probe a, Run a, RepWire b) => Trace -> a -> Thunk (Seq b)
mkThunk trace circuit = Thunk circuit (\c -> shallowSeq $ toXStream (witness :: b) $ run c trace)

