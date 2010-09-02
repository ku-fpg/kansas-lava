{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Language.KansasLava.Testing.Thunk where

import Language.KansasLava

import Language.KansasLava.Testing.Bench
import Language.KansasLava.Testing.Probes
import Language.KansasLava.Testing.Trace

import Data.List
import qualified Data.Map as M
import Data.Maybe

import qualified Data.Graph.Inductive as G

import qualified Data.Reify.Graph as DRG

import System.Directory
import System.FilePath.Posix

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

mkTarball :: (Ports b) => FilePath -> Thunk b -> IO ()
mkTarball tarfile thunk@(Thunk c k) = do
    let (path,_) = splitExtension tarfile

    createDirectoryIfMissing True path

    trace <- mkTrace (Just 100) thunk

    writeFile (path </> "shallow") $ unlines $ genShallow trace
    writeFile (path </> "info") $ unlines $ genInfo trace
