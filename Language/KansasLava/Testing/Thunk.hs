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
    let uname = "wholeCircuit5471" -- probably need a better solution than this
    let probed = probe uname circuit

    rc <- reifyCircuit [] $ probed
    rc' <- reifyCircuit [] $ k $ probed -- this is essentially what probeCircuit does

    let pdata = [ (k,v) | (_,Entity _ _ _ attrs) <- theCircuit rc'
                       , ProbeValue k v <- attrs ]
        io = sortBy (\(k1,_) (k2,_) -> compare k1 k2) [ s | s@(OVar _ name, _) <- pdata, name == uname ]
        ins = M.fromList $ init io
        out = snd $ last io

    return $ Trace { len = c, inputs = ins, outputs = out, probes = M.fromList pdata }

mkThunk :: forall a b. (Ports a, Probe a, Run a, RepWire b) => Trace -> a -> Thunk (Seq b)
mkThunk trace circuit = Thunk circuit (\c -> shallowSeq $ toXStream (witness :: b) $ run c trace)

mkTarball :: (Ports b) => FilePath -> Thunk b -> IO ()
mkTarball tarfile thunk@(Thunk c k) = do
    let (path,_) = splitExtension tarfile

    createDirectoryIfMissing True path

    trace <- mkTrace (Just 100) thunk

    writeFile (path </> "circuit" <.> "input") $ unlines $ genShallow trace
    writeFile (path </> "circuit" <.> "info") $ unlines $ genInfo trace

    mkTestbench [] [] "circuit" path c

rcToGraph :: ReifiedCircuit -> G.Gr (MuE DRG.Unique) ()
rcToGraph rc = G.mkGraph (theCircuit rc) [ (n1,n2,())
                                         | (n1,Entity _ _ ins _) <- theCircuit rc
                                         , (_,_,Port _ n2) <- ins ]
