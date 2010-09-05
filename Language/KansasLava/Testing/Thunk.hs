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

import System.Cmd
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

mkTarball :: (Ports b) => FilePath -> Int -> Thunk b -> IO ()
mkTarball tarfile cycles thunk@(Thunk c k) = do
    let (path,_) = splitExtension tarfile
        name = "circuit"

    createDirectoryIfMissing True path

    trace <- mkTrace (Just cycles) thunk

    writeFile (path </> name <.> "input") $ unlines $ genShallow trace
    writeFile (path </> name <.> "info") $ unlines $ genInfo trace

    mkTestbench [] [] name path c

    writeFile (path </> "test" <.> "sh") $ unlines
        ["#!/bin/bash"
        ,"LM_LICENSE_FILE=1800@carl.ittc.ku.edu:1717@carl.ittc.ku.edu"
        ,"export LM_LICENSE_FILE"
        ,"echo \"Simulating...\""
        ,"/tools/modelsim/linux/6.3c/modeltech/bin/vsim -c -do circuit.do"
        ,"echo \"10 lines from the info file...\""
        ,"tail " ++ name ++ ".info"
        ,"echo \"The same 10 lines from the input file...\""
        ,"tail " ++ name ++ ".input"
        ,"echo \"Ditto for the output file...\""
        ,"tail " ++ name ++ ".output"
        ,""
        ,"THEDIFF=`diff " ++ name ++ ".input " ++ name ++ ".output`"
        ,""
        ,"if [[ -z \"$THEDIFF\" ]]; then"
        ,"    echo \"Input/Output Files Are The Same\""
        ,"    exit 0"
        ,"else"
        ,"    echo \"Warning: Differences Below:\""
        ,"    echo \"$THEDIFF\""
        ,"    exit 1"
        ,"fi"
        ]

    system $ "chmod +x " ++ path </> "test.sh"

    return ()

rcToGraph :: Circuit -> G.Gr (MuE DRG.Unique) ()
rcToGraph rc = G.mkGraph (theCircuit rc) [ (n1,n2,())
                                         | (n1,Entity _ _ ins _) <- theCircuit rc
                                         , (_,_,Port _ n2) <- ins ]
