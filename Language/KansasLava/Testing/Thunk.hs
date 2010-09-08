{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Language.KansasLava.Testing.Thunk (Thunk(..), runShallow, runDeep, mkThunk, mkTrace, mkTarball, runTarball) where

import Language.KansasLava

import Language.KansasLava.Testing.Bench
import Language.KansasLava.Testing.Probes
import Language.KansasLava.Testing.Trace

import Data.List
import qualified Data.Map as M
import Data.Maybe

import System.Cmd
import System.Directory
import System.FilePath.Posix
import System.Posix.Directory

data Thunk b = forall a. (Ports a, Probe a, Run a, Ports b) => Thunk a (a -> b)

runShallow :: Thunk b -> b
runShallow (Thunk circuit fn) = fn circuit

runDeep :: (Ports b) => String -> Int -> Thunk b -> IO ()
runDeep name cycles thunk = do
    tmp <- getTemporaryDirectory

    let tarfile = tmp </> name <.> "tgz"

    mkTarball tarfile cycles thunk
    runTarball tarfile

    removeFile tarfile

mkTrace :: (Ports a) => Maybe Int -> Thunk a -> IO Trace
mkTrace c (Thunk circuit k) = do
    let uname = "wholeCircuit5471" -- probably need a better solution than this
    let probed = probe uname circuit

    rc <- reifyCircuit $ k $ probed -- this is essentially what probeCircuit does

    let pdata = [ (k,v) | (_,Entity _ _ _ attrs) <- theCircuit rc
                       , ProbeValue k v <- attrs ]
        io = sortBy (\(k1,_) (k2,_) -> compare k1 k2) [ s | s@(OVar _ name, _) <- pdata, name == uname ]
        ins = M.fromList $ init io
        out = snd $ last io

    return $ Trace { len = c, inputs = ins, outputs = out, probes = M.fromList pdata }

mkThunk :: forall a b. (Ports a, Probe a, Run a, Rep b) => Trace -> a -> Thunk (Seq b)
mkThunk trace circuit = Thunk circuit (\c -> shallowSeq $ toXStream (witness :: b) $ run c trace)

runTarball :: FilePath -> IO ()
runTarball tarfile = do
    exists <- doesFileExist tarfile

    if exists
        then do
            tmp <- getTemporaryDirectory

            let path = tmp </> name
                name = last $ splitPath $ dropExtensions tarfile

            system $ unwords ["tar -xzf",tarfile,"-C",tmp,"--transform='s,/?" ++ dropDrive tmp ++ ",,ix'"]

            cwd <- getWorkingDirectory
            changeWorkingDirectory path

            system $ path </> "run"

            changeWorkingDirectory cwd -- restore cwd to previous state

            -- path better not contain symlinks!!! (or this will follow them)
            removeDirectoryRecursive path

        else return ()

mkTarball :: (Ports b) => FilePath -> Int -> Thunk b -> IO ()
mkTarball tarfile cycles thunk@(Thunk c k) = do
    tmp <- getTemporaryDirectory

    let path = tmp </> name
        name = last $ splitPath $ dropExtensions tarfile

    createDirectoryIfMissing True path -- create workspace in temp directory
    createDirectoryIfMissing True $ dropFileName tarfile -- make sure tar file can be created

    trace <- mkTrace (return cycles) thunk

    writeFile (path </> name <.> "shallow") $ unlines $ genShallow trace
    writeFile (path </> name <.> "info") $ unlines $ genInfo trace

    rc <- reifyCircuit c
    mkTestbench name path rc

    writeFile (path </> "run") $ unlines
        ["#!/bin/bash"
        ,"LM_LICENSE_FILE=1800@carl.ittc.ku.edu:1717@carl.ittc.ku.edu"
        ,"export LM_LICENSE_FILE"
        ,"echo \"Simulating " ++ name ++ "...\""
        ,"/tools/modelsim/linux/6.3c/modeltech/bin/vsim -c -do " ++ name ++ ".do"
        ,"echo \"10 lines from the info file...\""
        ,"tail " ++ name ++ ".info"
        ,"echo \"The same 10 lines from the shallow trace...\""
        ,"tail " ++ name ++ ".shallow"
        ,"echo \"Ditto for the deep trace...\""
        ,"tail " ++ name ++ ".deep"
        ,""
        ,"THEDIFF=`diff " ++ name ++ ".shallow " ++ name ++ ".deep`"
        ,""
        ,"if [[ -z \"$THEDIFF\" ]]; then"
        ,"    echo \"Shallow/Deep Traces Are The Same\""
        ,"    exit 0"
        ,"else"
        ,"    echo \"Warning: Differences Below:\""
        ,"    echo \"$THEDIFF\""
        ,"    exit 1"
        ,"fi"
        ]

    system $ "chmod +x " ++ path </> "run"

    system $ unwords ["tar -czf", tarfile, path]

    -- path better not contain symlinks!!! (or this will follow them)
    removeDirectoryRecursive path

    return ()
