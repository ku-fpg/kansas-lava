{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Language.KansasLava.Testing.Thunk (Thunk(..), runShallow, runDeep, mkThunk, mkTrace, mkDeepThunk, runDeepThunk) where

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

runDeep :: (Ports b) => String -> Int -> Thunk b -> (FilePath -> IO ()) -> IO ()
runDeep name cycles thunk invoker = do
    tmp <- getTemporaryDirectory

    let target = tmp </> name

    mkDeepThunk target cycles thunk
    runDeepThunk target invoker

    -- there better not be any symlinks in here!
    removeDirectoryRecursive target

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

runDeepThunk :: FilePath -> (FilePath -> IO ()) -> IO ()
runDeepThunk path invoker = do
    exists <- doesDirectoryExist path

    if exists
        then invoker path
        else putStrLn $ "runDeepThunk: " ++ path ++ " does not exist!"

mkDeepThunk :: (Ports b) => FilePath -> Int -> Thunk b -> IO Trace
mkDeepThunk path cycles thunk@(Thunk c k) = do
    let name = last $ splitPath path

    createDirectoryIfMissing True path

    trace <- mkTrace (return cycles) thunk

    writeFile (path </> name <.> "shallow") $ unlines $ genShallow trace
    writeFile (path </> name <.> "info") $ unlines $ genInfo trace

    rc <- reifyCircuit c
    mkTestbench name path rc

    return trace

