{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Language.KansasLava.Testing.Thunk
    ( mkTrace
    , Thunk(..)
    , runShallow
    , runDeep
    , mkTestBench
    , runTestBench
    , exposeProbes
    , exposeProbesIO) where

import Language.KansasLava hiding (head)

import Language.KansasLava.Testing.Bench
import Language.KansasLava.Testing.Trace

import Control.Monad

import Data.List

import System.Directory
import System.FilePath.Posix

data Thunk b = forall a. (Probe a, Probe b) => Thunk a (a -> b)

instance (Show b) => Show (Thunk b) where
   show thunk = show $ runShallow thunk

runShallow :: Thunk a -> a
runShallow (Thunk c k) = k c

mkTrace :: Maybe Int -- ^ Nothing means infinite trace, Just x sets trace length to x cycles.
        -> Fabric () -- ^ The fabric we are tracing
        -> [(String,Pad)]
        -> IO Trace
mkTrace c fabric input = do
    (trace, _) <- mkTraceCM c fabric input (return)
    return trace

mkTestBench :: FilePath -- ^ Directory where we should place testbench files. Will be created if it doesn't exist.
            -> Int      -- ^ Generate inputs for this many cycles.
            -> (Circuit -> IO Circuit)  -- ^ any operations on the circuit before VHDL generation
            -> Fabric ()
            -> [(String,Pad)]
            -> IO Trace
mkTestBench path cycles circuitMod fabric input = do
    let name = last $ splitPath path

    createDirectoryIfMissing True path

    (trace, rc) <- mkTraceCM (return cycles) fabric input circuitMod

    writeFile (path </> name <.> "shallow") $ unlines $ genShallow trace
    writeFile (path </> name <.> "info") $ unlines $ genInfo trace
    writeFile (path </> name <.> "sig") $ show $ traceSignature trace

    mkTestbench name path rc

    return trace

-- | Combination of recordThunk and runTestBench, working in the temp directory.
runDeep :: String              -- ^ User significant name for the Thunk
        -> Int                 -- ^ Number of cycles to simulate.
        -> Fabric ()
        -> (Circuit -> IO Circuit) -- ^ any operations on the circuit before VHDL generation
        -> (FilePath -> IO ()) -- ^ Invocation function, given a path to the testbench and charged with actually executing the test. Can assume path exists.
        -> IO ()
runDeep name cycles fabric circuitMod invoker = do
    tmp <- getTemporaryDirectory

    let target = tmp </> name

    _ <- mkTestBench target cycles circuitMod fabric []
    runTestBench target invoker

    -- there better not be any symlinks in here!
    removeDirectoryRecursive target

-- | Run a generated testbench.
-- eventually runTestBench :: FilePath -> (FilePath -> IO ()) -> IO Trace
runTestBench :: FilePath            -- ^ Path to testbench we want to run.
             -> (FilePath -> IO ()) -- ^ Invocation function, given a path to the testbench and
                                    --   charged with actually executing the test. Can assume path exists.
             -> IO ()
runTestBench path invoker = do
    exists <- doesDirectoryExist path

    if exists
        then invoker path
        else putStrLn $ "runTestBench: " ++ path ++ " does not exist!"

    -- eventually we will read the deep file here and return it as a trace

exposeProbesIO :: [String] -> Circuit -> IO Circuit
exposeProbesIO names = return . (exposeProbes names) -- seems like return . exposeProbes should work

-- This doesn't need IO, so we keep the flexibility here.
exposeProbes :: [String] -> Circuit -> Circuit
exposeProbes names rc = rc { theSinks = oldSinks ++ newSinks }
    where oldSinks = theSinks rc
          n = succ $ head $ sortBy (\x y -> compare y x) $ [ i | (OVar i _, _, _) <- oldSinks ]
          allProbes = sort [ (pname, nm, outs)
                        | (nm, Entity (TraceVal pnames _) outs _) <- theCircuit rc
                        , pname <- pnames ]
          exposed = nub [ (p, oty, Port onm nm)
                        | (p@(OVar _ pname), nm, outs) <- allProbes
                        , or [ name `isPrefixOf` pname | name <- names ]
                        , (onm,oty) <- outs ]
          showPNames x pname = show pname ++ "_" ++ show x

          newSinks = [ (OVar i $ showPNames i pname, ty, d) | (i,(pname, ty,d@(Port _ _))) <- zip [n..] exposed ]

mkTraceCM :: Maybe Int -- ^ Nothing means infinite trace, Just x sets trace length to x cycles.
          -> Fabric ()
          -> [(String, Pad)]
          -> (Circuit -> IO Circuit) -- Circuit Mod
          -> IO (Trace, Circuit)
mkTraceCM c fabric input circuitMod = do
    rc <- (reifyFabric >=> mergeProbesIO >=> circuitMod) fabric

    let output = runFabric fabric input
        withInput  = foldr (\(i,(nm,p)) t@(Trace _ ins _ _)  -> t { inputs  = (OVar i nm, padToTraceStream p):ins }) emptyT $ zip [0..] input
        withOutput = foldr (\(i,(nm,p)) t@(Trace _ _ outs _) -> t { outputs = (OVar i nm, padToTraceStream p):outs }) withInput $ zip [0..] output
        emptyT = Trace { len = c, inputs = [], outputs = [], probes = [] }

    return (addProbes rc withOutput, rc)

addProbes :: Circuit -> Trace -> Trace
addProbes rc t = t { probes = ps }
    where pdata = [ (nid,k,v) | (nid,Entity (TraceVal ks v) _ _) <- theCircuit rc, k <- ks ]
          ps = [ (OVar nid nm, strm) | (nid, OVar _ nm, strm) <- pdata ]

{-
mkTraceCM :: Maybe Int -- ^ Nothing means infinite trace, Just x sets trace length to x cycles.
--          -> Thunk a   -- ^ The thunk we are executing.
          -> Fabric ()
          -> (Circuit -> IO Circuit) -- Circuit Mod
          -> IO (Trace, Circuit)
mkTraceCM c fabric circuitMod = do -- (Thunk circuit k) circuitMod = do
--     let probed = probeWholeCircuit circuit

    rc <- (reifyFabric >=> mergeProbesIO >=> circuitMod) fabric -- probed

    -- we can't apply circuitMods to applied circuit, because our pads disappear!
    -- TODO: figure out why we can't call mergeProbes on this
--     rcWithData <- reifyCircuit $ k probed
    let rcWithData = rc

    let pdata = [ (nid,k',v) | (nid,Entity (TraceVal ks v) _ _) <- theCircuit rcWithData , k' <- ks ]
        outNum = maximum [ i | (_, WholeCircuit _ i _, _) <- pdata ]
        uniqueWCs = map head
                  $ groupBy grouping
                  $ sortBy (\ (_, l, _) (_, r, _) -> compare l r)
                    [ wc | wc@(_, WholeCircuit _ _ _, _) <- pdata ]
        ins = M.fromList [ (WholeCircuit s i nid, strm) | (nid, WholeCircuit s i _, strm) <- uniqueWCs, i /= outNum ]
        outs = M.fromList [ (WholeCircuit s i nid, strm) | (nid, WholeCircuit s i _, strm) <- uniqueWCs, i == outNum ]
        ps = M.fromList [ (Probe nm i nid, strm) | (nid, Probe nm i _, strm) <- pdata ]

    return (Trace { len = c, inputs = ins, outputs = outs, probes = ps }, rc)
  where grouping (_, WholeCircuit s1 i1 _, _) (_, WholeCircuit s2 i2 _, _) = s1 == s2 && i1 == i2
        grouping _ _ = False

-- | Make a Trace from a Thunk
mkTrace :: (Probe a)
        => Maybe Int -- ^ Nothing means infinite trace, Just x sets trace length to x cycles.
        -> Thunk a   -- ^ The thunk we are executing.
        -> IO Trace
mkTrace c thunk = do
    (trace, _) <- mkTraceCM c thunk (return)
    return trace

-- | Make a Thunk from a Trace and a (non-reified) lava circuit.
mkThunk :: forall a b. (Probe a, Rep b)
        => Trace -- ^ A (possibly partial) trace to supply inputs.
        -> a     -- ^ The lava circuit.
        -> Thunk (Seq b)
mkThunk trace circuit = Thunk circuit (\c -> shallowSeq $ fromTrace $ run c trace)

-- | Like mkTrace, but also generates a VHDL testbench and input files.
-- | Since we must shallowly run the thunk to generate the input, this returns a
-- | trace as a convenience.
recordThunk :: (Probe b)
            => FilePath -- ^ Directory where we should place testbench files. Will be created if it doesn't exist.
            -> Int      -- ^ Generate inputs for this many cycles.
            -> (Circuit -> IO Circuit)  -- ^ any operations on the circuit before VHDL generation
            -> Thunk b
            -> IO Trace
recordThunk path cycles circuitMod thunk@(Thunk _ _) = do
    let name = last $ splitPath path

    createDirectoryIfMissing True path

    (trace, rc) <- mkTraceCM (return cycles) thunk circuitMod

    writeFile (path </> name <.> "shallow") $ unlines $ genShallow trace
    writeFile (path </> name <.> "info") $ unlines $ genInfo trace
    writeFile (path </> name <.> "sig") $ show $ traceSignature trace

    mkTestbench name path rc

    return trace
-}
