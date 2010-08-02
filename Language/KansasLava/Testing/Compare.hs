{-# LANGUAGE FlexibleInstances, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts, Rank2Types, ExistentialQuantification, TypeFamilies #-}
module Language.KansasLava.Testing.Compare (DebugOpts(..), def, testCircuit) where

import System.IO.Unsafe

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Dynamic

import Data.Bits
import Data.Default
import Data.List as L
import Data.Maybe

import Data.Reify.Graph

import Data.Sized.Arith as A
import Data.Sized.Matrix as M
import Data.Sized.Sampled as S

import Data.Sized.Signed as S
import Data.Sized.Unsigned as U

import Debug.Trace

import Language.KansasLava.Circuit
import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Netlist
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Testing.Bench
import Language.KansasLava.Testing.Output.Dot
import Language.KansasLava.Testing.Output.VCD
import Language.KansasLava.Testing.Probes
import Language.KansasLava.Testing.TruthTable
import Language.KansasLava.Testing.Utils
import Language.KansasLava.Utils
import Language.KansasLava.Wire

import Language.Netlist.GenVHDL

import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import qualified System.FilePath.Posix as FP
import System.IO

import qualified System.Posix.Env as Posix

import Text.PrettyPrint(render)

data DebugOpts = Opts { username :: String -- ^ Username on simulation machine (see host).
                      , host :: String -- ^ SSH host that will run vsim.
                      , cycles :: Int -- ^ Number of cycles to simulate.
                      , baseDir :: String -- ^ Base directory for output files.
                      , reifyOptions :: [ReifyOptions] -- ^ Reification options to use with tests.
                      , enabled :: [String] -- ^ List of enabled tests. [] -> run all.
                      }
    deriving (Eq, Show)

instance Default DebugOpts where
    def = Opts { username = ""
               , host = "rome"
               , cycles = 100
               , baseDir = "examine/"
               , reifyOptions = []
               , enabled = []
               }

testCircuit :: (Ports a, Probe a, Ports b) => DebugOpts -> String -> a -> (a -> b) -> IO ()
testCircuit opts name circuit apply
    | null (enabled opts) || name `elem` (enabled opts) = do
        let probed = probe name circuit

        rc <- reifyCircuit (reifyOptions opts) probed
        pdata <- probeCircuit $ apply probed

        print rc

        algDebug opts name rc pdata "" $ probeForest rc
    | otherwise = return ()

algDebug :: DebugOpts
         -> String
         -> ReifiedCircuit
         -> [(String, ProbeValue)]
         -> String
         -> [ProbeTree]
         -> IO ()
algDebug opts name rc pdata parent forest = go parent forest
    where go []     [] = putStrLn "Embeddings act the same."
          go parent [] = putStrLn $ parent ++ " failed, but all its children (if any) succeeded."
          go parent ((Node nm _ ch):ts) = do
                code <- testSubcircuit opts name rc pdata nm
                case code of
                    ExitSuccess -> go parent ts -- move on to a sibling
                    ExitFailure _ -> go nm ch   -- start checking children

testSubcircuit opts wholeName rc pdata name = do
    let sub = extractSubcircuit name rc

    -- Glad to see Haskell's directory handling libraries suck as much
    -- as every other language out there. Although mostly this is complicated
    -- by the fact that user directories are mounted differently on rome
    -- than on a local machine.
    pwd <- getCurrentDirectory -- since our examine folder is here

    -- this mess to cut the /nfs off the front of local paths
    let pwdsp = filter (\dir -> dir /= "/") $ FP.splitDirectories pwd
        remotePath = FP.joinPath $ dropWhile (\dir -> dir == "nfs") pwdsp
        base = (baseDir opts) ++ wholeName ++ "/"
        path = base ++ name ++ "/"
        fp = "/" ++ remotePath ++ "/" ++ path
        ropts = []

    mod <- netlistCircuit' ropts [] name sub

    let vhdl = render $ genVHDL mod

    ports <- ports' ropts sub
    waves <- genProbes' name sub

    let probeData = probesFor name pdata
        dotName = path ++ name ++ ".dot"
        user = case username def of
                "" -> ""
                uname -> uname ++ "@"

    mkInputs name path (cycles opts) probeData fp
    mkTestbench' name base vhdl ports waves
    writeDotCircuit' dotName sub
    system $ "dot -Tpng " ++ dotName ++ " > " ++ path ++ name ++ ".png"

    code <- system $ "ssh " ++ user ++ host def ++ " " ++ fp ++ "test.sh"

    return code

extractSubcircuit :: String -> ReifiedCircuit -> ReifiedCircuit
extractSubcircuit pname rc = extract root leaves rc
    where (root:leaves) = map fst $ sortBy (\(_,n1) (_,n2) -> compare n2 n1)
                        $ [ (node, name) | (node, Entity _ _ _ attrs) <- theCircuit rc
                                         , ("simValue", val) <- attrs
                                         , Just (ProbeValue name _) <- [fromDynamic val]
                                         , pname `isPrefixOf` name ]

extract :: Unique -> [Unique] -> ReifiedCircuit -> ReifiedCircuit
extract n ls rc = ReifiedCircuit { theCircuit = newGraph
                                 , theSrcs = newPads
                                 , theSinks = newSinks
                                 }
    where subtree = bfs' (\id -> (id == n) || (id `notElem` ls)) [n] rc
          subGraph = filter ((`elem` subtree) . fst) $ theCircuit rc

          newLeaves = [ (id, Entity (Name "probe" ("i" ++ show i)) outs nins attrs)
                      | (i, (id, Entity _ outs ins attrs)) <- zip [0..] (lookupAll (reverse ls) $ theCircuit rc)
                      , let nins = [ (Var $ "i" ++ show x, ty, Pad $ Var $ "i" ++ show i) | (x, (_,ty)) <- zip [0..] outs ]
                      ]

          newGraph = newLeaves ++ (filter ((`notElem` (map fst newLeaves)) . fst) subGraph)

          newPads = [ (v,ty) | (_, Entity _ _ ins _) <- newLeaves
                             , (_, ty, Pad v) <- ins ]

          newSinks = [ (Var $ "o" ++ show oname, ty, Port nm n)
                     | Just (Entity _ outs _ _) <- [lookup n $ theCircuit rc]
                     , (oname, (nm, ty)) <- zip [0..] outs ]

data ProbeTree = Node String Unique [ProbeTree]
    deriving (Eq, Show)

probeForest :: ReifiedCircuit -> [ProbeTree]
probeForest rc = [ go (last $ probesOn n circuit) n | n <- initial ]
    where circuit = theCircuit rc
          initial = findProbes bfsProbes (sinkNames rc) rc
          bfsOrder = lookupAll (bfs (sinkNames rc) rc) circuit
          bfsProbes = [ id | (id, Entity _ _ _ attrs) <- bfsOrder
                           , "simValue" `elem` (map fst attrs)
                      ]
          go fam n = Node fam
                          n
                          [ go (last ps) x
                          | x <- findProbes (bfsProbes \\ [n]) [n] rc
                          , let ps = probesOn x circuit
                          , fam `notElem` ps
                          ]

probesOn n circuit = nub [ fst $ splitWith '_' nm
                         | Just (Entity _ _ _ attrs) <- [lookup n circuit]
                         , ("simValue", val) <- attrs
                         , Just (ProbeValue nm _) <- [fromDynamic val]
                         ]

findProbes pnodes nodes rc = reverse $ go nodes [] []
    where go []     _ l = l
          go (n:ns) v l = go (nub ns') v' l' where v' = n:v
                                                   ns' = filter (not . (`elem` v'))
                                                       $ ns ++ if (n `elem` pnodes) then [] else reverse (children n rc)
                                                   l' = if n `elem` pnodes then n:l else l

-- bfs the circuit from a given set of starting nodes
bfs = bfs' (\_ -> True)
bfs' fn nodes rc  = reverse $ go nodes []
    where go [] acc = acc
          go (n:ns) acc = go ns' acc' where acc' = n:acc
                                            ns' = nub
                                                $ filter (not . (`elem` acc'))
                                                $ ns ++ (case fn n of
                                                            True -> reverse (children n rc)
                                                            False -> [])

sinkNames rc = map (\(_,_,Port _ n) -> n) $ theSinks rc
children name rc = catMaybes $ filter isJust
                   $ map (\(_,_,d) -> case d of
                                        Port _ child -> Just child
                                        _ -> Nothing) ins
    where c = theCircuit rc
          Entity _ _ ins _ = fromJust $ lookup name c

probeValues name rc = [ pv | Just (Entity _ _ _ attrs) <- [lookup name $ theCircuit rc]
                          , ("simValue", val) <- attrs
                          , Just pv@(ProbeValue n v) <- [fromDynamic val]]

testReify :: (Ports a) => String -> a -> IO ()
testReify nm fn = do
    putStrLn $ "Testing " ++ nm ++ " reify"
    putStrLn $ "======="
    debugCircuit [OptimizeReify] fn

mkInputs name path count probes fp = do
    let name_n = name ++ "_0"

    createDirectoryIfMissing True path

    writeFile (path ++ "test.sh")
        $ unlines
        $ ["#!/bin/bash"
          ,"LM_LICENSE_FILE=1800@carl.ittc.ku.edu:1717@carl.ittc.ku.edu"
          ,"export LM_LICENSE_FILE"
          ,"cd " ++ fp
          ,"echo \"Simulating...\""
          ,"/tools/modelsim/linux/6.3c/modeltech/bin/vsim -c -do " ++ name ++ ".do"
          ,"echo \"10 lines from the info file...\""
          ,"tail " ++ name_n ++ ".info"
          ,"echo \"The same 10 lines from the input file...\""
          ,"tail " ++ name ++ ".input"
          ,"echo \"Ditto for the output file...\""
          ,"tail " ++ name ++ ".output"
          ,"THEDIFF=`diff *.input *.output`"
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
    system $ "chmod +x " ++ path ++ "test.sh"

    let bits = map (\(_, (ProbeValue _ xs)) -> bitsXStream xs) probes

    writeFile (path ++ name ++ ".input")
        $ unlines
        $ take count
        $ mergeWith (++)
        $ bits

    -- kind of messy I know
    writeFile (path ++ name_n ++ ".info")
        $ unlines
        $ L.zipWith (\n l -> "(" ++ show n ++ ") " ++ l) [0..]
        $ mergeWith (\x y -> x ++ " -> " ++ y)
        $ L.zipWith (\bs vs -> L.zipWith (\v b -> v ++ "/" ++ b) (take count vs) (take count bs)) bits
        $ map (\(_, (ProbeValue _ xs)) -> valsXStream xs) probes
