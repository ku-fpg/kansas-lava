{-# LANGUAGE FlexibleInstances, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts, Rank2Types, ExistentialQuantification, TypeFamilies #-}
module Language.KansasLava.Test.Probes where

import System.IO.Unsafe

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Dynamic

import Data.Bits
import Data.List as L
import Data.Maybe

import Data.Sized.Arith as A
import Data.Sized.Matrix as M
import Data.Sized.Sampled as S

import Data.Sized.Signed as S
import Data.Sized.Unsigned as U

import Debug.Trace

import Language.KansasLava.Circuit
import Language.KansasLava.Comb
import Language.KansasLava.Dot
import Language.KansasLava.Entity
import Language.KansasLava.Netlist
import Language.KansasLava.Probes
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Utils
import Language.KansasLava.VHDL.Testbench
import Language.KansasLava.VHDL.VCD
import Language.KansasLava.Wire

import Language.Netlist.GenVHDL

import System.Cmd
import System.Directory
import System.Environment
import qualified System.FilePath.Posix as FP
import System.IO

import qualified System.Posix.Env as Posix

import Text.PrettyPrint(render)

opts = Opts { username = "afarmer", host = "rome" }

numberOfCycles :: Int
numberOfCycles = 100

dumpDir = "examine/"

halfAdder a b = (sum,carry)
  where sum = xor2 a b
        carry = and2 a b

fullAdder a b cin = (sum,cout)
  where (s1,c1) = probe "ha1" halfAdder a b
        (sum,c2) = probe "ha2" halfAdder cin s1
        cout = xor2 c1 c2

-- Run through the list of nodes that have probes on them and generate a list of
-- subcircuits that we can build testbenches for. This depends on the behavior of
-- List.intersect (elements come out in same order as they appear in first list).
genRCs probes rc = go bfsProbes []
    where bfsProbes = intersect bfsOrder probelist
          bfsOrder = bfs (sinkNames rc) rc
          probelist = map fst probes
          go [] _ = []
          go (n:ns) seen = [ (n, rc { theCircuit = newGraph -- trace ("newGraph: " ++ (unlines $ map show newGraph)) newGraph
                                    , theSrcs = newPads -- trace ("newPads: " ++ show newPads) newPads
                                    , theSinks = newSinks -- trace ("newSinks: " ++ show newSinks) newSinks
                				    })
                           | not $ prefix `elem` seen
                           ] ++ (go ns (prefix:seen))
              where
                  newSinks = case lookup n (theCircuit rc) of
                                Just (Entity _ outs _ _) -> [(Var $ "o" ++ show oname, ty, Port nm n)
                                                            | (oname, (nm, ty)) <- zip [0..] outs ]
                                Nothing -> error $ "mkSink failed on " ++ show n

                  prefix = fst $ splitWith '_' $ last $ fromJust $ lookup n probes

                  subtree = bfs' (\c -> (c == n) || (not (elem c $ map fst probefamily))) [n] rc

                  probefamily = [ (node, pname) | (node, pnames) <- probes, pname <- pnames, prefix `isPrefixOf` pname ]

                  subGraph = filter ((`elem` subtree) . fst) $ theCircuit rc

                  -- note, sorting by probe name so we can assign inputs in correct order in newLeaves
                  leafNodes = sortBy (\(pn1,_,_) (pn2,_,_) -> compare pn1 pn2)
                                     [ (pname, node, entity) | (node, pname) <- probefamily, node /= n, Just entity <- [lookup node subGraph] ]

                  newLeaves = [ (ch,Entity (Name "probe" ("i" ++ show i)) outs nins attrs)
                              | (i, (_, ch, Entity _ outs ins attrs)) <- zip [0..] leafNodes
                              , let nins = [ (Var $ "i" ++ show x, ty, Pad $ Var $ "i" ++ show i) | (x, (_,ty)) <- zip [0..] outs ]
                              ]

                  newGraph = newLeaves ++ (filter (not . (`elem` (map fst newLeaves)) . fst) subGraph)

                  newPads = [ (v,ty) | (_, Entity _ _ ins _) <- newLeaves
                                     , (_, ty, Pad v) <- ins ]

-- Seems like this should also exist in the Prelude
splitWith :: Char -> String -> (String,String)
splitWith c s = go s []
    where go [] acc = (reverse acc,[])
          go (i:inp) acc | i == c = (reverse acc,inp)
                         | otherwise = go inp (i:acc)

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

testCircuit' :: (Ports a, Probe a, Ports b) => [String] -> String -> a -> (a -> b) -> IO ()
testCircuit' enabled name f apply
    | null enabled || name `elem` enabled = do
        let probed = probe name f

        plist <- probeCircuit $ apply probed

        let probeData = probesFor name plist

        putStrLn $ "Truth table for " ++ name
        putStrLn $ "======="
        putStrLn $ probesTT probeData

        putStrLn $ "Generating Testbench for " ++ name
        putStrLn $ "======="
        let ropts = [OptimizeReify]

        rc <- reifyCircuit ropts probed

        vcdCircuit name 10 $ apply probed

        let sinks = sinkNames rc
            pvs = filter (not . null . snd) $ map ((\x -> (x, probeValues x rc)) . fst) $ theCircuit rc
            subRCs = genRCs pnames rc
            pnames = [(n,names :: [String]) | (n,vs) <- pvs, let names = [ nm | ProbeValue nm _ <- vs ]]

        mapM_ (mkTest ropts plist (dumpDir ++ name ++ "/") pnames) subRCs

        return ()
    -- Hack to speed up the generation of our tests
    | otherwise = return ()

data Opts = Opts { username :: String
                 , host :: String
                 }

mkTest ropts plist base names (n,rc) = do
    -- So they can see what the subcircuit looks like.
    print rc

    let name = fst $ splitWith '_' $ last $ fromJust $ lookup n names

    -- Glad to see Haskell's directory handling libraries suck as much
    -- as every other language out there. Although mostly this is complicated
    -- by the fact that user directories are mounted differently on rome
    -- than on a local machine.
    home <- getHomeDirectory   -- so we can find stuff on rome
    pwd <- getCurrentDirectory -- since our examine folder is here

    -- this mess to cut the /nfs off the front of local paths
    let pwdsp = filter (\dir -> dir /= "/") $ FP.splitDirectories pwd
        remotePath = FP.joinPath $ dropWhile (\dir -> dir == "nfs") pwdsp
        path = base ++ name ++ "/"
        fp = "/" ++ remotePath ++ "/" ++ path

    mod <- netlistCircuit' ropts [] name rc

    let vhdl = render $ genVHDL mod

    (inputs,outputs,sequentials) <- ports' ropts rc
    waves <- genProbes' name rc

    let probeData = probesFor name plist

    mkInputs name path numberOfCycles probeData fp
    mkTestbench' name base vhdl (inputs,outputs,sequentials) waves
    writeDotCircuit' (path ++ name ++ ".dot") rc

    system $ "ssh " ++ username opts ++ "@" ++ host opts ++ " " ++ fp ++ "test.sh"

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
          ,"else"
          ,"    echo \"Warning: Differences Below:\""
          ,"    echo \"$THEDIFF\""
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

-- surely this exists in the prelude?
mergeWith :: (a -> a -> a) -> [[a]] -> [a]
mergeWith fn probes = go probes []
    where go (p:ps) []  = go ps p
          go (p:ps) acc = go ps $ L.zipWith fn acc p
          go []     acc = acc

-- stab at truth tables
probeTT :: (Probe a, Ports b) => a -> (a -> b) -> IO [[String]]
probeTT f apply = do
        plist <- probeCircuit $ apply $ probe "tt" f
        return [valsXStream xs | (_, (ProbeValue _ xs)) <- probesFor "tt" plist]

asciiTT :: String -> Int -> [[String]] -> IO ()
asciiTT name entries tt = do
    putStrLn $ "Truth table for " ++ name
    putStrLn $ "======="
    putStrLn $ unlines
             $ take entries
             $ mergeWith (\x y -> x ++ " | " ++ y) tt

probesTT probes = unlines
                $ take numberOfCycles
                $ mergeWith (\x y -> x ++ " | " ++ y)
                $ map (\(_, (ProbeValue _ xs)) -> valsXStream xs) probes

muxWaveform = do
    let expand n = concatMap (replicate n)
        allBools = [Just False, Just True, Nothing]
    tt <- probeTT (mux2 :: Seq Bool -> (Seq Bool, Seq Bool) -> Seq Bool)
                  (\f -> f (toSeq' $ cycle $ expand 9 allBools)
                           (toSeq' $ cycle $ expand 3 allBools,
                            toSeq' $ cycle allBools))

    asciiTT "mux2" 27 tt

showTruthTable :: (TruthTable a) => String -> a -> IO ()
showTruthTable name fn = asciiTT name (L.length $ head tt) tt
    where tt = truthTable fn

truthTable :: (TruthTable a) => a -> [[String]]
truthTable fn = L.transpose [ l | ls <- genTT fn, l <- asciiTTV ls ]

asciiTTV :: TTV -> [[String]]
asciiTTV (ArgValue nm rest) = [ nm : rs | rss <- map asciiTTV rest, rs <- rss ]
asciiTTV (ResValue str)      = [[ str ]]

class TruthTable a where
    genTT :: a -> [TTV]

data TTV = ArgValue String [TTV]
         | ResValue String
    deriving Show

instance (RepWire a) => TruthTable (Comb a) where
    genTT c = [ ResValue $ showRepWire (undefined :: a) (combValue c) ]

instance (Enum (WIDTH w), Size (WIDTH w), RepWire w, TruthTable b) => TruthTable (Comb w -> b) where
    genTT fn = [ ArgValue (showRepWire (undefined :: w) a)
                           (genTT (fn (shallowComb a)))
               | a <- map optX $ args ++ [Nothing] ]
        where
            args :: [Maybe w]
            args = [ toWireRep rep | rep <- allWireReps ]

{- below is an experiment ... it takes a function over Combs
-- and gives back a list of Seqs which contain every possible
-- input possible to the Comb. The problem, as you can see,
-- is how I had to box up the Seqs, making them impossible to
-- use for application (I think).
data Lifter = forall w . (Show w, RepWire w) => Lift (Seq w)
            | forall w x . (Show w, RepWire w, Show x, RepWire x) => LiftTuple (Seq w, Seq x)

instance Show Lifter where
    show (Lift seq) = show seq
    show (LiftTuple t) = show t

class ProbeTT a where
    discover :: a -> [Lifter] -> [Lifter]

instance ProbeTT (Comb w) where
    discover _ args = args

instance (Show w, RepWire w, ProbeTT b) => ProbeTT (Comb w -> b) where
    discover fn as = discover (fn (shallowComb (optX $ head args))) (as ++ [Lift $ toSeq' args])
        where
            args :: [Maybe w]
            args = [ toWireRep rep | rep <- allWireReps ] ++ [Nothing]

instance (Show w, RepWire w, Show x, RepWire x, ProbeTT b) => ProbeTT ((Comb w, Comb x) -> b) where
    discover fn as = discover (fn ((shallowComb (optX $ head args1)),
                                   (shallowComb (optX $ head args2))))
                              (as ++ [LiftTuple $ (toSeq' args1, toSeq' args2)])
        where
            args1 :: [Maybe w]
            args1 = [ toWireRep rep | rep <- allWireReps ] ++ [Nothing]
            args2 :: [Maybe x]
            args2 = [ toWireRep rep | rep <- allWireReps ] ++ [Nothing]
-}
