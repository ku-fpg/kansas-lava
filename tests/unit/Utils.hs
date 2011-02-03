{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Utils where

import Language.KansasLava
import Language.KansasLava.Testing.Thunk
import Language.KansasLava.Testing.Trace

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad

import Data.Default
import Data.List
import Data.Maybe as Maybe
import Data.Ord ( comparing )
import Data.Sized.Ix
import Data.Sized.Unsigned

import System.Cmd
import System.Directory
import System.FilePath as FP
import qualified System.IO.Strict as Strict
import qualified System.Random as R

import Types

-------------------------------------------------------------------------------------

data TestData = Rand Int | Complete

testMe _ Nothing     = True
testMe nm (Just nms) = or [ (n `isInfixOf` nm) | n <- nms ]

neverTestMe nm nms = or [ (n `isInfixOf` nm) | n <- nms ]

verbose vlvl name n m | vlvl >= n = putStrLn (name ++ " :" ++ take n (repeat ' ') ++ m)
                      | otherwise           = return ()

fileReporter path nm res = do
    createDirectoryIfMissing True (path </> nm)
    writeFile (path </> nm </> "result") $ show res

-------------------------------------------------------------------------------------

-- Given a circuit that returns an a, and the expected results,
-- do some tests for sanity.

data TestSeq = TestSeq
        (forall a . (Rep a, Show a, Eq a) => String -> Int -> Thunk (Seq a) -> Seq a -> IO ())
        (forall a. Gen a -> [a])

testSeq :: (Rep a, Show a, Eq a)
        => Options                  -- Options
        -> String                   -- Test Name
        -> Int                      -- Number of Cycles
        -> Thunk (Seq a)            -- Circuit and Input
        -> Seq a                    -- Expected Result
        -> IO ()
testSeq opts name count thunk expected
   | testMe name (testOnly opts) && not (neverTestMe name (testNever opts)) = do
        let verb = verbose (verboseOpt opts) name
            path = (simPath opts) </> name
            report = fileReporter $ simPath opts

        verb 2 $ "testing(" ++ show count ++ ")"
        verb 9 $ show ("expected",expected)

        -- First run the shallow
        let shallow = runShallow thunk
        verb 9 $ show ("shallow",shallow)
        if cmpSeqRep count expected shallow
          then do verb 3 $ "shallow passed"
                  if genSim opts
                    then do createDirectoryIfMissing True path

                            -- get permuted/unpermuted list of sims for which we generate testbenches
                            let sims = [ (modname, (recordThunk (path </> modname) count (snd mod) thunk))
                                       | mod <- if permuteMods opts
                                                    then map (foldr (\(nm,m) (nms,ms) -> (nm </> nms, m >=> ms)) ("unmodified", (return)))
                                                           $ concatMap permutations
                                                           $ subsequences
                                                           $ simMods opts
                                                    else simMods opts
                                       , let modname = fst mod
                                       ]

                            -- generate each testbench, report any failures
                            ts <- sequence [ do vrb 2 $ "generating simulation"
                                                E.catch (Just <$> action)
                                                        (\e -> do vrb 3 "vhdl generation failed"
                                                                  vrb 4 $ show (e :: E.SomeException)
                                                                  rep $ CodeGenFail (show (e :: E.SomeException))
                                                                  return Nothing)
                                           | (modname, action) <- sims
                                           , let rep = report (name </> modname)
                                           , let vrb = verbose (verboseOpt opts) (name </> modname)
                                           ]

                            -- for successfully generated testbenches, add some files
                            sequence_ [ do writeFile (path </> modname </> "Makefile") $ localMake (name </> modname)
                                           copyLavaPrelude opts (path </> modname)
                                           writeFile (path </> modname </> "options") $ show opts
                                           vrb 9 $ show ("trace",fromJust t)
                                      | (modname, t) <- zip (map fst sims) ts
                                      , isJust t
                                      , let vrb = verbose (verboseOpt opts) (name </> modname)
                                      ]

                            return ()
                    else report name ShallowPass
          else do verb 1 $ "shallow FAILED"
                  trace <- mkTrace (return count) thunk
                  report name $ ShallowFail trace (toTrace $ seqValue expected)

                                         | otherwise = return ()

simCompare :: FilePath -> (Result -> IO ()) -> (Int -> String -> IO ()) -> IO ()
simCompare path report verb = do
    let localname = last $ splitPath path

    ran <- doesFileExist $ path </> "transcript"
    if ran
        then do log <- Strict.readFile (path </> "transcript")
                success <- doesFileExist $ path </> localname <.> "deep"
                if success
                    then do shallow <- lines <$> Strict.readFile (path </> localname <.> "shallow")
                            deep    <- lines <$> Strict.readFile (path </> localname <.> "deep")
                            sig     <- read  <$> Strict.readFile (path </> localname <.> "sig")

                            let t1 = asciiToTrace shallow sig
                                t2 = asciiToTrace deep sig
                            if cmpTraceIO t1 t2
                                then do verb 3 "simulation passed"
                                        report $ Pass t1 t2 log
                                else do verb 3 "simulation failed"
                                        verb 4 $ show ("shallow",t1)
                                        verb 4 $ show ("deep",t2)
                                        report $ CompareFail t1 t2 log

                    else do verb 3 "VHDL compilation failed"
                            verb 4 log
                            report $ CompileFail log
        else verb 1 "Simulation hasn't been run, transcript file missing."

postSimulation :: FilePath -> IO ()
postSimulation spath = go "" spath
    where go :: String -> FilePath -> IO ()
          go name path = do
            isSimDir <- doesFileExist $ path </> "transcript"
            if isSimDir
                then simCompare path (fileReporter spath name) (verbose 9 name)
                else return ()

            contents <- getDirectoryContents path
            subdirs <- filterM (\(_,f) -> doesDirectoryExist f)
                               [ (name </> f, path </> f)
                               | f <- contents
                               , f /= "."
                               , f /= ".." ]

            mapM_ (uncurry go) subdirs

prepareSimDirectory :: Options -> IO ()
prepareSimDirectory opts = do
    let path = simPath opts
    putStrLn $ "preparing simulation directory: ./" ++ path
    pwd <- getCurrentDirectory

    -- Calling out to rm -rf is safer than Haskell's removeDirectoryRecursive, which
    -- follows symlinks. However, this still seems dangerous to put here,
    -- so we do a bit of checking to make sure we can't delete anything
    -- outside the present working directory.
    ok <- doesDirectoryExist $ pwd </> path
    if ok && not (isInfixOf ".." path)
        then do system $ "rm -rf " ++ path
                return ()
        else return ()

    createDirectoryIfMissing True path

    writeFile (path </> "runsims") $ unlines testRunner
    system $ "chmod +x " ++ path </> "runsims"

    return ()

testRunner :: [String]
testRunner = ["#!/bin/bash"
             ,"curdir=`pwd`"
             ,"find . -iname \"*.do\" | while read f"
             ,"do"
             ,"    echo \"Simulating: $f\""
             ,"    p=`dirname \"$f\"`"
             ,"    b=`basename \"$f\"`"
             ,"    cd $p"
             ,"    res=`vsim -c -do $b`"
             ,"    echo $res >> sim.log"
             ,"    cd $curdir"
             ,"done"
             ]

localMake :: String -> String
localMake relativePath = unlines
    ["vsim:"
    ,"\tvsim -c -do " ++ name ++ ".do"
    ,""
    ,"diff:"
    ,"\t" ++ dots </> "dist/build/kansas-lava-tracediff/kansas-lava-tracediff " ++ name ++ ".shallow " ++ name ++ ".deep " ++ name ++ ".sig"
    ,"\tgtkwave diff.vcd"
    ,""
    ,"vcd:"
    ,"\twlf2vcd vsim.wlf > " ++ name ++ ".vcd"
    ,""
    ,"view: vcd"
    ,"\tgtkwave " ++ name ++ ".vcd"
    ]
    where dots = joinPath $ replicate l ".."
          l = 3 + (length $ splitPath relativePath)
          name = last $ splitPath relativePath


preludeFile = "Lava.vhd"

copyLavaPrelude :: Options -> FilePath -> IO ()
copyLavaPrelude opts dest = do
        prel <- Strict.readFile (preludePath opts </> preludeFile)
        writeFile (dest </> preludeFile) prel

-------------------------------------------------------------------------------------

-- Not really random, but good enough for basic testing.
unsort :: [x] -> [x]
unsort es = map snd . sortBy (comparing fst) $ zip rs es
  where rs = R.randoms stdGen :: [Integer]
        stdGen = R.mkStdGen 0

-------------------------------------------------------------------------------------

u4s0 :: [U4]
u4s0 = cycle [0..15]

u4s1 :: [U4]
u4s1= cycle $ unsort $ take (16 * 16) (cycle [0..15])

data Gen a = Gen Integer (Integer -> Maybe a)

arbitrary :: forall w . (Rep w) => Gen w
arbitrary = Gen sz integer2rep
  where
        sz = 2^fromIntegral (repWidth (Witness :: Witness w))
        integer2rep :: Integer -> Maybe w
        integer2rep v = unX
                $ fromRep
                $ RepValue
                $ take (repWidth (Witness :: Witness w))
                $ map WireVal
                $ map odd
                $ iterate (`div` 2)
                $ fromIntegral v

loop :: Integer -> Gen w -> Gen w
loop n (Gen sz f) = Gen (sz * n) (\ i -> f $ i `mod` sz)


-- | makes sure all sequences of two specific elements happen.
-- Random messes this up a bit, but its still an approximation.
dubSeq :: Gen w -> Gen w
dubSeq g = ((\ a b c -> if a then b else c) <$> arbitrary)
        <*> g
        <*> g


instance Functor Gen where
        fmap g (Gen n f) = Gen n (\i -> do r <- f i
                                           return $ g r)

instance Applicative Gen where
        pure a = Gen 1 (const $ return a)
        (Gen n1 f1) <*> (Gen n2 f2) = Gen (n1 * n2) (\ i -> do r1 <- f1 (i `mod` n1)
                                                               r2 <- f2 (i `div` n1)
                                                               return $ r1 r2)

-- get *all* elements from a Gen
genToList :: Gen a -> [a]
genToList (Gen n f) = Maybe.catMaybes $ fmap f [0..(n-1)]

-- get some (random) elements from a Gen
-- If it is small, then just output all the values.
genToRandom :: Gen a -> [a]
genToRandom (Gen n f)
        | n <= 100 = unsort $ genToList (Gen n f)
        | otherwise = take (fromIntegral (min n largeNumber)) $ Maybe.catMaybes $ fmap f $ R.randomRs (0,n) (R.mkStdGen 0)


largeNumber = 10000
