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
import System.FilePath.Posix as FP
import qualified System.Random as R

import Report
-- Our Unit Tests

-------------------------------------------------------------------------------------

data TestData = Rand Int | Complete

data Options = Options
        { genSim      :: Bool                              -- ^ Generate modelsim testbenches for each test?
        , runSim      :: Bool                              -- ^ Run the tests after generation?
        , simCmd      :: FilePath -> String                -- ^ Take path relative to test root and generate command to run sim.
        , simPath     :: FilePath                          -- ^ Path into which we place all our simulation directories.
        , simMods     :: [(String, Circuit -> IO Circuit)] -- ^ List of modifications (like optimizations) to apply to
                                                           --   the circuit before simulation.
        , permuteMods :: Bool                              -- ^ False: Run each mod separately. True: Run all possible
                                                           --   permutations of the mods to see if they affect each other.
        , preludePath :: FilePath                          -- ^ location of the Lava prelude.
        , verboseOpt  :: Int                               -- ^ See verbose table below.
        , testOnly    :: Maybe [String]                    -- ^ Lists of tests to execute. Can match either end. Nothing means all tests.
        , testNever   :: [String]                          -- ^ List of tests to never execute. Can match either end.
        , testData    :: Int                               -- ^ cut off for random testing
        }

instance Show Options where
    show (Options gs rs sc sp sm pm pp vo to tn td) =
        unlines [ "genSim: " ++ show gs
                , "runSim: " ++ show rs
                , "simCmd: " ++ show (sc "/path/to/file.do")
                , "simPath: " ++ show sp
                , "simMods: " ++ show (map fst sm)
                , "permuteMods: " ++ show pm
                , "preludePath: " ++ show pp
                , "verboseOpt: " ++ show vo
                , "testOnly: " ++ show to
                , "testNever: " ++ show tn
                , "testData: " ++ show td ]

{-
instance Read Options where
    readsPrec _ xs = [(Options (read gs) (read rs) (read sc) (read sp) [] (read pm) (read pp) (read vo) (read to) (read tn) (read td),unlines rest)]
        where (ls, rest) = splitAt 11 $ lines xs
              [gs,rs,sc,sp,pm,pp,vo,to,tn,td] = [ tail $ dropWhile (\c -> c /= ' ') $ ls !! i | i <- [0,1,2,3,5,6,7,8,9,10] ]
-}
-------------------------------------------------------------------------------------
-- Verbose table
-- 1: Failures
-- 2: what was run
-- 3: what worked
-- 4: debugging from failures
-- 9: debugging from everything that happened
-------------------------------------------------------------------------------------

instance Default Options where
        def = Options
                { genSim = False
                , runSim = False
                , simCmd = \p -> "cd \"" ++ dropFileName p ++ "\" && vsim -c -do \"" ++ takeFileName p ++ "\""
                , simPath = "sims"
                , simMods = []
                , permuteMods = True
                , preludePath = "../../Prelude/VHDL"
                , verboseOpt = 3
                , testOnly = Nothing
                , testNever = []
                , testData = 1000
                }

testMe _ Nothing     = True
testMe nm (Just nms) = or [ (n `isInfixOf` nm) | n <- nms ]

neverTestMe nm nms = or [ (n `isInfixOf` nm) | n <- nms ]

verbose opt n m | verboseOpt opt >= n = putStrLn m
                | otherwise           = return ()

-------------------------------------------------------------------------------------

-- Given a circuit that returns an a, and the expected results,
-- do some tests for sanity.

data TestSeq = TestSeq
        (forall a . (Rep a, Show a, Eq a) => String -> Int -> Thunk (Seq a) -> Seq a -> IO ())
        (forall a. Gen a -> [a])

testSeq :: (Rep a, Show a, Eq a)
        => Options                  -- Options
        -> (TestCase -> IO ())      -- Called with the result
        -> String                   -- Test Name
        -> Int                      -- Number of Cycles
        -> Thunk (Seq a)            -- Circuit and Input
        -> Seq a                    -- Expected Result
        -> IO ()
testSeq opts r name count thunk expected
   | testMe name (testOnly opts) && not (neverTestMe name (testNever opts)) = do
        let verbBuilder nm = (\n m -> verbose opts n (nm ++ " :" ++ take n (repeat ' ') ++ m))
            verb = verbBuilder name
            path = (simPath opts) </> name
            report = (curry r) name

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
                                           , let rep = (curry r) (name </> modname)
                                           , let vrb = verbBuilder (name </> modname)
                                           ]

                            -- for successfully generated testbenches, run simulations
                            sequence_ [ do writeFile (path </> modname </> "Makefile") $ localMake (name </> modname)
                                           copyLavaPrelude opts (path </> modname)
                                           writeFile (path </> modname </> "options") $ show opts
                                           if runSim opts
                                              then simulate opts (path </> modname) rep vrb
                                              else do vrb 3 "simulation generated"
                                                      rep SimGenerated
                                           vrb 9 $ show ("trace",t)
                                      | (modname, t) <- zip (map fst sims) ts
                                      , isJust t
                                      , let rep = (curry r) (name </> modname)
                                      , let vrb = verbBuilder (name </> modname)
                                      ]

                            return ()
                    else report ShallowPass
          else do verb 1 $ "shallow FAILED"
                  trace <- mkTrace (return count) thunk
                  report $ ShallowFail trace (toTrace $ seqValue expected)

                                         | otherwise = return ()

simulate :: Options -> FilePath -> (Result -> IO ()) -> (Int -> String -> IO ()) -> IO ()
simulate opts path report verb = do
    let localname = last $ splitPath path

    verb 2 $ "simulating with modelsim"
    pwd <- getCurrentDirectory
    system $ "echo `" ++ (simCmd opts) (path </> localname <.> "do") ++ "` > \"" ++ path </> "everything.log\""
    log <- readFile (path </> "transcript")

    success <- doesFileExist $ path </> localname <.> "deep"
    if success
        then do shallow <- lines <$> readFile (path </> localname <.> "shallow")
                deep    <- lines <$> readFile (path </> localname <.> "deep")
                sig     <- read  <$> readFile (path </> localname <.> "sig")

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

prepareSimDirectory :: Options -> IO ()
prepareSimDirectory opts | genSim opts = do
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
                        | otherwise = return ()

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
    ,"\t" ++ dots ++ "/tracediff " ++ name ++ ".shallow " ++ name ++ ".deep " ++ name ++ ".sig"
    ,"\tgtkwave diff.vcd"
    ,""
    ,"vcd:"
    ,"\twlf2vcd vsim.wlf > " ++ name ++ ".vcd"
    ,""
    ,"view: vcd"
    ,"\tgtkwave " ++ name ++ ".vcd"
    ]
    where dots = joinPath $ replicate l ".."
          l = 1 + (length $ splitPath relativePath)
          name = last $ splitPath relativePath


preludeFile = "Lava.vhd"

copyLavaPrelude :: Options -> FilePath -> IO ()
copyLavaPrelude opts dest = do
        prel <- readFile (preludePath opts </> preludeFile)
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
