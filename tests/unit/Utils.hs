{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Utils where

import Language.KansasLava
import Language.KansasLava.Testing.Thunk
import Language.KansasLava.Testing.Trace
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Default
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Maybe as Maybe
import Data.List
import Control.Applicative
import qualified Control.Exception as E

import System.Cmd
import System.Directory
import System.FilePath.Posix as FP
import qualified System.Random as R

import Report
-- Our Unit Tests

-------------------------------------------------------------------------------------

data TestData = Rand Int | Complete

data Options = Options
        { genSim     :: Bool            -- Generate modelsim testbenches for each test?
        , runSim     :: Bool            -- Run the tests after generation?
        , simCmd     :: String          -- Command to run the .do file with
        , simPath    :: FilePath        -- Path into which we place all our simulation directories.
        , verboseOpt :: Int             -- See verbose table below.
        , testOnly   :: Maybe [String]  -- Lists of tests to execute. Nothing means all tests.
        , testData   :: Maybe Int       --- cut off for random testing
        }

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
                { genSim = True
                , runSim = True
                , simCmd = "vsim -c -do"
                , simPath = "sims"
                , verboseOpt = 3
                , testOnly = Nothing
                , testData = Just 1000
                }

testMe _ Nothing     = True
testMe nm (Just nms) = or [ n `isPrefixOf` nm
                          | n <- nms
                          ]

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
testSeq opts r name count thunk expected | testMe name (testOnly opts) = do
        let verb n m = verbose opts n (name ++ " :" ++ take n (repeat ' ') ++ m)
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

                            verb 2 $ "generating simulation(" ++ show count ++ ")"

                            t <- E.catch (Just <$> (recordThunk path count (return) thunk))
                                         (\e -> do verb 3 "vhdl generation failed"
                                                   verb 4 $ show (e :: E.SomeException)
                                                   report $ CodeGenFail (show (e :: E.SomeException))
                                                   return Nothing)

                            if t /= Nothing
                                then do writeFile (path </> "Makefile") $ localMake name
                                        if runSim opts
                                            then simulate opts path report verb
                                            else do verb 3 "simulation generated"
                                                    report SimGenerated
                                        verb 9 $ show ("trace",t)
                                else return ()

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
    setCurrentDirectory path
    system $ "echo `" ++ simCmd opts ++ " \"" ++ localname <.> "do\"` > \"" ++ "everything.log\""
    setCurrentDirectory pwd
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

--    writeFile "tests/run" $ unlines testRunner
--    system "chmod +x tests/run"

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
