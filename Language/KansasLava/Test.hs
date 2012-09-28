{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts, DeriveDataTypeable #-}
module Language.KansasLava.Test
        ( testMe
        , neverTestMe
        , verbose
        , fileReporter
        , TestSeq(..)
        , testFabrics
        , Gen(..)
        , arbitrary
        , allCases
        , finiteCases
        , testDriver
        , generateReport
        , Options(..)
        , matchExpected
        , StreamTest(..)
        , testStream
        ) where

import Language.KansasLava.Fabric
import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.VCD
import Language.KansasLava.VHDL
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Exception

-- found in dist/build/autogen
import Paths_kansas_lava

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Data.List as List
import Data.Maybe as Maybe
import Data.Default
--import Data.Sized.Unsigned

import System.Cmd
import System.Console.CmdArgs hiding (Default,def,name,summary,opt)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath as FP
import qualified System.IO.Strict as Strict
import qualified System.Random as R
import Data.Sized.Ix
--import System.Random

import qualified Language.KansasLava.Stream as S


-------------------------------------------------------------------------------------

-- data TestData = Rand Int | Complete

testMe :: String -> Maybe [String] -> Bool
testMe _ Nothing     = True
testMe nm (Just nms) = or [ (n `isInfixOf` nm) | n <- nms ]

neverTestMe :: String -> [String] -> Bool
neverTestMe nm nms = or [ (n `isInfixOf` nm) | n <- nms ]

verbose :: Int -> String -> Int -> String -> IO ()
verbose vlvl name n m | vlvl >= n = putStrLn (name ++ " :" ++ take n (repeat ' ') ++ m)
                      | otherwise           = return ()

fileReporter :: FilePath -> FilePath -> Result -> IO ()
fileReporter path nm res = do
    createDirectoryIfMissing True (path </> nm)
    writeFile (path </> nm </> "result") $ show res

-------------------------------------------------------------------------------------

-- Given a circuit that returns an a, and the expected results,
-- do some tests for sanity.

data TestSeq = TestSeq
        (String -> Int  -> Fabric () -> (Fabric (Int -> Maybe String)) -> IO ())
        ()      -- remove the unit

{-
-- | Fabric outputs are equal if for each output from the left fabric,
--   there exists an output in the right fabric with the same name, type,
--   and sequence of values. Sequences are compared with cmpRepValue,
--   so the left fabric is the 'golden' fabric, and may be more general
--   in respect to unknowns than the right fabric.
cmpFabricOutputs :: Int -> [(String,Pad)] -> [(String,Pad)] -> Bool
cmpFabricOutputs count expected shallow =
    and [    n1 == n2
         && ty1 == ty2
         && (and $ List.zipWith cmpRepValue (take count ereps) sreps)
        | ((n1,pad1),(n2,pad2)) <- zip expected shallow
        , let (ty1,ereps) = getTyRep pad1
        , let (ty2,sreps) = getTyRep pad2
        ]
    where getTyRep :: Pad -> (StdLogicType, [RepValue])
          getTyRep pad = case pad of
                           StdLogic s -> (padStdLogicType pad,map toRep $ toList $ shallowS s)
                           StdLogicVector s -> (padStdLogicType pad,map toRep $ toList $ shallowS s)
                           GenericPad _ -> error "testFabrics: Generic output pad?"
-}

type SimMods = [(String,KLEG -> IO KLEG)]

testFabrics
        :: Options                  -- Options
        -> SimMods                  -- ^ [(String,KLEG -> IO KLEG)]
        -> String                   -- Test Name
        -> Int                      -- Number of Cycles
        -> Fabric ()                         -- DUT
        -> (Fabric (Int -> Maybe String))    -- Driver
        -> IO ()
testFabrics opts simMods name count f_dut f_expected
   | testMe name (testOnly opts) && not (neverTestMe name (testNever opts)) = (do


        verb 2 $ "testing(" ++ show count ++ ")"

        let inp :: [(String,Pad)]
            (expected_fn,inp) = runFabric f_expected shallow

            shallow :: [(String,Pad)]
            (_,shallow) = runFabric f_dut inp

            expected = expected_fn count

        verb 9 $ show ("shallow",shallow)
        verb 9 $ show ("expected",expected)

        case expected of
          Nothing -> do
                  verb 3 $ "shallow passed"
                  if genSim opts
                    then do createDirectoryIfMissing True path

                            -- get permuted/unpermuted list of sims for which we generate testbenches
                            let sims = [ (modname, (mkTestbench' (path </> modname) count (snd cmod) f_dut inp))
                                       | cmod <- if permuteMods opts
                                                    then map (foldr (\(nm,m) (nms,ms) -> (nm </> nms, m >=> ms)) ("unmodified", (return)))
                                                           $ concatMap permutations
                                                           $ subsequences
                                                           $ simMods
                                                    else simMods
                                       , let modname = fst cmod
                                       ]

                            -- generate each testbench, report any failures
                            ts <- sequence [ do vrb 2 $ "generating simulation"
                                                E.catch (Just <$> action)
                                                        (\e -> do vrb 3 "vhdl generation failed"
                                                                  vrb 4 $ show (e :: E.SomeException)
                                                                  rep $ CodeGenFail -- (show (e :: E.SomeException))
                                                                  return Nothing)
                                           | (modname, action) <- sims
                                           , let rep = report (name </> modname)
                                           , let vrb = verbose (verboseOpt opts) (name </> modname)
                                           ]

                            -- for successfully generated testbenches, add some files
                            sequence_ [ do writeFile (path </> modname </> "Makefile") $ localMake (name </> modname)
                                           copyLavaPrelude (path </> modname)
                                           writeFile (path </> modname </> "options") $ show opts
                                           rep $ SimGenerated
                                           vrb 9 $ show ("trace",fromJust t)
                                      | (modname, t) <- zip (map fst sims) ts
                                      , isJust t
                                      , let vrb = verbose (verboseOpt opts) (name </> modname)
                                      , let rep = report (name </> modname)
                                      ]

                            return ()
                    else report name ShallowPass
          Just msg -> do
                  verb 1 $ "shallow FAILED"
--                  t_dut <- mkTrace (return count) f_dut inp
--                  verb 4 "DUT:"
--                  verb 4 $ show t_dut
--                  verb 4 "EXPECT IN:"
--                  verb 4 $ show $ take count shallow
--                  verb 4 "EXPECT OUT MESSAGE:"
                  verb 4 $ msg
                  report name $ ShallowFail) `E.catch`
                (\ (e :: E.SomeException) -> do verb 2 $ ("SomeException: " ++ show e)
                                                report name TestAborted)
  | otherwise = return ()
 where
         verb = verbose (verboseOpt opts) name
         path = (simPath opts) </> name
         report = fileReporter $ simPath opts

simCompare :: FilePath -> (Result -> IO ()) -> (Int -> String -> IO ()) -> IO ()
simCompare path report verb = do
    let localname = last $ splitPath path

    ran <- doesFileExist $ path </> "transcript"
    if ran
        then do -- transcript <- Strict.readFile (path </> "transcript")
                success <- doesFileExist $ path </> localname <.> "out.tbf"
                if success
                    then do shallow <- lines <$> Strict.readFile (path </> localname <.> "in.tbf")
                            deep    <- lines <$> Strict.readFile (path </> localname <.> "out.tbf")
                            sig     <- read  <$> Strict.readFile (path </> localname <.> "sig")

                            let t1 = readTBF shallow sig
                                t2 = readTBF deep sig
                            if cmpVCD (ioOnly t1) (ioOnly t2)
                                then do verb 3 "simulation passed"
                                        report $ Pass -- t1 t2 transcript
                                else do verb 3 "simulation failed"
--                                        verb 4 $ show ("shallow",t1)
--                                        verb 4 $ show ("deep",t2)
                                        report $ CompareFail

                    else do verb 3 "VHDL compilation failed"
--                            verb 4 transcript
                            report $ CompileFail -- transcript
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
        then do _ <- system $ "rm -rf " ++ path
                return ()
        else return ()

    createDirectoryIfMissing True path

    writeFile (path </> "runsims") $ unlines testRunner
    _ <- system $ "chmod +x " ++ path </> "runsims"

    return ()

testRunner :: [String]
testRunner = [
 "#!/bin/bash",
 "",
 "if [ \"$1\" == \"isim\" ] ; then",
 "\tCMD=\"make isim\"",
 "else",
 "\tCMD=\"vsim -c -do unmodified.do\"",
 "fi",
 "if type -P parallel; then",
 "echo \"Using parallel simulation\"",
 "",
 "[ -n \"$LAVA_MODELSIM_HOSTS\" ] ||  export LAVA_MODELSIM_HOSTS=\":\"",
 "echo \"Using $LAVA_MODELSIM_HOSTS for simulation\"",
 "",
 "find . -iname \"*.do\" | parallel dirname | \\",
 "\tparallel -j 300% --eta -W /tmp --sshlogin $LAVA_MODELSIM_HOSTS \\",
 "\t--transfer --return {} \"cd {} && $CMD > /dev/null\"",
 "else",
 "\t\tcurdir=`pwd`",
 "\t\techo \"Using sequential simulation\"",
 "\t\tfind . -iname \"*.do\" | while read f",
 "\t\tdo",
 "\t\t\t\techo \"Simulating: $f\"",
 "\t\t\t\tp=`dirname \"$f\"`",
 "\t\t\t\tb=`basename \"$f\"`",
 "\t\t\t\tcd $p",
 "\t\t\t\tres=`vsim -c -do $b`",
 "\t\t\t\techo $res >> sim.log",
 "\t\t\t\tcd $curdir",
 "\t\tdone;",
 "fi"
 ]


localMake :: String -> String
localMake relativePath = unlines
    ["vsim:"
    ,"\tvsim -c -do " ++ name ++ ".do"
    ,""
    ,"diff:"
    ,"\t" ++ dots </> "dist/build/kansas-lava-tbf2vcd/kansas-lava-tbf2vcd --diff " ++ name ++ ".sig " ++ name ++ ".in.tbf " ++ name ++ ".out.tbf diff.vcd"
    ,"\tgtkwave diff.vcd"
    ,""
    ,"vcd:"
    ,"\twlf2vcd vsim.wlf > " ++ name ++ ".vcd"
    ,""
    ,"view: vcd"
    ,"\tgtkwave " ++ name ++ ".vcd"
    ,""
    ,"isim: unmodified_sim " ++ name ++ ".tcl"
    ,"\t./" ++ name ++ "_sim -tclbatch " ++ name ++ ".tcl"
    ,""
    ,name ++ "_sim: " ++ name ++ ".vhd Lava.vhd " ++ name ++ ".prj"
    ,"\tfuse -prj " ++ name ++ ".prj work." ++ name ++ "_tb -o " ++ name ++ "_sim"
    ,""
    ,name ++ ".prj:"
    ,"\techo \"vhdl work Lava.vhd\" > " ++ name ++ ".prj"
    ,"\techo \"vhdl work " ++ name ++ ".vhd\" >> " ++ name ++ ".prj"
    ,"\techo \"vhdl work " ++ name ++ "_tb.vhd\" >> " ++ name ++ ".prj"
    ,""
    ,name ++ ".tcl:"
    ,"\techo \"vcd dumpfile " ++ name ++ ".vcd\" > " ++ name ++ ".tcl"
    ,"\techo \"vcd dumpvars -m / \" >> " ++ name ++ ".tcl"
    ,"\techo \"vcd dumpon\" >> " ++ name ++ ".tcl"
    ,"\techo \"run all\" >> " ++ name ++ ".tcl"
    ,"\techo \"vcd dumpoff\" >> " ++ name ++ ".tcl"
    ,"\techo \"quit\" >> " ++ name ++ ".tcl"
    ]
    where dots = joinPath $ replicate l ".."
          l = 2 + (length $ splitPath relativePath)
          name = last $ splitPath relativePath

preludeFile :: String
preludeFile = "Lava.vhd"

copyLavaPrelude :: FilePath -> IO ()
copyLavaPrelude dest = do
  file <- readPreludeFile ("Prelude/VHDL/" </> preludeFile)
  writeFile (dest </> preludeFile) file

-------------------------------------------------------------------------------------

data Gen a = Gen Integer (Integer -> Maybe a)

arbitrary :: forall w . (Rep w) => Gen w
arbitrary = Gen sz integer2rep
  where
        sz = 2 ^ (fromIntegral (repWidth (Witness :: Witness w)) :: Int)
        integer2rep :: Integer -> Maybe w
        integer2rep v = unX
                $ fromRep
                $ RepValue
                $ take (repWidth (Witness :: Witness w))
                $ map Just
                $ map odd
                $ iterate (`div` 2)
                $ (fromIntegral v :: Int)

------------------------------------------------------------------------------------
-- The new testing system.

-- | 'allCases' returns all values of type w, in a non-random order.
allCases :: (Rep w) => [w]
allCases = Maybe.catMaybes $ fmap f [0..(n-1)]
   where (Gen n f) = arbitrary

-- | 'finiteCases' returns finite values, perhaps many times, in a random order.
finiteCases :: (Rep w) => Int ->[w]
finiteCases i = take i $ Maybe.catMaybes $ fmap f $ R.randomRs (0,n-1) (R.mkStdGen 0)
  where (Gen n f) = arbitrary

-------------------------------------------------------------------

data Report = Report Summary [TestCase]

instance Show Report where
    show (Report s _) = show s

data Summary = Summary { sfail :: Int
                       , spass :: Int
                       , generated :: Int
                       , codegenfail :: Int
                       , vhdlfail :: Int
                       , simfail :: Int
                       , compfail :: Int
                       , testaborted :: Int
                       , passed :: Int
                       , total :: Int
                       }


instance Show Summary where
    show summary = unlines [tt,sf,sp,rf,gn,vf,cp,si,ja,ps]
        where tt = "Total tests: " ++ show (total summary)
              sf = "Shallow test failures: " ++ show (sfail summary)
              sp = "Shallow tests passed: "
                   ++ case spass summary of
                        0 -> show $ sum [ fn summary
                                        | fn <- [generated, codegenfail, vhdlfail, simfail, compfail, passed]
                                        ]
                        x -> show x
              rf = "VHDL generation failures: " ++ show (codegenfail summary)
              gn = "Simulations generated: "
                   ++ case generated summary of
                        0 -> show $ sum [ fn summary
                                        | fn <- [vhdlfail, simfail, compfail, passed]
                                        ]
                        x -> show x
              vf = "VHDL compilation failures: " ++ show (vhdlfail summary)
              cp = "Simulation failures (non-matching traces): " ++ show (compfail summary)
              si = "Simulation failures (other): " ++ show (simfail summary)
              ja = "Tests that just aborted: " ++ show (testaborted summary)
              ps = "Simulation tests passed: " ++ show (passed summary)

generateReport :: FilePath -> IO ()
generateReport path = do
    postSimulation path
    r <- buildReport <$> buildResults path

    putStrLn $ show r

    html <- reportToHtml r
    writeFile "report.html" html
    shtml <- reportToSummaryHtml r
    writeFile "summary.html" shtml

-- Traverses all the generated simulation directories and reads the result files.
buildResults :: FilePath -> IO [TestCase]
buildResults spath = go "" spath
    where go :: String -> FilePath -> IO [TestCase]
          go name path = do
            resE <- doesFileExist $ path </> "result"
            res <- if resE
                    then liftM (\r -> [(name,r)]) (read <$> (Strict.readFile $ path </> "result"))
                    else return []

            contents <- getDirectoryContents path
            subdirs <- filterM (\(_,f) -> doesDirectoryExist f)
                               [ (name </> f, path </> f)
                               | f <- contents
                               , f /= "."
                               , f /= ".." ]

            subresults <- concat <$> (mapM (uncurry go) subdirs)

            return $ res ++ subresults

addtoSummary :: Result -> Summary -> Summary
addtoSummary (ShallowFail {}) s = s { sfail = 1 + (sfail s) }
addtoSummary ShallowPass      s = s { spass = 1 + (spass s) }
addtoSummary SimGenerated     s = s { generated = 1 + (generated s) }
addtoSummary (CodeGenFail {}) s = s { codegenfail = 1 + (codegenfail s) }
addtoSummary (CompileFail {}) s = s { vhdlfail = 1 + (vhdlfail s) }
addtoSummary (SimFail {})     s = s { simfail = 1 + (simfail s) }
addtoSummary (CompareFail {}) s = s { compfail = 1 + (compfail s) }
addtoSummary (TestAborted)    s = s { testaborted = 1 + (testaborted s) }
addtoSummary (Pass {})        s = s { passed = 1 + (passed s) }

buildReport :: [TestCase] -> Report
buildReport rs = Report summary rs
    where rs' = map snd rs
          summary = foldr addtoSummary (Summary 0 0 0 0 0 0 0 0 0 (length rs')) rs'

reportToSummaryHtml :: Report -> IO String
reportToSummaryHtml (Report summary _) = do
    header <- readPreludeFile "Prelude/HTML/header.inc"
    mid <-  readPreludeFile "Prelude/HTML/mid.inc"
    footer <- readPreludeFile "Prelude/HTML/footer.inc"

    return $ header ++ (summaryToHtml summary) ++ mid ++ footer

summaryToHtml :: Summary -> String
summaryToHtml s = unlines [ "<table>"
                          , "<tr class=\"huge " ++ sclass ++ "\"><td>Shallow Failures:</td><td>" ++ show (sfail s) ++ "</td><td>(" ++ show (total s - sfail s) ++ "/" ++ show (total s) ++ " passed)</td></tr>"
                          , "<tr class=\"huge " ++ dclass ++ "\"><td>Simulation Failures:</td><td>" ++ show (sum [codegenfail s, vhdlfail s, compfail s, simfail s]) ++
                              "</td><td>(" ++ show (passed s) ++ "/" ++ show (total s - sfail s) ++ " passed)</td></tr>"
                          , "</table>"
                          , "<hr width=\"90%\">"
                          , "<table>"
                          , "<tr id=\"cgf\" class=\"kindahuge\"><td>VHDL Generation Failures:</td><td>" ++ show (codegenfail s) ++ "</td></tr>"
                          , "<tr id=\"vcf\" class=\"kindahuge\"><td>VHDL Compilation Failures:</td><td>" ++ show (vhdlfail s) ++ "</td></tr>"
                          , "<tr id=\"cpf\" class=\"kindahuge\"><td>Comparison Failures:</td><td>" ++ show (compfail s) ++ "</td></tr>"
                          , "<tr id=\"osf\" class=\"kindahuge\"><td>Other Simulation Failures:</td><td>" ++ show (simfail s) ++ "</td></tr>"
                          , "</table>"
                          ]
    where chooser x = case x of
                        0 -> "allpass"
                        i | i == total s -> "allfail"
                        _ -> "somepass"
          sclass = chooser $ sfail s
          dclass = chooser $ total s - sfail s - passed s


reportToHtml :: Report -> IO String
reportToHtml (Report summary results) = do
    header <- readPreludeFile "Prelude/HTML/header.inc"
    mid <- readPreludeFile "Prelude/HTML/mid.inc"
    footer <- readPreludeFile "Prelude/HTML/footer.inc"

    let showall = "<a href=\"#\" id=\"showall\">Show All</a>"
        res = unlines [ concat ["<div id=\"", name, "\" class=\"header ", sc, "\">", name
                               ,"<span class=\"status\">", s, "</span></div>\n<div class=\"additional\">"
                               , "</div>"]
                      | (name, r) <- results
                      , let (sc, s) = case r of
                                           ShallowFail {}-> ("shallowfail", "Shallow Failed")
                                           ShallowPass -> ("shallowpass", "Shallow Passed")
                                           SimGenerated -> ("simgenerated", "Simulation Generated")
                                           CodeGenFail {} -> ("codegenfail", "VHDL Generation Failed")
                                           CompileFail {} -> ("compilefail", "VHDL Compilation Failed")
                                           SimFail {} -> ("simfail", "Simulation Failed (other)")
                                           CompareFail {} -> ("comparefail", "Failed")
                                           TestAborted {} -> ("testabort", "Test Aborted")
                                           Pass {} -> ("pass", "Passed")
                      ]
    return $ header ++ (summaryToHtml summary) ++ mid ++ showall ++ res ++ footer

--unDiv :: [String] -> String
--unDiv = foldr (\s t -> "<div>" ++ sliceString 200 80 s ++ "</div>" ++ t) ""

--sliceString :: Int -> Int -> String -> String
--sliceString r c str = unlines $ take r $ chunk str
--    where chunk [] = []
--          chunk s  = let (c1,r') = splitAt c s in c1 : chunk r'

testDriver :: Options -> [TestSeq -> IO ()] -> IO ()
testDriver dopt tests = do
        opt <- cmdArgs dopt

        putStrLn "Running with the following options:"
        putStrLn $ show opt

        prepareSimDirectory opt

        work <- newEmptyMVar :: IO (MVar (Either (MVar ()) (IO ())))

        let thread_count :: Int
            thread_count = parTest opt

        sequence_ [
                forkIO $
                let loop = do
                        act <- takeMVar work
                        case act of
                           Left end -> do
                                putMVar end ()
                                return () -- stop command
                           Right io ->
                                do io `catches`
                                        [ Handler $ \ (ex :: AsyncException) -> do
                                             putStrLn ("AsyncException: " ++ show ex)
                                             throw ex
                                        , Handler $ \ (ex :: SomeException) -> do
                                             putStrLn ("SomeException: " ++ show ex)
                                             throw ex
                                        ]
                                   loop
                in loop
                | _ <- [1..thread_count]]


        let test :: TestSeq
            test = TestSeq (\ nm sz fab fn ->
                              let work_to_do = testFabrics opt [] nm sz fab fn
                              in
                                putMVar work (Right $ work_to_do)
--                              work_to_do
                           )
                           ()

        -- The different tests to run (from different modules)
        sequence_ [ t test
                  | t <- tests
                  ]

        -- wait for then kill all the worker threads
        sequence_ [ do stop <- newEmptyMVar
                       putMVar work (Left stop)
                       takeMVar stop
                  | _ <- [1..thread_count]
                  ]


        -- If we didn't generate simulations, make a report for the shallow results.
        if genSim opt
            then if runSim opt
                    then do _ <- system $ simCmd opt
                            generateReport $ simPath opt
                    else do putStrLn $ unlines [""
                                               ,"Run simulations and generate reports using the Makefile commands"
                                               ,"or the individual Makefiles in each simulation subdirectory."
                                               ,""]
            else generateReport $ simPath opt

--------------------------------------------------------------------


data Options = Options
        { genSim      :: Bool                        -- ^ Generate modelsim testbenches for each test?
        , runSim      :: Bool                        -- ^ Run the tests after generation?
        , simCmd      :: String                      -- ^ Command to call with runSim is True
        , simPath     :: FilePath                    -- ^ Path into which we place all our simulation directories.
        , permuteMods :: Bool                        -- ^ False: Run each mod separately. True: Run all possible
                                                     --   permutations of the mods to see if they affect each other.
        , verboseOpt  :: Int                         -- ^ See verbose table below.
        , testOnly    :: Maybe [String]              -- ^ Lists of tests to execute. Can match either end. Nothing means all tests.
        , testNever   :: [String]                    -- ^ List of tests to never execute. Can match either end.
        , testData    :: Int                         -- ^ cut off for random testing
        , parTest     :: Int                         -- ^ how may tests to run in parallel
        } deriving (Data, Typeable)

instance Show Options where
    show (Options gs rs sc sp pm vo to tn td pt) =
        unlines [ "genSim: " ++ show gs
                , "runSim: " ++ show rs
                , "simCmd: " ++ show sc
                , "simPath: " ++ show sp
                , "permuteMods: " ++ show pm
                , "verboseOpt: " ++ show vo
                , "testOnly: " ++ show to
                , "testNever: " ++ show tn
                , "testData: " ++ show td
                , "parTest: " ++ show pt ]



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
                { genSim = False &= help "Generate modelsim testbenches for each test?"
                , runSim = False &= help "Run the tests after generation?"
                , simCmd = "sims/runsims" &= help "Command to call when runSim is True"
                , simPath = "sims" &= typDir &= help "Path into which we place all our simulation directories."
                , permuteMods = True &= help "Run all possible permutations of circuit mods."
                , verboseOpt = 4 &= help "Verbosity level. 1: Failures 2: What runs 3: What succeeds 4: Failures 9: Everything"
                , testOnly = Nothing &= help "List of tests to execute. Can match either end. Default is all tests."
                , testNever = [] &= help "List of tests to never execute. Can match either end."
                , testData = 1000 &= help "Cutoff for random testing."
                , parTest = 4 &= help "Number of tests to run in parallel."
                                -- everyone has multicore now.
                                -- This is the number of *threads*,
                                -- so we cope with upto 4 cores.
                }

type TestCase = (String, Result)

data Result = ShallowFail {- Trace String -}      -- Shallow result doesn't match expected
            | ShallowPass                    -- Shallow result matches, we aren't simulating
            | SimGenerated                   -- Shallow passed, testbench generated, not running sim
            | CodeGenFail {- String -}             -- Shallow passed, testbench generation failed
            | CompileFail {- String -}             -- VHDL compilation failed during simulation
            | SimFail     {- String -}            -- Modelsim failed for some other reason
            | CompareFail {- Trace Trace String -} -- Deep result didn't match the shallow result
            | TestAborted                          -- Something went badly wrong a some stage
            | Pass        {- Trace Trace String  -}-- Deep matches shallow which matches expected
    deriving (Show, Read)

---------------------------------------------------------------------------------------

-- | matchExpected reads a named input port from
-- a Fabric, and checks to see that it is a refinement
-- of a given "specification" of the output.
-- If there is a problem, issue an error message.

matchExpected :: (Rep a, Size (W a), Show a) => String -> Seq a -> Fabric (Int -> Maybe String)
matchExpected out_name ref = do
        o0 <- inStdLogicVector out_name
        let sq = o0 `refinesFrom` ref
        return $ \ count ->
                case [ (i::Int,o,r)
                     | (i,v,o,r) <- take (fromIntegral count)
                                $ zip4 [0..]
                                  (fromS sq)
                                  (S.toList (fmap (show . unRepValue . toRep) (shallowS o0)))
                                  (S.toList (fmap (show . unRepValue . toRep) (shallowS ref)))

                     , v /= Just True
                     ] of
                     [] -> Nothing
                     ns -> Just $ "failed on cycles " ++ show (take 20 $ ns)


----------------------------------------------------------------------------

data StreamTest w1 w2 = StreamTest
            { theStream            :: Patch (Seq (Enabled w1))          (Seq (Enabled w2))
                                            (Seq Ack)                   (Seq Ack)
            , correctnessCondition :: [w1] -> [w2] -> Maybe String
            , theStreamTestCount   :: Int
            , theStreamTestCycles  :: Int
            , theStreamName        :: String
            }

testStream :: forall w1 w2 . ( Eq w1, Rep w1, Show w1, Size (W w1)
                             , Eq w2, Rep w2, Show w2, Size (W w2)
                             )
        => TestSeq -> String -> StreamTest w1 w2 -> IO ()
testStream (TestSeq test _) tyName streamTest = do

        let vals0 :: [Maybe w1]
            vals0 = finiteCases (fromIntegral (theStreamTestCycles streamTest))

            vals1 :: [Int]
            vals1 = drop (fromIntegral (theStreamTestCount streamTest))
                    [ n
                    | (Just _,n) <- zip vals0 [0..]
                    ]

            vals :: [Maybe w1]
            vals = case vals1 of
                     [] -> vals0
                     (n:_) -> [ if i < n then v else Nothing
                              | (v,i) <- zip vals0 [0..]
                              ]

{-
        print (theStreamTestCount StreamTest,theStreamTestCycles StreamTest)
        print vals0
        print vals1
        print vals
-}
        -- good enough for this sort of testing
--        let stdGen = mkStdGen 0

        let -- (lhs_r,rhs_r) = split stdGen

            cir = theStream streamTest

            driver :: Fabric (Int -> Maybe String)
            driver = do
                -- backedge output from DUT
                ack <- inStdLogic "ack" :: Fabric (Seq Ack)

                let vals2 :: Seq (Enabled w1)
                    (_,vals2) = toAckBox' a (vals ++ Prelude.repeat Nothing,ack)

                -- sent to DUT
                outStdLogicVector "vals"        (enabledVal vals2)
                outStdLogic       "vals_en"     (isEnabled vals2)

                -- DUT does stuff

                -- reading from DUT
                res     <- inStdLogicVector "res"
                res_en  <- inStdLogic       "res_en"

                let flag :: Seq Ack
                    opt_as :: [Maybe w2]

                    (flag, opt_as) = fromAckBox' d (packEnabled res_en res,())

                outStdLogic "flag" flag

                return $ \ n -> correctnessCondition streamTest
                                   [ x | (Just x) <- take n $ vals ]
                                   [ x | (Just x) <- take n $ opt_as ]

{-
let ans = [ a | Just a <- take n opt_as ]
                                    inp = [ a | Just a <- take n vals ]
                                in if ans == take (length ans) inp
                                   && length inp > 1000
                                   then Nothing -- ("matched" ++ show (length ans))
                                   else Just (show (ans,inp))
-}

            dut :: Fabric ()
            dut = do
                flag    <- inStdLogic "flag"
                vls     <- inStdLogicVector "vals"
                vals_en <- inStdLogic "vals_en"
                let (ack,res') = cir (packEnabled vals_en vls, flag)
                outStdLogicVector "res"  (enabledVal res')
                outStdLogic "res_en"     (isEnabled res')
                outStdLogic "ack"        ack


            a = cycle [0..2] -- \ n -> [0.1,0.2 ..] !! fromIntegral (n `div` 10000)
            d = cycle [0..4] -- \ n -> [0.1,0.2 ..] !! fromIntegral (n `div` 10000)

        test ("stream/" ++ theStreamName streamTest ++ "/" ++ tyName) (length vals) dut driver


-- | Get a file from the prelude. First, check the KANSAS_LAVA_ROOT system
-- environment variable. If it exists, use that. If not, try to get it from the
-- installed cabal package.
readPreludeFile :: String -> IO String
readPreludeFile fname = do
   ks <- getEnv "KANSAS_LAVA_ROOT"
   Strict.readFile (ks </> fname)
 `E.catch` \ (_ :: IOException) -> do
    path <- getDataFileName fname
    Strict.readFile path
 `E.catch` \ (_ :: IOException) -> do
   putStrLn "Set the KANSAS_LAVA_ROOT environment variable"
   putStrLn "to point to the root of the KsLava source directory."
   exitFailure

-----------------------------------

-- | Make a VHDL testbench from a 'Fabric' and its inputs.
mkTestbench' :: FilePath                 -- ^ Directory where we should place testbench files. Will be created if it doesn't exist.
            -> Int                      -- ^ Generate inputs for this many cycles.
            -> (KLEG -> IO KLEG)  -- ^ any operations on the circuit before VHDL generation
            -> Fabric ()                -- ^ The Fabric for which we are building a testbench.
            -> [(String,Pad)]           -- ^ Inputs to the Fabric
            -> IO VCD
mkTestbench' path cycles circuitMod fabric input = do
    let name = last $ splitPath path

    createDirectoryIfMissing True path

    (vcd, rc) <- mkVCDCM cycles fabric input circuitMod

    writeTBF (path </> name <.> "in.tbf") vcd
    writeFile (path </> name <.> "sig") $ show $ toSignature vcd
    writeFile (path </> name <.> "kleg") $ show rc

    writeVhdlCircuit name (path </> name <.> "vhd") rc
    mkTestbench name path rc

    return vcd
