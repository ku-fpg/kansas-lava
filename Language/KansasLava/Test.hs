{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts #-}
module Language.KansasLava.Test
        ( testMe
        , neverTestMe
        , verbose
        , fileReporter
        , TestSeq(..)
        , testFabrics
        , Gen(..)
        , arbitrary
        , loop
        , dubGen
        , genToList
        , genToRandom
        , largeNumber
        , testDriver
        , generateReport
        , Options(..)
        , matchExpected
	, StreamTest(..)
	, testStream
        ) where

import Language.KansasLava.Trace
import Language.KansasLava.Types
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Utils
import Language.KansasLava.Seq
import Language.KansasLava.VHDL
import Language.KansasLava.Protocols

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Data.List as List
import Data.Maybe as Maybe
import Data.Default
import Data.Ord ( comparing )
--import Data.Sized.Unsigned

import System.Cmd
import System.Directory
import System.Environment
import System.FilePath as FP
import qualified System.IO.Strict as Strict
import qualified System.Random as R
import Data.Sized.Ix
--import System.Random


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
        (forall a. Gen a -> [a])

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
                           StdLogic s -> (padStdLogicType pad,map toRep $ toList $ seqValue s)
                           StdLogicVector s -> (padStdLogicType pad,map toRep $ toList $ seqValue s)
                           GenericPad _ -> error "testFabrics: Generic output pad?"
-}

testFabrics
        :: Options                  -- Options
        -> String                   -- Test Name
        -> Int                      -- Number of Cycles
        -> Fabric ()                         -- DUT
        -> (Fabric (Int -> Maybe String))    -- Driver
        -> IO ()
testFabrics opts name count f_dut f_expected
   | testMe name (testOnly opts) && not (neverTestMe name (testNever opts)) = do
        let verb = verbose (verboseOpt opts) name
            path = (simPath opts) </> name
            report = fileReporter $ simPath opts

        verb 2 $ "testing(" ++ show count ++ ")"

        let inp :: [(String,Pad)]
            (expected_fn,inp) = runFabric' f_expected shallow

            shallow :: [(String,Pad)]
            (_,shallow) = runFabric' f_dut inp

            expected = expected_fn count

        verb 9 $ show ("shallow",shallow)
        verb 9 $ show ("expected",expected)

        case expected of
          Nothing -> do
                  verb 3 $ "shallow passed"
                  if genSim opts
                    then do createDirectoryIfMissing True path

                            -- get permuted/unpermuted list of sims for which we generate testbenches
                            let sims = [ (modname, (mkTestbench (path </> modname) count (snd cmod) f_dut inp))
                                       | cmod <- if permuteMods opts
                                                    then map (foldr (\(nm,m) (nms,ms) -> (nm </> nms, m >=> ms)) ("unmodified", (return)))
                                                           $ concatMap permutations
                                                           $ subsequences
                                                           $ simMods opts
                                                    else simMods opts
                                       , let modname = fst cmod
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
                  t_dut <- mkTrace (return count) f_dut inp
                  verb 4 "DUT:"
                  verb 4 $ show t_dut
                  verb 4 "EXPECT IN:"
                  verb 4 $ show $ take count shallow
                  verb 4 "EXPECT OUT MESSAGE:"
                  verb 4 $ msg
                  report name $ ShallowFail t_dut msg
  | otherwise = return ()

simCompare :: FilePath -> (Result -> IO ()) -> (Int -> String -> IO ()) -> IO ()
simCompare path report verb = do
    let localname = last $ splitPath path

    ran <- doesFileExist $ path </> "transcript"
    if ran
        then do transcript <- Strict.readFile (path </> "transcript")
                success <- doesFileExist $ path </> localname <.> "deep"
                if success
                    then do shallow <- lines <$> Strict.readFile (path </> localname <.> "shallow")
                            deep    <- lines <$> Strict.readFile (path </> localname <.> "deep")
                            sig     <- read  <$> Strict.readFile (path </> localname <.> "sig")

                            let t1 = fromASCII shallow sig
                                t2 = fromASCII deep sig
                            if cmpTraceIO t1 t2
                                then do verb 3 "simulation passed"
                                        report $ Pass t1 t2 transcript
                                else do verb 3 "simulation failed"
                                        verb 4 $ show ("shallow",t1)
                                        verb 4 $ show ("deep",t2)
                                        report $ CompareFail t1 t2 transcript

                    else do verb 3 "VHDL compilation failed"
                            verb 4 transcript
                            report $ CompileFail transcript
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
 "\tparallel --eta -W /tmp --sshlogin $LAVA_MODELSIM_HOSTS \\",
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
    ,"\t" ++ dots </> "dist/build/kansas-lava-tracediff/kansas-lava-tracediff " ++ name ++ ".shallow " ++ name ++ ".deep " ++ name ++ ".sig"
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
	ks <- getEnv "KANSAS_LAVA_ROOT"
        prel <- Strict.readFile (ks </> "Prelude/VHDL" </> preludeFile)
	writeFile (dest </> preludeFile) prel

-------------------------------------------------------------------------------------

-- Not really random, but good enough for basic testing.
unsort :: [x] -> [x]
unsort es = map snd . sortBy (comparing fst) $ zip rs es
  where rs = R.randoms stdGen :: [Integer]
        stdGen = R.mkStdGen 0

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
                $ map WireVal
                $ map odd
                $ iterate (`div` 2)
                $ (fromIntegral v :: Int)

loop :: Integer -> Gen w -> Gen w
loop n (Gen sz f) = Gen (sz * n) (\ i -> f $ i `mod` sz)


-- | makes sure all sequences of two specific elements happen.
-- Random messes this up a bit, but its still an approximation.
dubGen :: Gen w -> Gen w
dubGen g = ((\ a b c -> if a then b else c) <$> arbitrary)
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
        | otherwise = {- take (fromIntegral (min n largeNumber)) $ -} Maybe.catMaybes $ fmap f $ R.randomRs (0,n) (R.mkStdGen 0)

largeNumber :: Integer
largeNumber = 10000

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
                       , passed :: Int
                       , total :: Int
                       }


instance Show Summary where
    show summary = unlines [tt,sf,sp,rf,gn,vf,cp,si,ps]
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
addtoSummary (ShallowFail _ _) s = s { sfail = 1 + (sfail s) }
addtoSummary ShallowPass       s = s { spass = 1 + (spass s) }
addtoSummary SimGenerated      s = s { generated = 1 + (generated s) }
addtoSummary (CodeGenFail _)   s = s { codegenfail = 1 + (codegenfail s) }
addtoSummary (CompileFail _)   s = s { vhdlfail = 1 + (vhdlfail s) }
addtoSummary (SimFail _)       s = s { simfail = 1 + (simfail s) }
addtoSummary (CompareFail _ _ _) s = s { compfail = 1 + (compfail s) }
addtoSummary (Pass _ _ _)        s = s { passed = 1 + (passed s) }

buildReport :: [TestCase] -> Report
buildReport rs = Report summary rs
    where rs' = map snd rs
          summary = foldr addtoSummary (Summary 0 0 0 0 0 0 0 0 (length rs')) rs'

reportToSummaryHtml :: Report -> IO String
reportToSummaryHtml (Report summary _) = do
    ks <- getEnv "KANSAS_LAVA_ROOT"
    header <- Strict.readFile (ks </> "Prelude/HTML/header.inc")
    mid <- Strict.readFile    (ks </> "Prelude/HTML/mid.inc")
    footer <- Strict.readFile (ks </> "Prelude/HTML/footer.inc")

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
    ks <- getEnv "KANSAS_LAVA_ROOT"
    header <- Strict.readFile (ks </> "Prelude/HTML/header.inc")
    mid <- Strict.readFile    (ks </> "Prelude/HTML/mid.inc")
    footer <- Strict.readFile (ks </> "Prelude/HTML/footer.inc")

    let showall = "<a href=\"#\" id=\"showall\">Show All</a>"
        res = unlines [ concat ["<div id=\"", name, "\" class=\"header ", sc, "\">", name
                               ,"<span class=\"status\">", s, "</span></div>\n<div class=\"additional\">"
                               , a, "</div>"]
                      | (name, r) <- results
                      , let (sc, s, a) = case r of
                                           ShallowFail t ts -> ("shallowfail", "Shallow Failed", unDiv [show t, show ts])
                                           ShallowPass -> ("shallowpass", "Shallow Passed", unDiv [""])
                                           SimGenerated -> ("simgenerated", "Simulation Generated", unDiv [""])
                                           CodeGenFail s' -> ("codegenfail", "VHDL Generation Failed", unDiv [s'])
                                           CompileFail s' -> ("compilefail", "VHDL Compilation Failed", unDiv [s'])
                                           SimFail s' -> ("simfail", "Simulation Failed (other)", unDiv [s'])
                                           CompareFail t1 t2 s' -> ("comparefail", "Failed", unDiv [show t1, show t2, s'])
                                           Pass t1 t2 s' -> ("pass", "Passed", unDiv [show t1, show t2, s'])
                      ]
    return $ header ++ (summaryToHtml summary) ++ mid ++ showall ++ res ++ footer

unDiv :: [String] -> String
unDiv = foldr (\s t -> "<div>" ++ sliceString 200 80 s ++ "</div>" ++ t) ""

sliceString :: Int -> Int -> String -> String
sliceString r c str = unlines $ take r $ chunk str
    where chunk [] = []
          chunk s  = let (c1,r') = splitAt c s in c1 : chunk r'

testDriver :: Options -> [TestSeq -> IO ()] -> IO ()
testDriver opt tests = do

        putStrLn "Running with the following options:"
        putStrLn $ show opt

        prepareSimDirectory opt

        let test :: TestSeq
            test = TestSeq (testFabrics opt)
                           (take (testData opt) . genToRandom)

        -- The different tests to run (from different modules)
        sequence_ [ t test
                  | t <- tests
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
        , simMods     :: [(String, KLEG -> IO KLEG)] -- ^ List of modifications (like optimizations) to apply to
                                                     --   the circuit before simulation.
        , permuteMods :: Bool                        -- ^ False: Run each mod separately. True: Run all possible
                                                     --   permutations of the mods to see if they affect each other.
        , verboseOpt  :: Int                         -- ^ See verbose table below.
        , testOnly    :: Maybe [String]              -- ^ Lists of tests to execute. Can match either end. Nothing means all tests.
        , testNever   :: [String]                    -- ^ List of tests to never execute. Can match either end.
        , testData    :: Int                         -- ^ cut off for random testing
        }

instance Show Options where
    show (Options gs rs sc sp sm pm vo to tn td) =
        unlines [ "genSim: " ++ show gs
                , "runSim: " ++ show rs
                , "simCmd: " ++ show sc
                , "simPath: " ++ show sp
                , "simMods: " ++ show (map fst sm)
                , "permuteMods: " ++ show pm
                , "verboseOpt: " ++ show vo
                , "testOnly: " ++ show to
                , "testNever: " ++ show tn
                , "testData: " ++ show td ]

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
                , simCmd = "sims/runsims"
                , simPath = "sims"
                , simMods = []
                , permuteMods = True
                , verboseOpt = 4
                , testOnly = Nothing
                , testNever = []
                , testData = 1000
                }

type TestCase = (String, Result)

data Result = ShallowFail Trace String       -- Shallow result doesn't match expected
            | ShallowPass                    -- Shallow result matches, we aren't simulating
            | SimGenerated                   -- Shallow passed, testbench generated, not running sim
            | CodeGenFail String             -- Shallow passed, testbench generation failed
            | CompileFail String             -- VHDL compilation failed during simulation
            | SimFail     String             -- Modelsim failed for some other reason
            | CompareFail Trace Trace String -- Deep result didn't match the shallow result
            | Pass        Trace Trace String -- Deep matches shallow which matches expected
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
                case [ i::Int
                     | (i,v) <- take (fromIntegral count) $ zip [0..] (fromSeq sq)
                     , v /= Just True
                     ] of
                     [] -> Nothing
                     ns -> Just $ "failed on cycles " ++ show (take 20 $ ns)


----------------------------------------------------------------------------

data StreamTest w1 w2 = StreamTest
            { theStream            :: Patch (Seq (Enabled w1))		(Seq (Enabled w2))
					    (Seq Ack)  			(Seq Ack)
            , correctnessCondition :: [w1] -> [w2] -> Maybe String
	    , theStreamTestCount   :: Int
	    , theStreamTestCycles  :: Int
            , theStreamName        :: String
            }

testStream :: forall w1 w2 . ( Eq w1, Rep w1, Show w1, Size (W w1)
			     , Eq w2, Rep w2, Show w2, Size (W w2)
			     )
        => TestSeq -> String -> StreamTest w1 w2 -> Gen (Maybe w1) -> IO ()
testStream (TestSeq test _) tyName streamTest ws = do

        let vals0 :: [Maybe w1]
	    vals0 = take (fromIntegral (theStreamTestCycles streamTest))
			 (cycle (genToRandom $ loop 10 $ ws))

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

