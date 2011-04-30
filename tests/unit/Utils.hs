{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Utils where

import Language.KansasLava
import Language.KansasLava.Trace

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad

import Data.List as List
import Data.Maybe as Maybe
import Data.Ord ( comparing )
import Data.Sized.Unsigned

import System.Cmd
import System.Directory
import System.FilePath as FP
import qualified System.IO.Strict as Strict
import qualified System.Random as R

import Types

-------------------------------------------------------------------------------------

data TestData = Rand Int | Complete

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
        (String -> Int  -> Fabric () -> Fabric () -> Fabric () -> IO ())
        (forall a. Gen a -> [a])

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

testFabrics
        :: Options                  -- Options
        -> String                   -- Test Name
        -> Int                      -- Number of Cycles
        -> Fabric ()                -- Reference input
        -> Fabric ()                -- DUT
        -> Fabric ()                -- Reference output
        -> IO ()
testFabrics opts name count f_driver f_dut f_expected
   | testMe name (testOnly opts) && not (neverTestMe name (testNever opts)) = do
        let verb = verbose (verboseOpt opts) name
            path = (simPath opts) </> name
            report = fileReporter $ simPath opts

        verb 2 $ "testing(" ++ show count ++ ")"

        let inp :: [(String,Pad)]
            inp = runFabric f_driver []

            shallow :: [(String,Pad)]
            shallow = runFabric f_dut inp

            expected :: [(String,Pad)]
            expected = runFabric f_expected []

        verb 9 $ show ("expected",expected)
        verb 9 $ show ("shallow",shallow)

        if cmpFabricOutputs count expected shallow
          then do verb 3 $ "shallow passed"
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
                                           copyLavaPrelude opts (path </> modname)
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
          else do verb 1 $ "shallow FAILED"
                  t_dut <- mkTrace (return count) f_dut inp
                  t_driver <- mkTrace (return count) f_driver []
                  t_expected <- mkTrace (return count) f_expected []
                  verb 4 "DUT:"
                  verb 4 $ show t_dut
                  verb 4 "EXPECT IN:"
                  verb 4 $ show t_driver
                  verb 4 "EXPECT OUT:"
                  verb 4 $ show t_expected
                  report name $ ShallowFail t_dut t_expected
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
          l = 3 + (length $ splitPath relativePath)
          name = last $ splitPath relativePath

preludeFile :: String
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

largeNumber :: Integer
largeNumber = 10000
