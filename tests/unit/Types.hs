module Types where

import Language.KansasLava
import Language.KansasLava.Testing.Trace

import Data.Default

data Options = Options
        { genSim      :: Bool                              -- ^ Generate modelsim testbenches for each test?
        , runSim      :: Bool                              -- ^ Run the tests after generation?
        , simCmd      :: String                            -- ^ Command to call with runSim is True
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
                , "simCmd: " ++ show sc
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
                , simCmd = "sims/runsims"
                , simPath = "sims"
                , simMods = []
                , permuteMods = True
                , preludePath = "../../Prelude/VHDL"
                , verboseOpt = 3
                , testOnly = Nothing
                , testNever = []
                , testData = 1000
                }

type TestCase = (String, Result)

data Result = ShallowFail Trace Trace        -- Shallow result doesn't match expected
            | ShallowPass                    -- Shallow result matches, we aren't simulating
            | SimGenerated                   -- Shallow passed, testbench generated, not running sim
            | CodeGenFail String             -- Shallow passed, testbench generation failed
            | CompileFail String             -- VHDL compilation failed during simulation
            | SimFail     String             -- Modelsim failed for some other reason
            | CompareFail Trace Trace String -- Deep result didn't match the shallow result
            | Pass        Trace Trace String -- Deep matches shallow which matches expected
    deriving (Show, Read)

