-- Necessary Imports
import Language.KansasLava

import Language.KansasLava.Testing.Probes
import Language.KansasLava.Testing.Compare

-- The circuit we want to test. Note we have probed both the half adders.
-- You can probe both values and functions, but for the purposes of the machinery
-- below, you should only probe functions.
fullAdder a b cin = (sum,cout)
    where (s1,c1) = probe "ha1" halfAdder a b
          (sum,c2) = probe "ha2" halfAdder cin s1
          cout = xor2 c1 c2

          halfAdder a b = (xor2 a b, and2 a b)

main = do
    -- To test reification
    testReify "fullAdder" (fullAdder :: Seq Bool -> Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool))

    -- To compare the deep and shallow embedding for equivalent behavior
    -- Basic pattern is:
    --      testCircuit <options> <name> <circuit with types specialized> <function that applies inputs to the circuit>
    testCircuit def "fullAdder" (fullAdder :: Seq Bool -> Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool))
                                (\f -> f (toSeq $ cycle [False, True])
                                         (toSeq $ cycle [False, False, True, True])
                                         (toSeq $ cycle [False, True, False]))
-- ghci -i../.. Example.hs
-- Main> :main
-- ... lots of stuff will happen ...
-- ... simulation happens on rome, requiring you to log in ...

-- To change options for these tests
-- Main> def :: DebugOpts
-- Opts {username = "", host = "rome", cycles = 100, baseDir = "examine/", reifyOptions = [], enabled = []}
--
-- username     -> Username to use when logging into 'host', an empty string
--                 results in your current username being used, which is why this is the default
--
-- host         -> Host to log into. This is the part after the @ when you ssh
--
-- cycles       -> Number of cycles to simulate/compare
--
-- baseDir      -> Where to put the debug info. This should be a relative path.
--
-- reifyOptions -> Any options to be passed along to the reifyCircuit function.
--
-- enabled      -> If multiple tests are created with testCircuit, specify which ones to run.
--                 An empty list runs all tests in succession.
--
-- The above example with some different options
    testCircuit (def { cycles = 500, reifyOptions = [OptimizeReify] })
                "fullAdder500"
                (fullAdder :: Seq Bool -> Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool))
                (\f -> f (toSeq $ cycle [False, True])
                         (toSeq $ cycle [False, False, True, True])
                         (toSeq $ cycle [False, True, False]))

-- After running these, you can look in examine/fullAdder500/fullAdder500/ for a load of goodies:
--
-- fullAdder500_0.info -> file showing probed inputs and output in both human and bit format
-- fullAdder500.dot    -> dot file of the circuit
-- fullAdder500.png    -> image of the dot file
-- fullAdder500.input  -> bit file of the shallow inputs/output (bad name I know)
-- fullAdder500.output -> bit file of the deep inputs/outputs (also a bad name, will change these)
-- fullAdder500.vhd    -> vhdl that was generated
--
-- If you have any probes within the circuit, each one that was run will have a subfolder
-- in examine/fullAdder500/ ... but these subcircuits are only run if the overall circuit fails
-- the test (the embeddings differ). If you want to only test a specific probed subcircuit within
-- a larger circuit:
--
    testOnly def "ha2" (fullAdder :: Seq Bool -> Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool))
                       (\f -> f (toSeq $ cycle [False, True])
                                (toSeq $ cycle [False, False, True, True])
                                (toSeq $ cycle [False, True, False]))

-- When using testOnly, the test data will appear in examine/ha2/ha2/
