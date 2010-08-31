import Language.KansasLava
import Language.KansasLava.Testing.Probes
import Language.KansasLava.Testing.Unit

import qualified Data.Map as M
import Data.Sized.Unsigned

import Data.Default
import Data.List

-- TODO: Add Ports/InPorts instance for Signal? Otherwise probe below won't work
-- halfAdder :: (Signal sig) => sig Bool -> sig Bool -> (sig Bool, sig Bool)
halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)
halfAdder a b = (a `xor2` b, probe "and2" and2 a b)

lavaFst :: Seq Bool -> Seq Bool -> Seq Bool
lavaFst a b = a

main = do
    let pv1 = PadVar 0 "bools0"
        pv2 = PadVar 1 "bools1"
        pv3 = PadVar 2 "ints"

        seq1 = toSeq $ cycle [True, False]
        seq2 = toSeq' $ cycle [Nothing, Just True, Just False]
        seq3 :: Seq U4
        seq3 = toSeq' $ cycle $ [Just x | x <- [0..14]] ++ [Nothing]

        -- here is how we build a trace from scratch
        trace = setCycles 100
              $ addInput pv1 seq1
              $ addInput pv2 seq2
              $ addInput pv3 seq3
              $ setOutput seq1
              $ emptyTrace

        thunk = mkThunk (lavaFst :: Seq Bool -> Seq Bool -> Seq Bool) (\f -> f (toSeq $ cycle [True,False]) (toSeq $ cycle [True,True,False,False]))
        limit = Just 100

    writeToFile "test.trace" trace
    newTrace <- readFromFile "test.trace"
    putStrLn "Serialization Test:"
    putStrLn "Before:"
    print trace
    putStrLn "After:"
    print newTrace


    t <- mkTrace limit (halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)) (\h -> h (toSeq $ cycle [True,False]) (toSeq $ cycle [True,True,False,False]))
    t2 <- mkTrace limit (lavaFst :: Seq Bool -> Seq Bool -> Seq Bool) (\f -> f (toSeq $ cycle [True,False]) (toSeq $ cycle [True,True,False,False]))
    t3 <- mkTrace' limit thunk

    putStrLn "lavaFst Result:"
    print $ test lavaFst t2
    putStrLn "halfAdder Result:"
    print $ test halfAdder t
    putStrLn "halfAdder Run:"
    print $ run halfAdder t
    putStrLn "halfAdder Execute:"
    print $ execute halfAdder t

    putStrLn "unit test:"
    res <- unitTest def "halfAdder" thunk
    print res
