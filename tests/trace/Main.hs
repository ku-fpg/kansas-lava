import Language.KansasLava
import Language.KansasLava.Testing

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
    let pv1 = OVar 0 "bools0"
        pv2 = OVar 1 "bools1"
        pv3 = OVar 2 "ints"

        seq1 = toSeq $ cycle [True, False]
        seq2 = toSeq' $ cycle [Nothing, Just True, Just False]
        seq3 :: Seq U4
        seq3 = toSeq' $ cycle $ [Just x | x <- [0..14]] ++ [Nothing]

        inp :: Seq U4
        inp  = toSeq $ cycle [0..15]
        inp2 :: Seq U4
        inp2 = toSeq $ cycle $ reverse [0..15]

        -- here is how we build a trace from scratch
        trace = setCycles 100
              $ addInput pv1 seq1
              $ addInput pv2 seq2
              $ addInput pv3 seq3
              $ setOutput seq1
              $ emptyTrace

        thunk = Thunk (lavaFst :: Seq Bool -> Seq Bool -> Seq Bool) (\f -> f (toSeq $ cycle [True,False]) (toSeq $ cycle [True,True,False,False]))
        thunk2 = Thunk (halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)) (\h -> h (toSeq $ cycle [True,False]) (toSeq $ cycle [True,True,False,False]))
        muxt = Thunk (mux2 :: Seq Bool -> (Seq U4, Seq U4) -> Seq U4) (\m -> m (toSeq (cycle [True,False,True,True,False])) (inp, inp2))
        limit = Just 100

    writeToFile "test.trace" trace
    newTrace <- readFromFile "test.trace"
    putStrLn "Serialization Test:"
    putStrLn "Before:"
    print trace
    putStrLn "After:"
    print newTrace

    t <- mkTrace limit thunk2
    t2 <- mkTrace limit $ Thunk (lavaFst :: Seq Bool -> Seq Bool -> Seq Bool) (\f -> f (toSeq $ cycle [True,False]) (toSeq $ cycle [True,True,False,False]))
    t3 <- mkTrace limit thunk

    -- TODO: Fix mkThunk to handle -> mkThunk t halfAdder :: Thunk (Seq Bool, Seq Bool)
    putStrLn "mkThunk test:"
    print $ runT (mkThunk t2 lavaFst :: Thunk (Seq Bool))

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

    mkTarball "test/lavaFst" 100 thunk
    mkTarball "test/halfAdder" 100 thunk2

    t4 <- mkTrace limit muxt
    mkTarball "test/mux2" 100 muxt
