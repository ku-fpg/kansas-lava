import Language.KansasLava
import Language.KansasLava.Testing

import qualified Data.Map as M
import Data.Sized.Unsigned

import Data.Default
import Data.List

import Data.Word
import Data.Sized.Ix
import Data.Sized.Arith


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


-- Andy's tests
-- Can be moved or removed

    do -- hack to get tabbed do


	-- The lavaId is because we can not annotate funMap
	let thunk5 = Thunk (lavaId . funMap (\ x -> return (x + 1)) . lavaId :: Seq Word8 -> Seq Word8)
		       (\ f -> f (toSeq [1..100]))

	print $ runT thunk5
	trace5 <- mkTrace (return 200) thunk5
	print $ trace5

	let thunk6 = Thunk (pipeToMemory :: Env () -> Env () -> Seq (Pipe Word8 Int) -> Seq Word8 -> Seq Int)
	       $ \ cir -> cir shallowEnv 
			      shallowEnv 
			      (toSeq $ [Nothing,Nothing,Nothing,Nothing,Nothing]
				    ++ [ return (0,100)
				       , return (1,101)
				       , return (2,102)
				       , return (3,103)
				       , return (4,104)
			       	       ]
			 	    ++ cycle [Nothing])
			      (toSeq $ cycle [0..2])

	print $ runT thunk6
	trace6 <- mkTrace (return 20) thunk6
        print $ trace6

    return ()
