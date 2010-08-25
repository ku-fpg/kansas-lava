import Language.KansasLava hiding (output)
import Language.KansasLava.Testing.Probes
import Language.KansasLava.Trace

import qualified Data.Map as M
import Data.Sized.Unsigned

import Data.List

-- TODO: Add Ports/InPorts instance for Signal? Otherwise probe below won't work
-- halfAdder :: (Signal sig) => sig Bool -> sig Bool -> (sig Bool, sig Bool)
halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)
halfAdder a b = (a `xor2` b, probe "and2" and2 a b)

lavaFst :: Seq Bool -> Seq Bool -> Seq Bool
lavaFst a b = a

main = do
    let pv1 = PadVar 0 "blah"
        pv2 = PadVar 1 "bleh"
        ov1 = PadVar 0 "out1"
        ov2 = PadVar 99 "out2"

        seq1 = toSeq $ cycle [True, False]
        seq2 = toSeq' $ cycle [Nothing, Just True, Just False]
        seq3 :: Seq U4
        seq3 = toSeq' $ cycle $ [Just x | x <- [0..14]] ++ [Nothing]

        inmap = addSeq pv1 seq1 $ addSeq pv2 seq2 $ addSeq (PadVar 3 "ints") seq3 $ M.empty
        out = (B,fromXStream witness (seqValue seq1))

        trace = Trace { cycles = 100, inputs = inmap, output = out, probes = M.empty } -- , circuit = ReifiedCircuit [] [] [] }

        witness = error "witness" :: Bool
        witness2 = error "witness" :: U4

    print trace
    print $ trace == trace
    print $ trace == Trace {cycles = 100, inputs = M.fromList (tail (M.toList inmap)), output = out, probes = M.empty}
    print $ trace == Trace {cycles = 100, inputs = inmap, output = (B,fromXStream witness (seqValue seq2)), probes = M.empty}
    print $ trace == trace {cycles = 99}

    writeToFile "test.trace" trace
    newTrace <- readFromFile "test.trace"

    print newTrace

    t <- mkTrace 100 (halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)) (\h -> h (toSeq $ cycle [True,False]) (toSeq $ cycle [True,True,False,False]))
    print t
    t2 <- mkTrace 100 (lavaFst :: Seq Bool -> Seq Bool -> Seq Bool) (\f -> f (toSeq $ cycle [True,False]) (toSeq $ cycle [True,True,False,False]))
    print t2

--    why don't streams with Nothings work?
    print $ delay shallowEnv $ (getSeq pv1 (inputs trace) witness :: Seq Bool)
    print $ (getSeq (PadVar 3 "ints") (inputs newTrace) witness2 :: Seq U4)
