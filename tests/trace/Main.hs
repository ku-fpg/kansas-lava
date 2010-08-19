import Language.KansasLava
import Language.KansasLava.Testing.Probes
import Language.KansasLava.Trace

import qualified Data.Map as M
import Data.Sized.Unsigned

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
        outmap = addSeq ov1 seq1 $ addSeq ov2 seq2 M.empty

        trace = Trace { cycles = 100, inputs = inmap, outputs = outmap, probes = M.empty } -- , circuit = ReifiedCircuit [] [] [] }

        witness = error "witness" :: Bool
        witness2 = error "witness" :: U4

    print trace
    print $ trace == trace
    print $ trace == Trace {cycles = 100, inputs = outmap, outputs = inmap, probes = M.empty}
    print $ trace == trace {cycles = 99}

--    why don't streams with Nothings work?
    print $ delay shallowEnv $ (getSeq pv1 (inputs trace) witness :: Seq Bool)
    print $ (getSeq (PadVar 3 "ints") (inputs trace) witness2 :: Seq U4)

