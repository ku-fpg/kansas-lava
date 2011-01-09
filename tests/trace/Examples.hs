import Language.KansasLava
import Language.KansasLava.Testing

import Control.Monad
import System.Cmd

import Modelsim

halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)
halfAdder a b = (probe "and2" and2 a b, xor2 a $ probe "b" b)

fullAdder :: Seq Bool -> Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)
fullAdder a b cin = (carry, sum)
    where (c1, s1) = (probe "ha1" halfAdder) a b
          (c2, sum) = halfAdder cin s1
          carry = xor2 c1 c2

stupid :: Seq Bool -> Seq Bool
stupid s = probe "a" $ probe "b" $ probe "c" s

s1 = toSeq $ cycle [True, False]
s2 = toSeq $ cycle [True, True, False, False]
s3 = toSeq $ cycle $ (replicate 4 True) ++ (replicate 4 False)

main = do
    rc <- reifyCircuit fullAdder
    writeDotCircuit "fa.dot" $ mergeProbes rc
    system "dot -Tpng fa.dot > fa.png"
    print rc
{-
    rc' <- reifyCircuit $ stupid
    writeDotCircuit "s.dot" rc'
    system "dot -Tpng s.dot > s.png"
    writeDotCircuit "sprime.dot" $ remProbes rc'
    system "dot -Tpng sprime.dot > sprime.png"
    print rc'

-}
    let thunk = Thunk fullAdder (\circuit -> circuit s1 s2 s3)
        cMod = (writeDotCircuit "exposeb.dot") >=> (exposeProbesIO ["ha1"]) >=> (writeDotCircuit "exposea.dot")

    trace <- recordThunk "test/fullAdder" 16 cMod thunk
    print trace
    system "dot -Tpng exposeb.dot > exposeb.png"
    system "dot -Tpng exposea.dot > exposea.png"
--    runTestBench "test/fullAdder" modelsim
