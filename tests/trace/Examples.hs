import Language.KansasLava
import Language.KansasLava.Testing

import Control.Monad

import Modelsim

halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)
halfAdder a b = (probe "and2" and2 a b, xor2 a $ probe "b" b)

fullAdder :: Seq Bool -> Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool)
fullAdder a b cin = (carry, sum)
    where (c1, s1) = halfAdder a b
          (c2, sum) = halfAdder cin s1
          carry = xor2 c1 c2

s1 = toSeq $ cycle [True, False]
s2 = toSeq $ cycle [True, True, False, False]
s3 = toSeq $ cycle $ (replicate 4 True) ++ (replicate 4 False)

main = do
    let thunk = Thunk fullAdder (\circuit -> circuit s1 s2 s3)

    trace <- recordThunk "test/fullAdder" 16 (exposeProbes ["and2"]) thunk
    print trace
    runTestBench "test/fullAdder" modelsim
