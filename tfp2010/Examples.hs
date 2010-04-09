module Examples where

import Language.KansasLava
import Language.KansasLava.Probes


halfAdder a b = (sum,carry)
  where sum = xor2 a b
        carry = and2 a b


fullAdder a b cin = (sum,cout)
  where (s1,c1) = halfAdder a b
        (sum,c2) = halfAdder cin s1
        cout = xor2 c1 c2

fullAdder' a b cin = (sum,cout)
  where (s1,c1) = halfAdder a b
        (sum,c2) = halfAdder cin (probe "s1" s1)
        cout = xor2 (probe "c1" c1) (probe "c2" c2)





test = do
  probes <- probeCircuit $ fullAdder' false false false
  case (getProbe probes "s1") of
    Just (ProbeValue _ xstrm) -> return $ showXStream xstrm

fullAdder'' a b cin = (sum,cout)
  where (s1,c1) = (probe "h1" halfAdder) a a
        (sum,c2) = halfAdder cin s1
        cout = xor2 c1 c2

test' = do
  probes <- probeCircuit $ fullAdder'' false false false
  mapM_ printProbe probes
 where printProbe (i, (ProbeValue name xstrm)) = do
          putStr $ name ++ ": "
          putStrLn $ show $ showXStream xstrm

