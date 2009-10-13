import Language.KansasLava

-- Simple examples

halfAdder :: (Signal Bool,Signal Bool) -> (Signal Bool,Signal Bool)
halfAdder (a,b) = (sum,carry)
  where sum = a `xor2` b
        carry = a `and2` b



main = do
	debugCircuit [] halfAdder
