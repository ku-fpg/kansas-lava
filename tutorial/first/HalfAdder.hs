import Language.KansasLava
 
halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool,Seq Bool)
halfAdder a b = (carry,sum)
 where carry = and2 a b
       sum   = xor2 a b

example1 :: Seq Bool
example1 = toSeq [True,False,True,False]

example2 :: (Seq Bool,Seq Bool)
example2 = halfAdder (toSeq [True,False,True,False])
		     (toSeq [True,True,False,False])
		
example3 :: IO ()
example3 = reifyCircuit halfAdder >>= writeDotCircuit "x.dot"
	
	