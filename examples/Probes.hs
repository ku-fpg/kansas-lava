import Language.KansasLava

-- define a circuit
halfAdder a b = (sum,carry)
  where sum = xor2 a b
        carry = and2 a b

fullAdder a b cin = (sum,cout)
  where (s1,c1) = probe "ha1" halfAdder a b -- probe an entire function
        (sum,c2) = halfAdder cin s1
        cout = xor2 c1 (probe "c2" c2)      -- probe a single sequence

-- turn it into a fabric
dut = do
    let a = toSeq $ cycle [True,False]
        b = toSeq $ cycle [True,True,False,False]
        cin = toSeq $ cycle [False,False,False,False,True]
        (sum,cout) = fullAdder a b cin
    outStdLogic "sum" sum
    outStdLogic "cout" cout

-- get the first 20 values of each probe and print it
-- printProbes -> horizontal, one probe per line
-- printProbeTable -> vertical, cycle count and one probe per column
main = do
    streams <- probeCircuit 20 dut
    printProbes streams
    printProbeTable streams
