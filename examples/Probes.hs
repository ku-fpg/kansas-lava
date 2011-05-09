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
    putStrLn ""
    printProbeTable streams

{- ------------------------- Output --------------------------------------------
     c2$0: TraceStream B [0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0]
ha1-snd$2: TraceStream B [1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0]
ha1-fst$2: TraceStream B [0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0]
    ha1$1: TraceStream B [1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0]
    ha1$0: TraceStream B [1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0]

clk c2$0 ha1-snd$2 ha1-fst$2 ha1$1 ha1$0
0   0    1         0         1     1
1   0    0         1         1     0
2   0    0         1         0     1
3   0    0         0         0     0
4   0    1         0         1     1
5   0    0         1         1     0
6   0    0         1         0     1
7   0    0         0         0     0
8   0    1         0         1     1
9   1    0         1         1     0
10  0    0         1         0     1
11  0    0         0         0     0
12  0    1         0         1     1
13  0    0         1         1     0
14  1    0         1         0     1
15  0    0         0         0     0
16  0    1         0         1     1
17  0    0         1         1     0
18  0    0         1         0     1
19  0    0         0         0     0
----------------------------------------------------------------------------- -}
