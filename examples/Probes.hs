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
    -- if KANSAS_LAVA_PROBE=capture
    streams <- probeCircuit 20 dut
    -- you have two options for output, list of probes or a table
    -- we do both here
    printProbes streams
    putStrLn ""
    printProbeTable streams

    -- if KANSAS_LAVA_PROBE=trace
    let (s,c) = fullAdder (toSeq $ cycle [True,False])
                          (toSeq $ cycle [True,True,False,False])
                          (toSeq $ cycle [False,False,False,False,True])
    -- have to force evaluation (traces are lazy too)
    -- print isn't a good way, as the print/trace info is interleaved,
    -- but it's simple. We ascribe the type to 's' to specify the clock
    print (s :: Seq Bool,c)

-- run this file with:
-- KANSAS_LAVA_PROBE=capture ghci -i../ -i../dist/build/autogen Probes.hs

{- ------------------------- Output --------------------------------------------
       0c2: TraceStream B [0b0,0b0,0b0,0b0,0b0,0b0,0b0,0b0,0b0,0b1,0b0,0b0,0b0,0b0,0b1,0b0,0b0,0b0,0b0,0b0]
  2ha1-snd: TraceStream B [0b1,0b0,0b0,0b0,0b1,0b0,0b0,0b0,0b1,0b0,0b0,0b0,0b1,0b0,0b0,0b0,0b1,0b0,0b0,0b0]
  2ha1-fst: TraceStream B [0b0,0b1,0b1,0b0,0b0,0b1,0b1,0b0,0b0,0b1,0b1,0b0,0b0,0b1,0b1,0b0,0b0,0b1,0b1,0b0]
      1ha1: TraceStream B [0b1,0b1,0b0,0b0,0b1,0b1,0b0,0b0,0b1,0b1,0b0,0b0,0b1,0b1,0b0,0b0,0b1,0b1,0b0,0b0]
      0ha1: TraceStream B [0b1,0b0,0b1,0b0,0b1,0b0,0b1,0b0,0b1,0b0,0b1,0b0,0b1,0b0,0b1,0b0,0b1,0b0,0b1,0b0]

clk 0c2   2ha1-snd   2ha1-fst   1ha1   0ha1
0   0b0   0b1        0b0        0b1    0b1
1   0b0   0b0        0b1        0b1    0b0
2   0b0   0b0        0b1        0b0    0b1
3   0b0   0b0        0b0        0b0    0b0
4   0b0   0b1        0b0        0b1    0b1
5   0b0   0b0        0b1        0b1    0b0
6   0b0   0b0        0b1        0b0    0b1
7   0b0   0b0        0b0        0b0    0b0
8   0b0   0b1        0b0        0b1    0b1
9   0b1   0b0        0b1        0b1    0b0
10  0b0   0b0        0b1        0b0    0b1
11  0b0   0b0        0b0        0b0    0b0
12  0b0   0b1        0b0        0b1    0b1
13  0b0   0b0        0b1        0b1    0b0
14  0b1   0b0        0b1        0b0    0b1
15  0b0   0b0        0b0        0b0    0b0
16  0b0   0b1        0b0        0b1    0b1
17  0b0   0b0        0b1        0b1    0b0
18  0b0   0b0        0b1        0b0    0b1
19  0b0   0b0        0b0        0b0    0b0
----------------------------------------------------------------------------- -}
