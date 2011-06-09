import Language.KansasLava

-- define a circuit
halfAdder a b = (sum,carry)
  where sum = xor2 a b
        carry = and2 a b

fullAdder a b cin = (sum,cout)
  where (s1,c1) = halfAdder a b
        (sum,c2) = halfAdder cin s1
        cout = xor2 c1 c2

-- turn it into a fabric
dut = do
    a <- inStdLogic "a"
    b <- inStdLogic "b"
    cin <- inStdLogic "cin"
    let (sum,cout) = fullAdder a b cin
    outStdLogic "sum" sum
    outStdLogic "cout" cout

main = do
    -- reify the circuit into a kansas lava entity graph (kleg)
    kleg <- reifyFabric dut

    -- write out a vhdl file, given entity name, file name, and the kleg
    writeVhdlCircuit "fullAdder" "fullAdder.vhdl" kleg

    -- write out a dot file of the circuit, on linux, view with:
    --    dot -Tpng fullAdder.dot > fullAdder.png
    -- assuming you have dot tools installed
    writeDotCircuit "fullAdder.dot" kleg

    -- just print out the kleg, output shown below
    print kleg

{- ------------------------- Output --------------------------------------------

------------------------------------------------------------------------------
-- Inputs                                                                   --
------------------------------------------------------------------------------
a$0 : B
b$0 : B
cin$0 : B
------------------------------------------------------------------------------
-- Outputs                                                                  --
------------------------------------------------------------------------------
sum$0 <- (2).o0 : B
cout$0 <- (4).o0 : B
------------------------------------------------------------------------------
-- Entities                                                                 --
------------------------------------------------------------------------------
(4) xor2
      out    o0:B
      in     i0 <- (5).o0 : B
      in     i1 <- (6).o0 : B

(6) and2
      out    o0:B
      in     i0 <- cin$0 : B
      in     i1 <- (3).o0 : B

(5) and2
      out    o0:B
      in     i0 <- a$0 : B
      in     i1 <- b$0 : B

(2) xor2
      out    o0:B
      in     i0 <- cin$0 : B
      in     i1 <- (3).o0 : B

(3) xor2
      out    o0:B
      in     i0 <- a$0 : B
      in     i1 <- b$0 : B

------------------------------------------------------------------------------

----------------------------------------------------------------------------- -}
