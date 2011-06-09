-- Example for building a fabric and generating VHDL/Dot output of a circuit.
-- You are highly encouraged to run both main and main2 and view the VHDL output.
import Language.KansasLava

-- for dut2
import Data.Sized.Unsigned

-- define a circuit
halfAdder a b = (sum,carry)
  where sum = xor2 a b
        carry = and2 a b

fullAdder a b cin = (sum,cout)
  where (s1,c1) = halfAdder a b
        (sum,c2) = halfAdder cin s1
        cout = xor2 c1 c2

-- turn it into a fabric
-- inStdLogic is like the vhdl std_logic, which is a wire that can be either high or low
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

--------------------------------------------------------------------------------

-- inStdLogicVector is a group of wires representing some type 'a'
-- You must provide enough type ascriptions that the types of inputs and
-- outputs can be deduced by the type checker. Here, since 'register' has
-- the type signature:
--
--      register :: (Rep a, Clock clk) => a -> CSeq clk a -> CSeq clk a
--
-- Ascribing the type 'U4' to the first argument is sufficient to deduce
-- the types of 'inp' and 'out' (which must be "CSeq clk U4")
dut2 = do
    inp <- inStdLogicVector "in"
    let out = register (4 :: U4) inp
    outStdLogicVector "out" out

main2 = do
    kleg <- reifyFabric dut2
    writeVhdlCircuit "u4reg" "u4reg.vhdl" kleg

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
