-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.VHDL(writeVhdlCircuit) where


import Language.KansasLava.Types(Circuit)
import Language.KansasLava.Netlist

import Language.Netlist.GenVHDL


-- | The 'vhdlCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
writeVhdlCircuit :: [String] -> String -> FilePath -> Circuit -> IO ()
writeVhdlCircuit mods nm file cir = do
	nlMod <- netlistCircuit nm cir
	writeFile file (genVHDL nlMod mods)

