-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.VHDL(writeVhdlCircuit) where


import Language.KansasLava.Types(Circuit)
import Language.KansasLava.Netlist

import Language.Netlist.GenVHDL


-- | The 'vhdlCircuit' function converts a Lava Circuit into a VHDL entity/architecture pair.
writeVhdlCircuit :: String -> FilePath -> Circuit -> IO ()
writeVhdlCircuit nm file cir = do
	nlMod <- netlistCircuit nm cir
	writeFile file (genVHDL nlMod mods)
    where
        -- we always use the following 'use' statements.
        mods = ["work.lava.all","work.all"]
