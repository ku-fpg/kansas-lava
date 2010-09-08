{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.VHDL(writeVhdlCircuit) where


-- import qualified Language.KansasLava.Entity as E
import Language.KansasLava.Reify(reifyCircuit,Ports)
import Language.KansasLava.Types
import Language.KansasLava.Entity

import Language.KansasLava.Netlist
import Language.KansasLava.Netlist.Utils
import Language.Netlist.GenVHDL

-- | The 'vhdlCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
{-
vhdlCircuit' :: (Ports o)
---               [CircuitOptions] -- ^ Options for controlling the observable-sharing reification.
            -> String         -- ^ The name of the generated entity.
	    -> [String]	      -- ^ The extra module arguments needed
            -> o              -- ^ The Lava circuit.
            -> IO String
vhdlCircuit' nlOpts name mods circuit = do
  mod <- netlistCircuit [] nlOpts name circuit
  return $ genVHDL mod mods
-}

writeVhdlCircuit :: [String] -> String -> FilePath -> Circuit -> IO ()
writeVhdlCircuit mods nm file cir = do
	mod <- netlistCircuit nm cir
	writeFile file (genVHDL mod mods)

