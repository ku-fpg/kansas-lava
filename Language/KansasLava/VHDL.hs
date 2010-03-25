{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.VHDL(vhdlCircuit) where


-- import qualified Language.KansasLava.Entity as E
import Language.KansasLava.Reify(reifyCircuit,Ports)
import Language.KansasLava.Circuit(ReifyOptions(..),ReifiedCircuit(..))
import Language.KansasLava.Entity hiding (name)



import Text.PrettyPrint(render)

import Language.KansasLava.Netlist
import Language.Netlist.GenVHDL

-- | The 'vhdlCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
vhdlCircuit :: (Ports o) =>
               [ReifyOptions] -- ^ Options for controlling the observable-sharing reification.
            -> [NetlistOption] -- ^ Options for controlling the netlist generation.
            -> String         -- ^ The name of the generated entity.
            -> o              -- ^ The Lava circuit.
            -> IO String
vhdlCircuit opts nlOpts name circuit = do
  mod <- netlistCircuit opts nlOpts name circuit
  return $  render $ genVHDL  mod










