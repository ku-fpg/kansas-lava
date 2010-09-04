{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.Netlist where


-- import Language.KansasLava hiding (Seq)
-- import qualified Language.KansasLava as KL
import qualified Language.KansasLava.Entity as E
import qualified Language.KansasLava.Seq as KL
import Language.KansasLava.Comb
import Language.KansasLava.Utils
import Language.KansasLava.Reify(reifyCircuit,Ports)
import Language.KansasLava.Circuit
import Language.KansasLava.Entity
import Language.KansasLava.Types.Type

import Data.Sized.Unsigned

import Data.Reify.Graph

import Text.PrettyPrint
import Data.List(intersperse,find,mapAccumL,nub,sort)
import Data.Maybe(fromJust)
import qualified Data.Map as M

import Debug.Trace



import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL

import Language.KansasLava.Netlist.Utils
import Language.KansasLava.Netlist.Decl
import Language.KansasLava.Netlist.Inst
import Language.KansasLava.Netlist.Sync

----------------------------------------------------------------------------------------------

-- | The 'netlistCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
netlistCircuit :: (Ports o) =>
               [CircuitOptions] -- ^ Options for controlling the observable-sharing reification.
            -> NetlistOptions -- ^ Options for controlling netlist generation
            -> String         -- ^ The name of the generated entity.
            -> o              -- ^ The Lava circuit.
            -> IO Module
netlistCircuit opts nlOpts name circuit = do
  rc <- reifyCircuit (opts {- ++ [DebugReify] -}) circuit
  netlistCircuit' opts nlOpts name rc

-- This interface is used by the probe tools.
netlistCircuit' :: [CircuitOptions] -- ^ Options for controlling the observable-sharing reification.
                -> NetlistOptions -- ^ Options for controlling netlist generation
                -> String         -- ^ The name of the generated entity.
                -> Circuit -- ^ The Lava circuit.
                -> IO Module
netlistCircuit' opts nlOpts name circuit = do
  let (Circuit nodes srcs sinks) = circuit

  let loadEnable = if addEnabled nlOpts then [("enable",Nothing)] else []
	         -- need size info for each input, to declare length of std_logic_vector
  let inports = loadEnable ++ [ (nm,sizedRange ty) | (OVar _ nm, ty) <- sort srcs]
                 -- need size info for each output, to declare length of std_logic_vector
  let outports = [ (nm,sizedRange ty) | (OVar _ nm,ty,_) <- sort sinks]

  let finals = [ NetAssign n (toStdLogicExpr ty x) | (OVar _ n,ty,x) <- sort sinks ]

  let mod = Module name inports outports []
		(concatMap genDecl nodes ++
		 concatMap (uncurry genInst) nodes ++
--		 concatMap (uncurry (genSync nlOpts)) nodes ++
		genSync nlOpts nodes ++
		 finals)
  return mod


----------------------------------------------------------------------------------------------

