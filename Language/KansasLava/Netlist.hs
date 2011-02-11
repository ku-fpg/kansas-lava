{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.Netlist where


-- import Language.KansasLava.Entity
import Language.KansasLava.Types



import Data.List(sort)
import qualified Data.Map as M




import Language.Netlist.AST

import Language.KansasLava.Netlist.Utils
import Language.KansasLava.Netlist.Decl
import Language.KansasLava.Netlist.Inst

----------------------------------------------------------------------------------------------

-- | The 'netlistCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
{-
netlistCircuit' :: (Ports o) =>
--               [CircuitOptions] -- ^ Options for controlling the observable-sharing reification.
              String         -- ^ The name of the generated entity.
            -> o              -- ^ The Lava circuit.
            -> IO Module
netlistCircuit' name circuit = do
  rc <- reifyCircuit {- ++ [DebugReify] -} circuit
  netlistCircuit name rc
-}

-- This interface is used by the probe tools.
netlistCircuit :: String         -- ^ The name of the generated entity.
               -> Circuit 	 -- ^ The Lava circuit.
               -> IO Module
netlistCircuit name circuit = do
  let (Circuit nodes srcs sinks) = circuit

  let loadEnable = [] -- if addEnabled nlOpts then [("enable",Nothing)] else []
	         -- need size info for each input, to declare length of std_logic_vector
  let inports = [ (nm,sizedRange ty) | (OVar _ nm, ty) <- sort srcs
                                     , case toStdLogicTy ty of
                                           MatrixTy {} -> error "can not have a matrix as an in argument"
                                           _ -> True
                 ]
                 -- need size info for each output, to declare length of std_logic_vector
  let outports = [ (nm,sizedRange ty) | (OVar _ nm,ty,_) <- sort sinks
                                      , case toStdLogicTy ty of
                                           MatrixTy {} -> error "can not have a matrix as an out argument"
                                           _ -> True
                 ]

  let finals = [ NetAssign n (toStdLogicExpr ty x) | (OVar _ n,ty,x) <- sort sinks
                                                   , case toStdLogicTy ty of
                                                        MatrixTy {} -> error "can not have a matrix as an out argument"
                                                        _ -> True
               ]

  let env = M.fromList nodes

  let mod = Module name inports outports []
		(concatMap genDecl nodes ++
		 concatMap (uncurry (genInst' env)) nodes ++
--		genSync nodes ++
		 finals)
  return mod



----------------------------------------------------------------------------------------------

