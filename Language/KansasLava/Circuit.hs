module Language.KansasLava.Circuit where

import Data.Reify
import Data.List as L

import Language.KansasLava.Entity
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Type

import Debug.Trace

--------------------------------------------------------
-- Grab a set of drivers (the outputs), and give me a graph, please.

data Uq = Uq Unique | Sink | Source
	deriving (Eq,Ord,Show)

data ReifiedCircuit = ReifiedCircuit
	{ theCircuit :: [(Unique,Entity BaseTy Unique)]
		-- ^ This the main graph. There is no actual node for the source or sink.
	, theSrcs    :: [(Var,BaseTy)]
	, theSinks   :: [(Var,BaseTy,Driver Unique)]
	-- , theTypes   :: TypeEnv
	}


data ReifyOptions
	= InputNames [String]
	| OutputNames [String]
	| DebugReify		-- show debugging output of the reification stage
	| OptimizeReify
	| NoRenamingReify	-- do not use renaming of variables
	deriving (Eq, Show)

