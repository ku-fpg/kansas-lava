module Language.KansasLava.Circuit 
	( Circuit(..)
	, CircuitOptions(..)
	, DepthOp(..)
	) where


import Data.Reify
import Data.List as L
import qualified Data.Map as Map

import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Types.Type

import Debug.Trace


data Circuit = Circuit
	{ theCircuit :: [(Unique,MuE Unique)]
		-- ^ This the main graph. There is no actual node for the source or sink.
	, theSrcs    :: [(OVar,Type)]
	, theSinks   :: [(OVar,Type,Driver Unique)]
	-- , theTypes   :: TypeEnv
	}


instance Show Circuit where
   show rCir = msg
     where
	showDriver d t = show d ++ " : " ++ show t
	
	bar = (replicate 78 '-') ++ "\n"

	inputs = unlines
		[ show var ++ " : " ++ show ty
		| (var,ty) <- theSrcs rCir
		]
	outputs = unlines
		[ show var   ++ " <- " ++ showDriver dr ty
		| (var,ty,dr) <- theSinks rCir
		]
	circuit = unlines
		[ case e of
		    Entity nm outs ins ann ->
			"(" ++ show uq ++ ") " ++ show nm ++ "\n"
			    ++ unlines [ "      out    " ++ show v ++ ":" ++ show ty | (v,ty) <- outs ]
 			    ++ unlines [ "      in     " ++ show v ++ " <- " ++ showDriver dr ty | (v,ty,dr) <- ins ]
 			    ++ unlines [ "      probes " ++ intercalate ", " [name ++ "_" ++ show i | ProbeValue (OVar i name) _ <- ann ] ]
			    ++ unlines [ "      comment " ++ str | Comment str <- ann ]
		    Table (v0,ty0) (v1,ty1,dr) mapping ->
			"(" ++ show uq ++ ") TABLE \n"
			    ++ "      out " ++ show v0 ++ ":" ++ show ty0 ++ "\n"
			    ++ "      in  " ++ show v1 ++ " <- " ++ showDriver dr ty1 ++ "\n"
			    ++ unlines [ "      case " ++ e1 ++ " -> " ++ e2
				       | (i,e1,o,e2) <- mapping
				       ]
		| (uq,e) <- theCircuit rCir
		]

	msg = bar
		++ "-- Inputs                                                                   --\n"
		++ bar
		++ inputs
		++ bar
		++ "-- Outputs                                                                  --\n"
		++ bar
		++ outputs
		++ bar
		++ "-- Entities                                                                 --\n"
		++ bar
		++ circuit
		++ bar

-------------------------------------------------------------------------

data CircuitOptions
	= DebugReify		-- ^ show debugging output of the reification stage
	| OptimizeReify		-- ^ perform basic optimizations
	| NoRenamingReify	-- ^ do not use renaming of variables
	| CommentDepth 
	      [(Name,DepthOp)] 	-- ^ add comments that denote depth
	deriving (Eq, Show)

-- Does a specific thing 
data DepthOp = AddDepth Float
	     | NewDepth Float
	deriving (Eq, Show)

