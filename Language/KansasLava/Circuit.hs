module Language.KansasLava.Circuit where

import Data.Reify
import Data.List as L
import qualified Data.Map as Map

import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
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
	{ theCircuit :: [(Unique,MuE Unique)]
		-- ^ This the main graph. There is no actual node for the source or sink.
	, theSrcs    :: [(PadVar,BaseTy)]
	, theSinks   :: [(PadVar,BaseTy,Driver Unique)]
	-- , theTypes   :: TypeEnv
	}


data ReifyOptions
	= InputNames [String]
	| OutputNames [String]
	| DebugReify		-- show debugging output of the reification stage
	| OptimizeReify
	| NoRenamingReify	-- do not use renaming of variables
	deriving (Eq, Show)



showDriver :: Driver Unique -> BaseTy -> String
showDriver (Port v i) ty = show i ++ "." ++ show v ++ ":" ++ show ty
showDriver (Lit x) ty = show x ++ ":" ++ show ty
showDriver (Pad (PadVar n x)) ty = show x ++ "<" ++ show n ++ ">" ++ ":" ++ show ty
showDriver (Error msg) ty = show msg ++ ":" ++ show ty
showDriver l _ = error $ "showDriver: " ++ show l

instance Show ReifiedCircuit where
   show rCir = msg
     where
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
		    Entity nm outs ins _	 ->
			"(" ++ show uq ++ ") " ++ show nm ++ "\n"
			    ++ unlines [ "      out " ++ show v ++ ":" ++ show ty | (v,ty) <- outs ]
 			    ++ unlines [ "      in  " ++ show v ++ " <- " ++ showDriver dr ty | (v,ty,dr) <- ins ]
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
-- 		++ "-- Types                                                                    --\n"
-- 		++ bar
-- 		++ types
-- 		++ bar
		++ "-- Entities                                                                 --\n"
		++ bar
		++ circuit
		++ bar


-- Not the correct place for this!
-- assumes no bad loops.

-- Use lava table to change tables
findChains :: [(Name,Float -> Float)] -> ReifiedCircuit -> [[(Float,Unique)]]
findChains fn cir = reverse
		$ L.groupBy (\ x y -> fst x == fst y)
		$ L.sort
		$ [ (b,a) | (a,b) <- Map.toList res ]
	where
		res = Map.map findEntityChain $ Map.fromList $ theCircuit cir

		findEntityChain :: MuE Unique -> Float
		findEntityChain (Entity nm _ ins _) =
			plus (maximum [ findDriverChain d | (_,_,d) <- ins ])
		   where plus = case lookup nm fn of
			          Nothing -> (+ 1)
			          Just f -> f
		findEntityChain (Table _ (_,_,d) _) = plus (findDriverChain d)
		   where plus = case lookup (Name "Lava" "table") fn of
			          Nothing -> (+ 1)
			          Just f -> f

		findDriverChain :: Driver Unique -> Float
		findDriverChain (Port _ u) =
			case Map.lookup u res of
			  Nothing -> error $ "Can not find " ++ show u
			  Just i -> i
		findDriverChain (Pad _) = 0
		findDriverChain (Lit _) = 0
		findDriverChain (Error err) = error $ "Error: " ++ show err
