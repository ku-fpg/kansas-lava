module Language.KansasLava.Circuit.Depth
	( DepthOp(..)
	, findChains
	, depthTable
	) where

import Language.KansasLava.Entity
import Language.KansasLava.Circuit

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

-- assumes no bad loops.


addDepthOp :: DepthOp -> Float -> Float
addDepthOp (AddDepth n) m = n + m
addDepthOp (NewDepth n) _ = n

findChains :: [(Id,DepthOp)] -> Circuit -> [[(Float,Unique)]]
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
			          Just f -> addDepthOp f
		findEntityChain (Table _ (_,_,d) _) = plus (findDriverChain d)
		   where plus = case lookup (Name "Lava" "table") fn of
			          Nothing -> (+ 1)
			          Just f -> addDepthOp f

		findDriverChain :: Driver Unique -> Float
		findDriverChain (Port _ u) =
			case Map.lookup u res of
			  Nothing -> error $ "Can not find " ++ show u
			  Just i -> i
		findDriverChain (Pad _) = 0
		findDriverChain (Lit _) = 0
		findDriverChain (Error err) = error $ "Error: " ++ show err

depthTable :: [(Id,DepthOp)]
depthTable =
	[ (Name "Memory" "register",NewDepth 0)
	, (Name "Memory" "BRAM", NewDepth 0)

	-- These are just pairing/projecting, and cost nothing in the final code
	, (Name "Lava" "fst", AddDepth 0)
	, (Name "Lava" "snd", AddDepth 0)
	, (Name "Lava" "pair", AddDepth 0)
	, (Name "Lava" "concat", AddDepth 0)
	, (Name "Lava" "index", AddDepth 0)
	, (Name "Lava" "id", AddDepth 0)
	, (Name "StdLogicVector" "toStdLogicVector", AddDepth 0)
	, (Name "StdLogicVector" "fromStdLogicVector", AddDepth 0)
	, (Name "StdLogicVector" "coerceStdLogicVector", AddDepth 0)
	, (Name "StdLogicVector" "spliceStdLogicVector", AddDepth 0)
        ]
