module Language.KansasLava.Circuit.Depth
	( DepthOp(..)
	, findChains
	, depthTable
	) where


import Data.Reify
import Data.List as L
import qualified Data.Map as Map

import Language.KansasLava.Types


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

		findEntityChain :: Entity Unique -> Float
		findEntityChain (Entity nm _ ins) =
			plus (maximum [ findDriverChain d | (_,_,d) <- ins ])
		   where plus = 
		           case nm of
                             Comment' {} -> (+ 0)
                             Label {} -> (+ 0)
		             _ -> case lookup nm fn of
			            Nothing -> (+ 1)
			            Just f -> addDepthOp f

		findDriverChain :: Driver Unique -> Float
		findDriverChain (Port _ u) =
			case Map.lookup u res of
			  Nothing -> error $ "Can not find " ++ show u
			  Just i -> i
		findDriverChain (Pad _)     = 0
		findDriverChain (Lit _)     = 0
                findDriverChain (Lits _)    = 0
		findDriverChain (Generic _) = 0
                findDriverChain (Error err) = error $ "Error: " ++ show err
                findDriverChain (ClkDom nm) = error $ "ClkDom: " ++ show nm


depthTable :: [(Id,DepthOp)]
depthTable = doubleUp
	[ (Name "Memory" "register",NewDepth 0)
	, (Name "Memory" "delay", NewDepth 0)
	, (Name "Memory" "RAM", NewDepth 0)

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
	, (Name "Lava" "toStdLogicVector", AddDepth 0)
	, (Name "Lava" "fromStdLogicVector", AddDepth 0)
	, (Name "Lava" "coerceStdLogicVector", AddDepth 0)
	, (Name "Lava" "spliceStdLogicVector", AddDepth 0)
        ]
  where doubleUp xs = concat [  [n, (Prim x,d)] | n@(Name _ x,d) <- xs ]

data DepthOp = AddDepth Float
             | NewDepth Float
        deriving (Eq, Show)


{-
addDepthComment :: [(Id,DepthOp)] -> Circuit -> Circuit
addDepthComment depths cir =
        cir {  theCircuit = [ (u,case e of
                                   Entity nm ins outs ann ->
                                      case Map.lookup u env of
                                          Nothing -> e
                                          Just d -> Entity nm ins outs (ann ++ [Comment $ "depth: " ++ show d])
                                                          _ -> e)
                                                     | (u,e) <- theCircuit rCit2
                                                     ]}

                                    return $ rCit2 { theCircuit = [ (u,case e of
                                                          Entity nm ins outs ann ->
                                                                case Map.lookup u env of
                                                                  Nothing -> e
                                                                  Just d -> Entity nm ins outs (ann ++ [Comment $ "depth: " ++ show d])
                                                          _ -> e)
                                                     | (u,e) <- theCircuit rCit2
                                                     ]}

        -- This maps original names to 
        env = Map.fromList [ (u,d) | (d,u) <- concat chains ]
        chains = findChains depths cir

-}
