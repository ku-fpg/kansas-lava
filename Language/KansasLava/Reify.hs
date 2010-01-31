{-# LANGUAGE TypeFamilies, FlexibleInstances,ParallelListComp #-}
module Language.KansasLava.Reify where

import Data.Reify
import Data.List as L


import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Sequential(Time(..))

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
	deriving (Eq, Show)


-- | reifyCircuit does reification and type inference.
-- reifyCircuit :: REIFY circuit => [ReifyOptions] -> circuit -> IO ReifiedCircuit
-- ([(Unique,Entity (Ty Var) Unique)],[(Var,Driver Unique)])
reifyCircuit :: (Ports a) => [ReifyOptions] -> a -> IO ReifiedCircuit

reifyCircuit opts circuit = do
        -- GenSym for input/output pad names
	let inputNames = head $
		[ nms | InputNames nms <- opts ] ++ [[ "i" ++ show i | i <- [0..]]]
	let outputNames = head $
		[ nms | OutputNames nms <- opts ] ++ [[ "o" ++ show i | i <- [0..]]]

	let os = ports inputNames circuit

	let o = Port (Var "o0")
            	$ E
            	$ Entity (Name "Lava" "top") [(Var "o0",B)]	-- not really a Bit
             	[ (Var var,tys, dr) 
	     	| (var,(tys,dr)) <- zip inputNames os
             	]
             	[]
		
        -- Get the graph, and associate the output drivers for the graph with
        -- output pad names.
        (gr, outputs) <- case o of
                Port _ o' -> do
                   (Graph gr out) <- reifyGraph o'
                   case lookup out gr of
                     Just (Entity (Name "Lava" "top")  _ ins _) ->
                       return $ (gr,[(Var sink,ity, driver)
                                       | (_,ity,driver) <- ins
                                       | sink <- outputNames])
                     Just (Entity (Name _ _) outs _ _) ->
                       return $ (gr, [(Var sink,oty, Port ovar out)
                                      | (ovar,oty) <- outs
                                      | sink <- outputNames])
		     Just (Table (ovar,oty) _ _) ->
		       return $ (gr, [ (Var sink,oty, Port ovar out)
                                     | sink <- [head outputNames]
				     ])
                     _ -> error $ "reifyCircuit: " ++ show o


		-- TODO: restore this
--                (Lit x) -> return ([],[(Var (head outputNames),ty,Lit x)])
                v -> fail $ "reifyGraph failed in reifyCircuit" ++ show v

        -- Search all of the enities, looking for input ports.
        let inputs = [ (v,vTy) | (_,Entity _ _ ins _) <- gr, (_,vTy,Pad v) <- ins]
		  ++ [ (v,vTy) | (_,Table _ (_,vTy,Pad v) _) <- gr ]
        return $ ReifiedCircuit { theCircuit = gr
                                , theSrcs = nub inputs
                                , theSinks = outputs
                                }

showReifiedCircuit :: (Ports circuit) => [ReifyOptions] -> circuit -> IO String
showReifiedCircuit opt c = do
	rCir <- reifyCircuit opt c
	let bar = (replicate 78 '-') ++ "\n"
        let showDriver :: Driver Unique -> BaseTy -> String
            showDriver (Port v i) ty = show i ++ "." ++ show v ++ ":" ++ show ty
            showDriver (Lit x) ty = show x ++ ":" ++ show ty
            showDriver (Pad x) ty = show x ++ ":" ++ show ty
            showDriver l _ = error $ "showDriver" ++ show l
	let inputs = unlines
		[ show var ++ " : " ++ show ty
		| (var,ty) <- theSrcs rCir
		]
	let outputs = unlines
		[ show var   ++ " <- " ++ showDriver dr ty
		| (var,ty,dr) <- theSinks rCir
		]
	let circuit = unlines
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

	let msg = bar
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

	return $ msg

debugCircuit :: (Ports circuit) => [ReifyOptions] -> circuit -> IO ()
debugCircuit opt c = showReifiedCircuit opt c >>= putStr

-- | The 'Ports' class generates input pads for a function type, so that the
-- function can be Reified. The result of the circuit, as a driver, as well as
-- the result's type, are returned. I _think_ this takes the place of the REIFY
-- typeclass, but I'm not really sure.

class Ports a where
  ports :: [String] -> a -> [(BaseTy, Driver E)]

instance OpType a => Ports (Signal a) where
  ports _ sig@(Signal _ d) = [(bitTypeOf sig, d)]

instance (Ports a, Ports b) => Ports (a,b) where 
  ports _ (a,b) = ports bad a ++ 
		  ports bad b 
     where bad = error "bad using of arguments in Reify"

instance (Ports a, Ports b, Ports c) => Ports (a,b,c) where 
  ports _ (a,b,c)
 		 = ports bad a ++ 
		   ports bad b ++
		   ports bad c
     where bad = error "bad using of arguments in Reify"

instance (InPorts a, Ports b) => Ports (a -> b) where
  ports vs f = ports vs' $ f a
     where (a,vs') = inPorts vs    

class OutPorts a where
    outPorts :: a ->  [(Var, BaseTy, Driver E)]

class InPorts a where
    inPorts :: [String] -> (a,[String])

instance OpType a => InPorts (Signal a) where
    inPorts (v:vs) = (Signal (error "InPorts (Signal a)") (Pad (Var v)),vs)

instance InPorts Time where
    inPorts vs = (Time c r,vs)
       where
	((c,r),vs') = inPorts vs

instance (InPorts a, InPorts b) => InPorts (a,b) where
    inPorts vs0 = ((a,b),vs2)
	 where
		(a,vs1) = inPorts vs0
		(b,vs2) = inPorts vs1

instance (InPorts a, InPorts b, InPorts c) => InPorts (a,b,c) where
    inPorts vs0 = ((a,b,c),vs3)
	 where
		(a,vs1) = inPorts vs0
		(b,vs2) = inPorts vs1
		(c,vs3) = inPorts vs2
