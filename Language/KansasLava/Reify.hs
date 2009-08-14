{-# LANGUAGE TypeFamilies #-}
module Language.KansasLava.Reify where

import Data.Reify
import Data.List as L

import qualified Data.Set as Set
import Data.Set (Set)

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.IO
import Debug.Trace

--------------------------------------------------------
-- Grab a set of drivers (the outputs), and give me a graph, please.

data Uq = Uq Unique | Sink | Source
	deriving (Eq,Ord,Show)

type QVar = (Uq,Var)

data ReifiedCircuit = ReifiedCircuit
	{ theCircuit :: [(Unique,Entity (Ty Var) Unique)]
		-- ^ This the main graph. There is no actual node for the source or sink. 
	, theSrcs    :: [Var]
	, theSinks   :: [(Var,Driver Unique)]
	, theTypes   :: [(QVar,Ty ())]
	}

-- TODO:
--   1. Use IN and OUT inside the unification pass
--   2. Use INPUT and OUTPUT overloading here.

data ReifyOptions
	= InputNames [String]
	| OutputNames [String]


-- | reifyCircuit does reification and type inference.
reifyCircuit :: REIFY circuit => [ReifyOptions] -> circuit -> IO ReifiedCircuit
-- ([(Unique,Entity (Ty Var) Unique)],[(Var,Driver Unique)])
reifyCircuit opts circuit = do
	let inputNames = head $
		[ nms | InputNames nms <- opts ] ++ [[ "i" ++ show i | i <- [0..]]]
	let outputNames = head $
		[ nms | OutputNames nms <- opts ] ++ [[ "o" ++ show i | i <- [0..]]]

	let p = P []

	let inputs' :: [(Var,Ty ())]
	    blob'    :: [(Ty (),Driver E)]
	    (inputs',blob') = ([],capture p circuit)

	    blob = [ (Var o,d) | ((_,d),o) <- zip blob' outputNames ]
	    
	    outputTyEquivs :: [[Ty QVar]]
	    outputTyEquivs = [ [TyVar (Sink,a),fmap (error "tyvar?") b] | (a,b) <- zip (map fst blob) (map fst blob')]
	
--	print outputTyEquivs
   	let root' = E $ Entity (Name "$" "ROOT") [] blob []
        (Graph nodes root) <- reifyGraph root'

	-- now, rewrite the names
	
	let allPaths = L.sort $ L.nub
		     [ p
		     | (v, Entity name outs ins tyeqs) <- nodes
		     , p <- [ p | (v,PathPad p) <- ins ]
		     ]	

--	print allPaths 

	let pnEnv = [(p,Pad $ Var $ nm) | (p,nm) <- zip allPaths inputNames ]
	
	let nodes' = [ (v, Entity name outs ins' tyeqs)
		     | (v, Entity name outs ins tyeqs) <- nodes
		     , let ins' = [ (v,case d of
					 PathPad p -> case lookup p pnEnv of
							 Nothing -> error "bad internal path name"
							 Just v -> v
					 other -> other)
				  | (v,d) <- ins
				  ]
		     ]			
		
	let nodes = nodes'

--	print nodes

--	inputs

        let nodes1 = [ (u,node) | (u,node) <- nodes, u /= root ]
        let entries = concat [ args | (u,Entity _ _ args _) <- nodes, u == root ]
	let src      = nub [ v | (_,Entity _ _ vs _) <- nodes, (_,Pad v) <- vs ]

	let mkUq :: Unique -> Uq
	    mkUq n | n == root = Sink
		   | otherwise = Uq n

	let all_equivs :: [Set (Ty QVar)] 
	    all_equivs = filter (not . Set.null)
	         $ map (Set.fromList)
		 $ (concat [ map (map addEntityId) tyeqs 
			 ++ [ case dr of
				Port v' i' -> [TyVar (mkUq i,v),TyVar (mkUq i',v')]
				Pad v'     -> [TyVar (mkUq i,v),TyVar (Source,v')] 
				Lit _      -> []
			    | (v,dr) <- ins
			    ]
                          | (i,Entity _ _ ins tyeqs) <- nodes
			  , let addEntityId = fmap (\ v -> (mkUq i,v))
                          ]) ++ outputTyEquivs

--	print all_equivs

	let all_equivs' = findMinEquivSets all_equivs
--	print $ all_equivs'

	let findTyFor :: QVar -> Ty ()
      	    findTyFor (i,v) 
		      | Prelude.null theSet        = error $ "can not find : " ++ show (i,v)
		      | Prelude.length theSet /= 1 = error $ "multiple occur, impossible : " ++ show (i,v,theSet)
		      | Prelude.length theTy == 1  = fmap (\ _ -> ()) theTy'
		      | otherwise                  = error $ "multiple Ty in one equiv class (err?)"
	     where 
		theSet = filter (Set.member (TyVar (i,v))) all_equivs'
		theSet' = Prelude.head theSet
		theTy   = filter (\ t -> case t of
					   TyVar {} -> False
					   _ -> True) $ Set.elems theSet' 
		theTy'  = Prelude.head theTy

	let all_vars = [ v 
		       | (TyVar v) <- Set.toList (Set.unions all_equivs)
		       ]
--	print $ all_vars

	let vars_with_types = [ (v,findTyFor v) | v <- all_vars ]

        return $ ReifiedCircuit nodes1 src entries vars_with_types

-- Some more type class magic.

entity :: 
	(INPUT a, REIFY a,INPUT b) =>
{- REIFY circuit => -} [ReifyOptions] 
	->  String -> (a -> b)  ->  (a -> b) 
entity opts nm circuit  = circuit'
    where
	p_root = P []

	-- (a -> b) -> (a -> b)
	circuit' inpX = result -- {- o0 $ e_entity -}
	   where 
		(result,pinsY) = generated' e_entity (P [1,2]) 
		e_entity =
        	    E
        	  $ Entity (Name "#AUTO" "ABC") 
			 [ Var (show ps)
			 | (_,ps) <- pinsY
			 ]
			 [ (Var ("i" ++ show n),dr)
			 | (n,(ty,dr)) <- zip [0..] pinsX
			 ]
			 ([ [fmap undefined ty,TyVar v] | (ty,Port v _) <- pinsX ] ++ 
			  [ [fmap undefined ty,TyVar (Var $ show ps)] | (ty,ps) <- pinsY ])
		(insX,pinsX) = capture' p_root inpX


entity' :: 
	(INPUT b) =>
{- REIFY circuit => -} [ReifyOptions] 
	->  String -> b -> b
entity' opts nm circuit  = wrapCircuit circuit
{-

instance (INPUT a, REIFY a,REIFY b, INPUT b) => INPUT (a -> b) where
	wrapCircuit inpX = result -- {- o0 $ e_entity -}
	   where 
		p_root = P []
		(result,pinsY) = generated' e_entity (P [1,2]) 
		e_entity =
        	    E
        	  $ Entity (Name "#AUTO" "ABC") 
			 [ Var (show ps)
			 | (_,ps) <- pinsY
			 ]
			 [ (Var ("i" ++ show n),dr)
			 | (n,(ty,dr)) <- zip [0..] pinsX
			 ]
			 ([ [fmap undefined ty,TyVar v] | (ty,Port v _) <- pinsX ] ++ 
			  [ [fmap undefined ty,TyVar (Var $ show ps)] | (ty,ps) <- pinsY ])
		(insX,pinsX) = capture' p_root inpX	

	-- generated :: E -> P -> (a -> b,[P])
	generated' e p = (
	   where
		(a,p1) = generated (p `w` 1)
		(b,p1) = generated (p `w` 2)
		

-}

{-
		entity1 :: Name -> [Var] -> [Var] -> [[Ty Var]] -> (a -> b) -> Signal a -> ESignal b
entity1 nm ins outs tyeqs f  s@(~(Signal vs1 w1)) 
        = 
-}

