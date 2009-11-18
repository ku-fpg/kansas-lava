{-# LANGUAGE TypeFamilies #-}
module Language.KansasLava.Reify where

import Data.Reify
import Data.List as L

import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad (when)

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
type TypeEnv = [(QVar,BaseTy)]

showQVar :: QVar -> String
showQVar (u,v) = show v ++ "-" ++ (case u of 
				    Uq uq -> show uq
				    Sink -> "out"
				    Source -> "in") ++ ""

data ReifiedCircuit = ReifiedCircuit
	{ theCircuit :: [(Unique,Entity (Ty Var) Uq)]
		-- ^ This the main graph. There is no actual node for the source or sink.
	, theSrcs    :: [Var]
	, theSinks   :: [(Var,Driver Uq)]
	, theTypes   :: TypeEnv
	}

-- TODO:
--   1. Use IN and OUT inside the unification pass
--   2. Use INPUT and OUTPUT overloading here.

data ReifyOptions
	= InputNames [String]
	| OutputNames [String]
	| DebugReify		-- show debugging output of the reification stage
	deriving (Eq, Show)

findTy :: ReifiedCircuit -> QVar -> Maybe BaseTy
findTy re (u,v) = lookup (u,v) (theTypes re)

-- | reifyCircuit does reification and type inference.
reifyCircuit :: REIFY circuit => [ReifyOptions] -> circuit -> IO ReifiedCircuit
-- ([(Unique,Entity (Ty Var) Unique)],[(Var,Driver Unique)])
reifyCircuit opts circuit = do
	let inputNames = head $
		[ nms | InputNames nms <- opts ] ++ [[ "i" ++ show i | i <- [0..]]]
	let outputNames = head $
		[ nms | OutputNames nms <- opts ] ++ [[ "o" ++ show i | i <- [0..]]]

	let p = P []


	let inputs' :: [(BaseTy,Var)]
	    blob'    :: [(BaseTy,Driver E)]
	    (inputs',blob') = case capture'' circuit of
				A f -> case f inputEntity (map Var inputNames) of
					  (res,tys,_) -> (tys,res)


	    inputEntity :: E
	    inputEntity = E $ Entity (Name "#" "INPUT") (map snd inputs') [] []

--	    inputs' = []


	    blob = [ (Var o,d) | ((_,d),o) <- zip blob' outputNames ]

	    outputTyEquivs :: [[Ty QVar]]
	    outputTyEquivs = [ [TyVar (Sink,a),BaseTy b]
			     | (a,b) <- zip (map fst blob) (map fst blob')]

	    inputTyEquivs :: [[Ty QVar]]
	    inputTyEquivs = [ [TyVar (Source,v),BaseTy ty]
			    | (ty,v) <- inputs'
			    ]


--	print outputTyEquivs



--	print outputTyEquivs
   	let root' = E $ Entity (Name "$" "ROOT") [] blob []
        (Graph nodes root) <- reifyGraph root'

	when (DebugReify `elem` opts) $ do
		putStrLn "-------------------------------------------------------"
		print (Graph nodes root)
		putStrLn "-------------------------------------------------------"

	let inputEntityId :: Unique
	    inputEntityId = head [ v
	 	     	         | (v, Entity name outs ins tyeqs) <- nodes
		                 , name == Name "#" "INPUT"
		                 ]
	let outputEntityId :: Unique
	    outputEntityId = head [ v
	 	     	         | (v, Entity name outs ins tyeqs) <- nodes
		                 , name == Name "$" "ROOT"
		                 ]
	-- now, rewrite the names

	let allPaths = L.sort $ L.nub
		     [ p
		     | (v, Entity name outs ins tyeqs) <- nodes
		     , p <- [ p | (v,PathPad p) <- ins ]
		     ]

--	print allPaths
{-
	let pnEnv = [(p,Pad $ Var $ nm) | (p,nm) <- zip allPaths inputNames ]
	print (pnEnv :: [([Int],Driver E)])
-}

	let mkUq :: Unique -> Uq
	    mkUq n | n == inputEntityId  = Source
		   | n == outputEntityId = Sink
		   | otherwise           = Uq n

{-
	let nodes' :: [(Unique,Entity (Ty Var) Uq)]
	    nodes' = [ (v, Entity name outs ins' tyeqs)
		     | (v, Entity name outs ins tyeqs) <- nodes
		     , let ins' = [ (v,case d of
					 Uq
					 PathPad p -> case lookup p pnEnv of
							 Nothing -> error "bad internal path name"
							 Just v -> v
					 other -> other)
				  | (v,d) <- ins
				  ]
		     , let outs' = [ (v,fmap mkUq out) | (v,out) <- outs ]
		     ]

-}

	let nodes' = nodes

	let nodes :: [(Unique,Entity (Ty Var) Uq)]
	    nodes = [ (v,fmap mkUq e) | (v,e) <- nodes' ]



	{-
	 - Next, we rename our input variables, using the given schema.
	 -}


	let nodes' = nodes
	let srcs = nub $ concat
		       [  outs
		       | (v, Entity name outs ins tyeqs) <- nodes
		       , v == inputEntityId
		       ]
	let inMap = zip (sort srcs) (map Var inputNames)

        let nodes1 = [ (u,node) | (u,node) <- nodes
				, u /= root
			  	, u /= inputEntityId
				]


	let src :: [Var]
--	    src  = nub [ v | (_,Entity _ _ vs _) <- nodes, (_,Pad v) <- vs ]
	    src  = nub $ concat
		       [  outs
		       | (v, Entity name outs ins tyeqs) <- nodes
		       , v == inputEntityId
		       ]

{-
	let mkUq :: Unique -> Uq
	    mkUq n | n == root = Sink
		   | otherwise = Uq n
-}

	let all_equivs :: [Set (Ty QVar)]
	    all_equivs = filter (not . Set.null)
	         $ map (Set.fromList)
		 $ (concat [map (map addEntityId) tyeqs
			 ++ [ case dr of
				Port v' i' -> [TyVar (mkUq i,v),TyVar (i',v')]
				Lit _      -> []
				other       -> error $ show other
			    | (v,dr) <- ins
			    ]
                          | (i,Entity _ _ ins tyeqs) <- nodes
--			  , i /= inputEntityId
			  , let addEntityId = fmap (\ v -> (mkUq i,v))
                          ]) ++ outputTyEquivs ++ inputTyEquivs


	when (DebugReify `elem` opts) $ do
		putStrLn "-------------------------------------------------------"
		putStrLn "[all_equivs]"
		print all_equivs
		putStrLn "-------------------------------------------------------"


--	print ("ZZ",all_equivs)
--	print all_equivs

	let all_equivs' = findMinEquivSets all_equivs
--	print $ all_equivs'

	let findTyFor :: QVar -> BaseTy
      	    findTyFor (i,v)
		      | Prelude.null theSet        = error $ "can not find : " ++ show (i,v)
		      | Prelude.length theSet /= 1 = error $ "multiple occur, impossible : " ++ show (i,v,theSet)
		      | Prelude.null theTy 	   = error $ "polymophic set of wires, can not infer type (usually a loop, or using a underdefined primitive)"
		      | Prelude.length theTy == 1  = case theTy' of
						       BaseTy bty -> bty
						       _ -> error $ "Type of wire is not a basic type, instead found : " ++ show theTy'
		      | otherwise                  = error $ "multiple Ty in one equiv class (err?)" ++ show (i,v) ++ " " ++ show theSet
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

        let entries = concat [ args
			     | (u,Entity _ _ args _) <- nodes
 			     , u == outputEntityId
			     ]

	let vars_with_types = [ (v,findTyFor v) | v <- all_vars ]

--	print ("XX",vars_with_types)

        return $ ReifiedCircuit nodes1 src entries vars_with_types

-- Some more type class magic.
{-
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
-}

entity :: (REIFY b, CLONE b) => [ReifyOptions] ->String -> b -> b
entity opts nm circuit = clone circuit deep
  where
	deep = wrapCircuit [] [] circuit

showReifiedCircuit :: REIFY circuit => [ReifyOptions] -> circuit -> IO String
showReifiedCircuit opt c = do
	rCir <- reifyCircuit opt c
	let bar = take 78 (repeat '-') ++ "\n"
	let showTy _ = "*"
	let showType :: QVar -> String
	    showType (u,v) = case findTy rCir (u,v) of
			      Just t -> show t
			      Nothing -> "?"

	let showVar v =  showQVar v ++ " : " ++ showType v
	let showDriver :: Driver Uq -> String
	    showDriver (Port v uq) = showVar (uq,v)
	    showDriver (Lit n)     = "#" ++ show n
	let inputs = unlines
		[ showVar (Source,var)
		| var <- theSrcs rCir
		]
	let outputs = unlines
		[ showVar (Sink,var)  ++ " <- " ++ showDriver dr
		| (var,dr) <- theSinks rCir
		]
	let types = unlines
		[ showVar qv
		| (qv,_) <- theTypes rCir
		]
	let circuit = unlines
		[ "(" ++ show uq ++ ") " ++ show nm ++ "\n"
			++ unlines [ "      out " ++ showVar (Uq uq,v) | v <- ins ]
			++ unlines [ "      in  " ++ showVar (Uq uq,v) ++ " <- " ++ showDriver dr | (v,dr) <- outs ]
			++ unlines [ "      eq  " ++ show tys | tys <- tyss ]
		| (uq,Entity nm ins outs tyss) <- theCircuit rCir
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
		++ "-- Types                                                                    --\n" 
		++ bar
		++ types 
		++ bar
		++ "-- Entities                                                                 --\n" 
		++ bar
		++ circuit 
		++ bar

	return $ msg

debugCircuit :: REIFY circuit => [ReifyOptions] -> circuit -> IO ()
debugCircuit opt c = showReifiedCircuit opt c >>= putStr

