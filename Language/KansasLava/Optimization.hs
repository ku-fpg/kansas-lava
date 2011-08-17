-- | The Optimization module performs a number of safe optimizations, such as
--  copy elimination, CSE, and combinatorial identity artifacts:
--  e.g. fst(x,y)-->x. The optimizer is *supposed* to be timing invariant;
--  however, it may change circuit properties such as fanout.
module Language.KansasLava.Optimization
	( optimizeCircuit
	, OptimizationOpts(..)
	) where

import Language.KansasLava.Types
import Data.Reify
import Control.Monad

import Data.List
import Data.Default
import Data.Maybe(fromMaybe)


-- NOTES:
-- We need to update the Lava::id operator, that can take and return *multiple* values.

-- A very simple optimizer.

-- | This returns an optimized version of the Entity, or fails to optimize.  |
-- Remove trivial overhead, such a direct projection from a product, pairing
-- after projection, and multiplexers with duplicated data input.
optimizeEntity :: (Unique -> Entity Unique) -> Entity Unique -> Maybe (Entity Unique)
optimizeEntity env (Entity (Prim "fst") [(o0,_)] [(_,_,Port o0' u)]) =
	case env u of
	    Entity (Prim "pair") [(o0'',_)] [(i1',t1,p1),(_,_,_)]
	       | o0' == o0'' -> return $ replaceWith o0 (i1',t1,p1)
	    _ -> Nothing
optimizeEntity env (Entity (Prim "snd") [(o0,_)] [(_,_,Port o0' u)]) =
	case env u of
	    Entity (Prim "pair") [(o0'',_)] [(_,_,_),(i2',t2,p2)]
	       | o0' == o0'' -> return $ replaceWith o0 (i2',t2,p2)
	    _ -> Nothing
optimizeEntity env (Entity (Prim "pair") [(o0,tO)] [(_,_,Port o0' u0),(_,_,Port o1' u1)]) =
	case (env u0,env u1) of
	    ( Entity (Prim "fst") [(o0'',_)] [(_,_,p2)]
	      , Entity (Prim "snd") [(o1'',_)] [(_,_,p1)]
	      ) | o0' == o0'' && o1' == o1'' && p1 == p2 ->
			return $ replaceWith o0 ("o0",tO,p1)
	    _ -> Nothing
optimizeEntity _ (Entity (Prim "mux2") [(o0,_)] [(_,_,_),(i1 ,tTy,t),(_,_,f)])
    | t == f = return $ replaceWith o0 (i1,tTy,t)
    | otherwise = Nothing
optimizeEntity _ (Entity (BlackBox _) [(o0,_)] [(i0, ti, di)]) =
  return $ replaceWith o0 (i0,ti,di)
optimizeEntity _ _ = Nothing

----------------------------------------------------------------------

replaceWith :: String -> (String, Type, Driver s) -> Entity s
replaceWith o (i,t,other) = Entity (Prim "id") [(o,t)] [(i,t,other)]
--replaceWith (i,t,x) = error $ "replaceWith " ++ show (i,t,x)

----------------------------------------------------------------------

-- | A optimization result will return the result along with an Int representing
-- some metric based on the optimization...
data Opt a = Opt a Int -- [String]

instance Monad Opt where
    return a = Opt a 0
    (Opt a n) >>= k = case k a of
	 		Opt r m -> Opt r (n + m)


----------------------------------------------------------------------

-- | Copy elimination. Eliminate 'id' entities by propagating inputs to outputs.
copyElimCircuit :: KLEG -> Opt KLEG
copyElimCircuit rCir =  Opt rCir' (length renamings)
    where
	env0 = theCircuit rCir

	rCir' = rCir
	      { theSinks =
		  [ ( v,t,
		      case d of
		        Port p u -> fromMaybe (Port p u) (lookup (u, p) renamings)
			Pad v' -> Pad v'
			Lit i -> Lit i
			Error i -> error $ "Found Error : " ++ show (i,v,t,d)
                        c -> error $ "copyElimCircuit: " ++ show c
		    )
		  | (v,t,d) <- theSinks rCir
		  ]
	      , theCircuit =
		 [ (u,case e of
			Entity nm outs ins -> Entity nm outs (map fixInPort ins)
		   )
		 | (u,e) <- env0
		 ]
	      }

	renamings = [ ((u,o),other)
		    | (u,Entity (Prim "id") [(o,tO)] [(_,tI,other)]) <- env0
		    , tO == tI	-- should always be, anyway
		    ]

	fixInPort (i,t,Port p u) =
			    (i,t,fromMaybe (Port p u) (lookup (u, p) renamings))
	fixInPort (i,t,o) = (i,t,o)


--
-- Starting CSE.
-- | Common subexpression eliminatation.
cseCircuit :: KLEG -> Opt KLEG
cseCircuit rCir = Opt  (rCir { theCircuit = concat rCirX }) cseCount
   where

	-- how many CSE's did we spot?
	cseCount = length (theCircuit rCir) - length rCirX

	-- for now, just use show: this is what has changed
	-- optMsg = [ (u,e)
	--          | (u,e) <- (theCircuit rCir)
	-- 	 , not (u `elem` (map fst (map head rCirX)))
	--          ]

	rCirX :: [[(Unique, Entity Unique)]]
	rCirX = map canonicalize
		-- We want to combine anything *except* idents's
		-- because we would just replace them with new idents's (not a problem)
		-- _and_ say we've found some optimization (which *is* a problem).
	      $ groupBy (\ (_,b) (_,b') -> (b `mycompare` b') == EQ && not (isId b))
	      $ sortBy (\ (_,b) (_,b') -> b `mycompare` b')
	      $ theCircuit rCir


	isId (Entity (Prim "id") _ _) = True
	isId _ = False

	-- The outputs do not form part of the comparison here,
	-- because they *can* be different, without effecting
	-- the CSE opertunity.
	mycompare (Entity nm _ ins)
	 	  (Entity nm' _ ins') =
		chain
		  [ nm `compare` nm'
		  , ins `compare` ins'
		  ]

	chain (LT:_ ) = LT
	chain (GT:_ ) = GT
	chain (EQ:xs) = chain xs
	chain []      = EQ

	-- Build the identites
        canonicalize [] = []
	canonicalize ((u0,e0@(Entity _ outs _)):rest) =
		(u0,e0) : [ ( uX,
                              Entity (Prim "id") outs'
                                [ (n,t, Port n u0) | (n,t) <- outs ]
			    )
			  | (uX,Entity _ outs' _) <- rest, length outs == length outs'
			  ]

-- | Dead code elimination. Although reifyGraph will only reify the part of the
-- circuit that is reachable, it is possible that additional optimizations will
-- make code unreachable. This pass eliminates those graph nodes.
dceCircuit :: KLEG -> Opt KLEG
dceCircuit rCir = if optCount == 0
			 then return rCir
			 else Opt  rCir' optCount
  where
	optCount = length (theCircuit rCir) - length optCir

	outFrees (_,_,Port _ u) = [u]
	outFrees _             = []

	allNames :: [Unique]
	allNames = nub (concat
		       [ case e of
			  Entity _ _ outs -> concatMap outFrees outs
		       | (_,e) <- theCircuit rCir
		       ] ++ concatMap outFrees (theSinks rCir)
		       )

	optCir = [ (uq,orig)
		 | (uq,orig) <- theCircuit rCir
		 , uq `elem` allNames
		 ]
	rCir' = rCir { theCircuit = optCir }

-- return Nothing *if* no optimization can be found.
-- | Apply the optimizeEntity optimization.
patternMatchCircuit :: KLEG -> Opt KLEG
patternMatchCircuit rCir = if optCount == 0
			      then return rCir	-- no changes
			      else Opt rCir' optCount
  where
	env0 = theCircuit rCir
	env1 v = fromMaybe (error $ "oops, can not find " ++ show v) (lookup v env0)
	attemptOpt = [ optimizeEntity env1 e | (_,e) <- theCircuit rCir ]
	optCount = length [ () | (Just _) <- attemptOpt ]

	optCir = [ (uq,fromMaybe orig mOpt)
		 | ((uq,orig),mOpt) <- zip (theCircuit rCir) attemptOpt
		 ]

	rCir' = rCir { theCircuit = optCir }

-- | Apply a number of named optimizations (the first argument) to the
-- input. Optimizations are applied sequentially, but the intermediate results
-- are returned in the resulting list.
optimizeCircuits :: [(String,KLEG -> Opt KLEG)] -> KLEG -> [(String,Opt KLEG)]
optimizeCircuits [] _ = []
optimizeCircuits ((nm,fn):fns) c = (nm,opt) : optimizeCircuits fns c'
	where opt@(Opt c' _) = case fn c of
				 Opt _ 0 -> Opt c 0	-- If there was no opts, avoid churn
				 res@(Opt _ _) -> res


-- | Data structure for passing optimization parameters.
data OptimizationOpts = OptimizationOpts
	{ optDebugLevel :: Int
	}

instance Default OptimizationOpts
   where
	def = OptimizationOpts
		{ optDebugLevel = 0
		}


-- | Basic optimizations, and assumes reaching a fixpoint.
-- Cleans things up, but does not work too hard, because
-- the VHDL compiler get many of the combinatorial optimizations anyway.
optimizeCircuit :: OptimizationOpts -> KLEG -> IO KLEG
optimizeCircuit options rCir = do
	when debug $ print rCir
	loop (optimizeCircuits (cycle opts) rCir)
   where
	debug = optDebugLevel options > 0
        loop [] = error "optimizeCircuit: loop []"
	loop cs@((nm,Opt code n):_) = do
		when debug $ do
		  putStrLn $ "##[" ++ nm ++ "](" ++ show n ++ ")###################################"
		  when (n > 0) $ print code
		case cs of
		    ((_,Opt c _):_) | and [ num == 0 | (_,Opt _ num) <- take (length opts) cs ] -> return c
		    ((_,Opt _ _):rest) -> loop rest
                    [] -> error "optimizeCircuit: no optimizations"

	opts = [ ("opt",patternMatchCircuit)
	       , ("cse",cseCircuit)
	       , ("copy",copyElimCircuit)
	       , ("dce",dceCircuit)
	       ]


