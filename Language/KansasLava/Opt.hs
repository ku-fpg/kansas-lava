module Language.KansasLava.Opt where

import Language.KansasLava.Type
import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Type
import Language.KansasLava.Wire
import Language.KansasLava.Entity
import Language.KansasLava.Circuit


import Data.List

import Debug.Trace
-- A very simple optimizer.

-- This returns an optimized version of the Entity, or fails to optimize.
optimizeEntity :: (Unique -> Entity BaseTy Unique) -> Entity BaseTy Unique -> Maybe (Entity BaseTy Unique)
optimizeEntity env (Entity (Name "Lava" "fst") [(o0,tO)] [(i0,tI,Port o0' u)] []) =
	case env u of
	    Entity (Name "Lava" "pair") [(o0'',tI')] [(i1',t1,p1),(i2',t2,p2)] []
	       | o0' == o0'' -> return $ replaceWith env o0 (i1',t1,p1)
	    _ -> Nothing
optimizeEntity env (Entity (Name "Lava" "snd") [(o0,tO)] [(i0,tI,Port o0' u)] []) =
	case env u of
	    Entity (Name "Lava" "pair") [(o0'',tI')] [(i1',t1,p1),(i2',t2,p2)] []
	       | o0' == o0'' -> return $ replaceWith env o0 (i2',t2,p2)
	    _ -> Nothing	
optimizeEntity env _ = Nothing

replaceWith env o (i,t,other) = Entity (Name "Lava" "id") [(o,t)] [(i,t,other)] []
--replaceWith (i,t,x) = error $ "replaceWith " ++ show (i,t,x)

----------------------------------------------------------------------

data Opt a = Opt a Int

instance Monad Opt where
    return a = Opt a 0
    (Opt a n) >>= k = case k a of
	 		Opt r m -> Opt r (n + m)

----------------------------------------------------------------------

-- copy elimination
copyElimReifiedCircuit :: ReifiedCircuit -> Opt ReifiedCircuit
copyElimReifiedCircuit rCir = trace (show renamings) $ Opt rCir' (length renamings) 
    where
	env0 = theCircuit rCir
	
	rCir' = rCir 
	      { theSinks = 
		  [ ( v,t,
		      case d of
		        Port p u -> case lookup (u,p) renamings of
				      Just other -> other
				      Nothing    -> Port p u

		    )
		  | (v,t,d) <- theSinks rCir
		  ]
	      , theCircuit = 
		 [ (u,case e of
			Entity nm outs ins tags -> Entity nm outs (map fixInPort ins) tags
			Table in' out' table -> Table in' (fixInPort out') table
		   )
		 | (u,e) <- env0
		 ]
	      }

	renamings = [ ((u,o),other)
		    | (u,Entity (Name "Lava" "id") [(o,tO)] [(i,tI,other)] []) <- env0
		    , tO == tI	-- should always be, anyway
		    ]

	fixInPort (i,t,Port p u) = 
			    (i,t, case lookup (u,p) renamings of
				     Just other -> other
				     Nothing    -> Port p u)
	fixInPort (i,t,o) = (i,t,o)
	

-- We assume, for now, that we cse if possible. This will not always be so.
-- cseReifiedCircuit :: ReifiedCircuit -> Opt ReifiedCircuit
--

dceReifiedCircuit :: ReifiedCircuit -> Opt ReifiedCircuit
dceReifiedCircuit rCir = if optCount == 0
			 then return rCir
			 else Opt  rCir' optCount
  where
	optCount = length (theCircuit rCir) - length optCir

	outFrees (_,_,Port _ u) = [u]
	outFrees _             = []

	allNames :: [Unique]
	allNames = nub (concat
		       [ case e of
			  Entity nm _ outs _ -> concatMap outFrees outs
		       | (u,e) <- theCircuit rCir
		       ] ++ concatMap outFrees (theSinks rCir)
		       )

	optCir = [ (uq,orig)
		 | (uq,orig) <- theCircuit rCir
		 , uq `elem` allNames
		 ]
	rCir' = rCir { theCircuit = optCir }

-- return Nothing *if* no optimization can be found.
optimizeReifiedCircuit :: ReifiedCircuit -> Opt ReifiedCircuit
optimizeReifiedCircuit rCir = if optCount == 0 
			      then return rCir	-- no changes
			      else Opt rCir' optCount
  where
	env0 = theCircuit rCir
	env1 v = case lookup v env0 of
		   Nothing -> error $ "oops, can not find " ++ show v
		   Just e -> e
		    
	attemptOpt = [ optimizeEntity env1 e | (u,e) <- theCircuit rCir ]
	optCount = length [ () | (Just _) <- attemptOpt ]

	optCir = [ (uq,case mOpt of
			 Nothing -> orig
			 Just opt -> opt)
		 | ((uq,orig),mOpt) <- zip (theCircuit rCir) attemptOpt
		 ]

	rCir' = rCir { theCircuit = optCir }

optimizeReifiedCircuits :: [(String,ReifiedCircuit -> Opt ReifiedCircuit)] -> ReifiedCircuit -> [(String,Opt ReifiedCircuit)]
optimizeReifiedCircuits ((nm,fn):fns) c = (nm,opt) : optimizeReifiedCircuits fns c'
	where opt@(Opt c' n) = fn c


-- Assumes reaching a fixpoint.
optimize :: ReifiedCircuit -> ReifiedCircuit
optimize rCir = loop (("init",Opt rCir 0) : optimizeReifiedCircuits (cycle opts) rCir)
   where
	loop cs@((nm,Opt c _):_) | and [ n == 0 | (_,Opt c n) <- take (length opts) cs ] = c
	loop ((nm,Opt c v):cs) = loop cs
	
	opts = [ ("opt",optimizeReifiedCircuit)
	       , ("copy",copyElimReifiedCircuit)
	       , ("dce",dceReifiedCircuit)
	       ]
