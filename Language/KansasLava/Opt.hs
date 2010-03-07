module Language.KansasLava.Opt where

import Language.KansasLava.Type
import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Type
import Language.KansasLava.Wire
import Language.KansasLava.Entity
import Language.KansasLava.Reify
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
	      }

	renamings = [ ((u,o),other)
		    | (u,Entity (Name "Lava" "id") [(o,tO)] [(i,tI,other)] []) <- env0
		    , tO == tI	-- should always be, anyway
		    ]
	

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

optimizeReifiedCircuits :: ReifiedCircuit -> [(Int,ReifiedCircuit)]
optimizeReifiedCircuits c = 
	case optimize c of
	     Opt r 0 -> []
	     Opt r n -> (n,r) : optimizeReifiedCircuits r
   where
	-- strange form, but does work, and allows easy reordering
	optimize c = return c
	    >>= optimizeReifiedCircuit
	    >>= copyElimReifiedCircuit
	    >>= dceReifiedCircuit



showOptReifiedCircuit :: (Ports circuit) => [ReifyOptions] -> circuit -> IO String
showOptReifiedCircuit opt c = do
	rCir <- reifyCircuit opt c
	let optCirs = optimizeReifiedCircuits
	let loop n [c] = do
		 putStrLn $ "## Answer " ++ show n ++ " ##############################"
		 print c
		 return c
	    loop n (c:cs) = do
		print $ "Round " ++ show n
		print rCir
		loop (succ n) cs

	(_,rCir') <- loop 0 (optimizeReifiedCircuits rCir)
	return $ show rCir'