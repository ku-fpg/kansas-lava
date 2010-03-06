module Language.KansasLava.Opt where

import Language.KansasLava.Type
import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Type
import Language.KansasLava.Wire
import Language.KansasLava.Entity
import Language.KansasLava.Reify

-- A very simple optimizer.

-- This returns an optimized version of the Entity, or fails to optimize.
optimizeEntity :: (Unique -> Entity BaseTy Unique) -> Entity BaseTy Unique -> Maybe (Entity BaseTy Unique)
optimizeEntity env (Entity (Name "Lava" "fst") [(o0,tO)] [(i0,tI,Port o0' u)] []) =
	case env u of
	    Entity (Name "Lava" "pair") [(o0'',tI')] [(i1',t1,p1),(i2',t2,p2)] []
	       | o0' == o0'' -> return $ replaceWith (i1',t1,p1)
	    _ -> Nothing
optimizeEntity env (Entity (Name "Lava" "snd") [(o0,tO)] [(i0,tI,Port o0' u)] []) =
	case env u of
	    Entity (Name "Lava" "pair") [(o0'',tI')] [(i1',t1,p1),(i2',t2,p2)] []
	       | o0' == o0'' -> return $ replaceWith (i2',t2,p2)
	    _ -> Nothing	
optimizeEntity env _ = Nothing

replaceWith (i,t,Pad v) = Entity (Name "Lava" "id") [(Var "o0",t)] [(Var "i0",t,Pad v)] []
replaceWith (i,t,other) = Entity (Name "Lava" "id") [(Var "o0",t)] [(Var "i0",t,other)] []
--replaceWith (i,t,x) = error $ "replaceWith " ++ show (i,t,x)


-- return Nothing *if* no optimization can be found.
optimizeReifiedCircuit :: ReifiedCircuit -> Maybe ReifiedCircuit
optimizeReifiedCircuit rCir = if optCount == 0 
			      then fail "Done Optimizing" 
			      else return rCir'
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

{-

		
			{ theCircuit :: [(Unique,Entity BaseTy Unique)]
		-- ^ This the main graph. There is no actual node for the source or sink.
	, theSrcs    :: [(Var,BaseTy)]
	, theSinks   :: [(Var,BaseTy,Driver Unique)]
	-- , theTypes   :: TypeEnv
	}	
-}

optimizeReifiedCircuits :: ReifiedCircuit -> [ReifiedCircuit]
optimizeReifiedCircuits c = 
	c : case optimizeReifiedCircuit c of
	     Nothing -> []
	     Just c' -> optimizeReifiedCircuits c'

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

	rCir' <- loop 0 (optimizeReifiedCircuits rCir)
	return $ show rCir'