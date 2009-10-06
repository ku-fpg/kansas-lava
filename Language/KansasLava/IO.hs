{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}

module Language.KansasLava.IO where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Language.KansasLava.Type
import Control.Applicative
import Data.Sized.Matrix
import Data.Traversable
import Data.List as L
import Data.Sized.Matrix as M
import Data.Sized.Ix
import Data.Array (Ix)

import Debug.Trace

data A a = A (E -> [Var] -> (a,[(BaseTy,Var)],[Var]))

instance Functor A where
	fmap f (A fn) = A $ \ e vars -> case fn e vars of
				         (a,tys,vars') -> (f a,tys,vars')

instance Applicative A where
	pure a = A $ \ e vars -> (a,[],vars)
	(A f1) <*> (A f2) = A $ \ e vars ->
		case f1 e vars of
		  (a1,tys1,vars1) -> case f2 e vars1 of
			 	      (a2,tys2,vars2) -> (a1 a2,tys1 ++ tys2,vars2)

--class INPUT i where
class REIFY c where
	create :: A c	
	capture'' ::  c -> A [(BaseTy,Driver E)]

	
	wrapCircuit :: [(Var, Driver E)] -> [[Ty Var]] -> c -> c
	wrapCircuit args tys circuit' = result -- {- o0 $ e_entity -}
	   where 
		(result,pinsY) = case create of
				   A fn -> case fn e_entity [Var $ "x" ++ show n | n <- [0..]] of
					     (res,tys,_) -> (res,tys)
		e_entity =
        	    E
        	  $ Entity (Name "#AUTO" "ABC") 
			 [ Var (show ps)
			 | (_,ps) <- pinsY
			 ]
			 args
			 (tys ++
			  [ [BaseTy ty,TyVar (Var $ show ps)] | (ty,ps) <- pinsY ]
			 )

data P = P [Int]

instance Show P where
 	show (P ps) = foldr (\ p s -> "_" ++ show p ++ s) "" ps
	
w (P ps) p = P (p:ps)

----
instance (REIFY i,REIFY o) => REIFY (i -> o) where
	-- the one place we need bind, so we hard wire it here.
        capture'' f = A $ \ e vars -> 
		case create of
		  (A g) -> case g e vars of
			     (o,tys,vars') -> case capture'' (f o) of
					       A c -> case c e vars' of
							(r,tys',vars'') -> (r,tys ++ tys',vars'')

	wrapCircuit args tys fn inpX = result -- {- o0 $ e_entity -}
	   where 
		(insX,pinsX) = case capture'' inpX of
				 A c -> case c  (error "GHHJDFG") [Var $ "ss" ++ show n | n <- [0..]] of
					  (o,tys,_) -> (tys,o)
		p_root = P []
		args' = args ++
			 [ (Var ("i" ++ show n),dr)
			 | (n,(ty,dr)) <- zip [(L.length args)..] pinsX
			 ]
		tys' = [ [BaseTy ty,TyVar v] | (ty,Port v _) <- pinsX ] ++ tys
		result = wrapCircuit args' tys' (fn inpX)


-- perhaps this can capture type??
instance (OpType a) => REIFY (Signal a) where
	create = A $ \ e (v:vs) ->
 	    let f a = (a,[(bitTypeOf a,v)],vs)	-- lambda bound a, therefor monotyped.
	    in f $ Signal (error "create") $ Port v e

--	capture p s@(Signal _ d) = [(bitTypeOf s,d)] 
--        capture' _ p s@(Signal _ d) = ([],[(bitTypeOf s,d)])
	capture'' s@(Signal _ d) = pure [(bitTypeOf s,d)]

-- perhaps we can remove the p link here?
-- will break (\ a -> a, \ b -> b) handling, which we might not want, anyway.

instance (REIFY a, REIFY b) => REIFY (a, b) where
--	capture p (a,b) = capture (p `w` 1) a ++ capture (p `w` 2) b
	create     = (,)  <$> create <*> create
	capture'' (a,b) = (++) <$> capture'' a <*> capture'' b 


instance (Size ix, REIFY a) => REIFY (Matrix ix a) where
	create = traverse (\ i -> create) coord
	capture'' m = concat <$> (traverse capture'' $ M.toList m)

