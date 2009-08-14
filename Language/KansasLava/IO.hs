{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}

module Language.KansasLava.IO where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Language.KansasLava.Type

import Debug.Trace

-- data A a = A (Path -> 


class INPUT i where
	input :: P -> i
	generated :: P -> (i,[P])
	generated' :: E -> P -> (i,[(Ty (),P)])
	
	
	wrapCircuit :: i -> i
	wrapCircuit circuit' = result -- {- o0 $ e_entity -}
	   where 
		(result,pinsY) = generated' e_entity (P [1,2]) 
		e_entity =
        	    E
        	  $ Entity (Name "#AUTO" "ABC") 
			 [ Var (show ps)
			 | (_,ps) <- pinsY
			 ]
			 []
	{-		
			 [ (Var ("i" ++ show n),dr)
			 | (n,(ty,dr)) <- zip [0..] pinsX
			 ]
	-}
			 ({- [ [fmap undefined ty,TyVar v] | (ty,Port v _) <- pinsX ] ++  -}
			  [ [fmap undefined ty,TyVar (Var $ show ps)] | (ty,ps) <- pinsY ])
{-
		(insX,pinsX) = capture' p_root inpX
-}

	

class REIFY c where
	capture :: P -> c -> [(Ty (),Driver E)]
	capture' :: P -> c -> ([P],[(Ty (),Driver E)])
	capture' p c = ([],capture p c)
----

data P = P [Int]

instance Show P where
 	show (P ps) = foldr (\ p s -> "_" ++ show p ++ s) "" ps
	
w (P ps) p = P (p:ps)

----

instance (INPUT i,REIFY o) => REIFY (i -> o) where
         capture p f = capture (p `w` 2) (f o)
	   where
		o = input (p `w` 1)
         capture' p f = ( args ++ args', res)
	   where
		(o,args)    = generated (p `w` 1)
		(args',res) = capture' (p `w` 2) (f o)
	

-- perhaps this can capture type??
instance (OpType a) => REIFY (Signal a) where
        capture p s@(Signal _ d) = [(bitTypeOf s,d)] 
        capture' p s@(Signal _ d) = ([],[(bitTypeOf s,d)])

-- perhaps we can remove the p link here?
-- will break (\ a -> a, \ b -> b) handling, which we might not want, anyway.

instance (REIFY a, REIFY b) => REIFY (a, b) where
	capture p (a,b) = capture (p `w` 1) a ++ capture (p `w` 2) b
instance (REIFY a) => REIFY [a] where
	capture p [] = []
	capture p (x:xs) = capture (p `w` 1) x ++ capture (p `w` 2) xs
instance (REIFY a, REIFY b, REIFY c) => REIFY (a, b,c ) where
	capture p (a,b,c) = capture (p `w` 1) a ++ capture (p `w` 2) b ++ capture (p `w` 3) c
instance (REIFY a, REIFY b, REIFY c, REIFY d) => REIFY (a, b, c, d ) where
	capture p (a,b,c,d) = capture (p `w` 1) a ++ capture (p `w` 2) b ++ capture (p `w` 3) c ++ capture (p `w` 4) d

instance (OpType a) => INPUT (Signal a) where
        input = \ (P ps) -> Signal undefinedSeq $ PathPad ps
	generated = \ (P ps) -> (Signal undefinedSeq $ PathPad ps,[P ps])
	generated' e (P ps) = (a,args)
	  where (a,args) = (Signal undefinedSeq $ Port (Var $ show (P ps)) e,[(bitTypeOf a,P ps)])

instance (INPUT a, INPUT b) => INPUT (a, b) where
	input p = ( input (p `w` 1) , input (p `w` 2)) 
	generated p = ((a',b'),a_ps ++ b_ps)
	    where
		(a',a_ps) = generated (p `w` 1)
		(b',b_ps) = generated (p `w` 2)
	generated' e p = ((a',b'),a_ps ++ b_ps)
	    where
		(a',a_ps) = generated' e (p `w` 1)
		(b',b_ps) = generated' e (p `w` 2)
		
		
instance (INPUT a, INPUT b, INPUT c) => INPUT (a, b, c) where
	input p = ( input (p `w` 1) , input (p `w` 2), input (p `w` 3))
instance (INPUT a, INPUT b, INPUT c, INPUT d) => INPUT (a, b, c, d) where
	input p = ( input (p `w` 1) , input (p `w` 2), input (p `w` 3), input (p `w` 4))

