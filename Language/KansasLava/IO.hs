{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}

module Language.KansasLava.IO where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Language.KansasLava.Type

import Debug.Trace

class INPUT i where
	input :: P -> i

class REIFY c where
	capture :: P -> c -> [(Ty (),Driver E)]

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

-- perhaps this can capture type??
instance (OpType a) => REIFY (Signal a) where
        capture p s@(Signal _ d) = [(bitTypeOf s,d)] 

-- perhaps we can remove the p link here?
-- will break (\ a -> a, \ b -> b) handling, which we might not want, anyway.

instance (REIFY a, REIFY b) => REIFY (a, b) where
	capture p (a,b) = capture (p `w` 1) a ++ capture (p `w` 2) b
instance (REIFY a, REIFY b, REIFY c) => REIFY (a, b,c ) where
	capture p (a,b,c) = capture (p `w` 1) a ++ capture (p `w` 2) b ++ capture (p `w` 3) c
instance (REIFY a, REIFY b, REIFY c, REIFY d) => REIFY (a, b, c, d ) where
	capture p (a,b,c,d) = capture (p `w` 1) a ++ capture (p `w` 2) b ++ capture (p `w` 3) c ++ capture (p `w` 4) d

instance INPUT (Signal a) where
        input = \ (P ps) -> Signal undefinedSeq $ PathPad ps

instance (INPUT a, INPUT b) => INPUT (a, b) where
	input p = ( input (p `w` 1) , input (p `w` 2)) 
instance (INPUT a, INPUT b, INPUT c) => INPUT (a, b, c) where
	input p = ( input (p `w` 1) , input (p `w` 2), input (p `w` 3))
instance (INPUT a, INPUT b, INPUT c, INPUT d) => INPUT (a, b, c, d) where
	input p = ( input (p `w` 1) , input (p `w` 2), input (p `w` 3), input (p `w` 4))

