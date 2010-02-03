{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Wire where

import Language.KansasLava.Type
import Language.KansasLava.Entity
import Control.Applicative
import Data.Word

-- | A 'Wire a' is an 'a' value that we can push over a wire.
class Wire w where
	-- | a way of adding unknown inputs to this wire.
	type X w

	-- | lift a 'w' into an 'X w'.
	pureX :: w -> X w

	-- 'w' is a witness to the type.
	errX :: w -> X w

	unX :: X w -> Maybe w

	-- | Naming a component that generates this type of wire
   	wireName :: w -> String

	-- | Each wire has a known type.
    	wireType :: w -> BaseTy

-- | "RepWire' is a wire where we know the represnetation used to encode the bits.
class Wire w => RepWire w where
	fromWireRep   :: w -> Integer
	toWireRep :: Integer -> w

	-- | 'w' here is a witness, not a value
	maxWireRep :: w -> Integer

	-- | how we want to present this value, in comments
	showWireRep :: w -> String

liftX0 :: (Wire w1) => w1 -> X w1	
liftX0 = pureX
{-
liftX1 :: forall w1 w2 . (Wire w1, Wire w2) => (w1 -> w2) -> X w1 -> X w2
liftX1 f x1 = case unX x1 of
		Nothing -> errX (undefined :: w2) err
		Right v  -> pureX (f v :: w2)
liftX2 :: (Wire w1, Wire w2, Wire w3) => (w1 -> w2 -> w3) -> X w1 -> X w2 -> X w3
liftX2 = f x1 x2 = case
-}		

instance (Wire a, Wire b) => Wire (a,b) where
	type X (a,b) = (X a, X b)
	pureX (a,b) = (pureX a, pureX b)
	errX ~(a,b) = (errX a, errX b)
	unX (a,b) = do x <- unX a
		       y <- unX b
		       return $ (x,y)
	wireName ~(a,b) = "Tuple_2"
	wireType (a,b) = error "tuple type"

instance Wire Bool where 
	type X Bool 	= Maybe Bool
	pureX b 	= return b
	errX _ 		= fail "Wire Bool"
	unX a		= a
	wireName _	= "Bool"
	wireType _	= B

instance RepWire Bool where
	toWireRep 0 		= False
	toWireRep 1 		= True
	fromWireRep False 	= 0
	fromWireRep True 	= 1
	maxWireRep _		= 1
	showWireRep 		= show

	
instance Wire Word32 where 	
	type X Word32 	= Maybe Word32
	pureX b 	= return b
	errX _ 		= fail "Wire Word32"
	unX a		= a
	wireName _	= "Word32"
	wireType _	= U 32
	
	