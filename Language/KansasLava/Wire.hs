{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Wire where

import Language.KansasLava.Type
import Language.KansasLava.Entity
import Control.Applicative
import Control.Monad
import Data.Sized.Ix
import Data.Sized.Matrix as M hiding (S)
import Data.Word

-- | A 'Wire a' is an 'a' value that we can push over a wire.
class Wire w where
	-- | a way of adding unknown inputs to this wire.
	type X w

	-- check for bad things
	unX :: X w -> Maybe w

	-- and, put the good or bad things back.	
	optX :: Maybe w -> X w

	-- | Naming a component that generates this type of wire
   	wireName :: w -> String

	-- | Each wire has a known type.
    	wireType :: w -> BaseTy

-- | "RepWire' is a wire where we know the represnetation used to encode the bits.
class Wire w => RepWire w where
	-- | What is the width of this wire, in X-bits.
	type WIDTH w

	fromWireRep :: w -> Matrix (WIDTH w) Bool 
	toWireRep   :: Matrix (WIDTH w) Bool -> w

	-- | 'w' here is a witness, not a value
	maxWireRep :: w -> Integer

	-- | how we want to present this value, in comments
	showWireRep :: w -> String

liftX0 :: (Wire w1) => w1 -> X w1	
liftX0 = pureX

pureX :: (Wire w) => w -> X w
pureX = optX . Just
{-
liftX1 :: forall w1 w2 . (Wire w1, Wire w2) => (w1 -> w2) -> X w1 -> X w2
liftX1 f x1 = case unX x1 of
		Nothing -> errX (undefined :: w2) err
		Right v  -> pureX (f v :: w2)
liftX2 :: (Wire w1, Wire w2, Wire w3) => (w1 -> w2 -> w3) -> X w1 -> X w2 -> X w3
liftX2 = f x1 x2 = case
-}		

data WireVal a = WireUnknown | WireVal a

instance Monad WireVal where
	return = WireVal
	fail _ = WireUnknown
	WireUnknown >>= _ = WireUnknown
	WireVal a >>= f = f a
	
instance Functor WireVal where
	fmap f WireUnknown = WireUnknown
	fmap f (WireVal a) = WireVal (f a)

instance Applicative WireVal where
	pure = WireVal
	WireVal f <*> WireVal a = WireVal $ f a
	_ <*> _ = WireUnknown

instance Show a => Show (WireVal a) where
	show WireUnknown = "?"
	show (WireVal a) = show a

instance (Wire a, Wire b) => Wire (a,b) where
	type X (a,b) 		= (X a, X b)
	optX (Just (a,b)) 	= (pureX a, pureX b)
	optX Nothing		= (optX (Nothing :: Maybe a), optX (Nothing :: Maybe b))
	unX (a,b) = do x <- unX a
		       y <- unX b
		       return $ (x,y)
	wireName _ = "Tuple_2"
	wireType ~(a,b) = TupleTy [wireType a, wireType b]

instance Wire Bool where 
	type X Bool 	= WireVal Bool
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire Bool"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Bool"
	wireName _	= "Bool"
	wireType _	= B

instance RepWire Bool where
	type WIDTH Bool	= X1
	toWireRep m  		= m ! 0
	fromWireRep v 		= matrix [v]
	maxWireRep _		= 1
	showWireRep 		= show

instance Wire Int where 	
	type X Int 	= Maybe Int
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Int"
	unX a		= a
	wireName _	= "Int"
	wireType _	= S 32		-- hmm. Not really on 64 bit machines.
		
instance Wire Word32 where 	
	type X Word32 	= Maybe Word32
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Int"
	unX a		= a
	wireName _	= "Word32"
	wireType _	= U 32

instance Wire Integer where 	
	type X Integer 	= Maybe Integer
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Int"
	unX a		= a
	wireName _	= "Integer"
	wireType _	= IntegerTy

instance (Num ix, Size ix, Wire a) => Wire (Matrix ix a) where 
	type X (Matrix ix a) = Matrix ix (X a)
	optX (Just m)	= fmap (optX . Just) m
	optX Nothing	= forAll $ \ ix -> optX (Nothing :: Maybe a)
	unX m		= liftM matrix $ sequence (map (\ i -> unX (m ! i)) (indices m))
	wireName _ 	= "Matrix"
	wireType m	= MatrixTy (size (ix m)) (wireType (a m))
		where
			ix :: Matrix ix a -> ix
			ix = undefined
			a :: Matrix ix a -> a
			a = undefined

type MUL a b = a

instance (Num ix, Size ix, Wire a) => RepWire (Matrix ix a) where
	type WIDTH (Matrix ix a) = MUL ix (WIDTH a)

	
		