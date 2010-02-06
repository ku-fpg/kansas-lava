{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Wire where

import Language.KansasLava.Type
import Language.KansasLava.Entity
import Control.Applicative
import Control.Monad
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Sized.Matrix hiding (S)
import qualified Data.Sized.Matrix as M
import Data.Sized.Unsigned as U 
import Data.Word
import Data.Bits
import qualified Data.Traversable as T

-- | A 'Wire a' is an 'a' value that we can push over a wire.
class Eq w => Wire w where
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

	toWireRep   :: (Size (WIDTH w)) => Matrix (WIDTH w) Bool -> Maybe w
	fromWireRep :: (Size (WIDTH w)) => w -> Matrix (WIDTH w) Bool 

	-- | how we want to present this value, in comments
	-- The first value is the witness.
	showRepWire :: w -> X w -> String

liftX0 :: (Wire w1) => w1 -> X w1	
liftX0 = pureX

pureX :: (Wire w) => w -> X w
pureX = optX . Just

-------------------------------------------------------------------------------------

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

-----------------------------------------------------------------------------------------

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
	toWireRep m  		= return $ m ! 0
	fromWireRep v 		= matrix [v]
	showRepWire _ = show

instance Wire Int where 	
	type X Int 	= Maybe Int
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Int"
	unX a		= a
	wireName _	= "Int"
	wireType _	= S 32		-- hmm. Not really on 64 bit machines.
		
instance RepWire Int where
	type WIDTH Int	= X32
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral 
	showRepWire _ = show

				
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

-------------------------------------------------------------------------------------
-- Now the containers

instance (Wire a, Wire b) => Wire (a,b) where
	type X (a,b) 		= (X a, X b)
	optX (Just (a,b)) 	= (pureX a, pureX b)
	optX Nothing		= (optX (Nothing :: Maybe a), optX (Nothing :: Maybe b))
	unX (a,b) = do x <- unX a
		       y <- unX b
		       return $ (x,y)
	wireName _ = "Tuple_2"
	wireType ~(a,b) = TupleTy [wireType a, wireType b]

instance (RepWire a, RepWire b) => RepWire (a,b) where
	type WIDTH (a,b)	= ADD (WIDTH a) (WIDTH b)
--	toWireRep m  		= return $ m ! 0
--	fromWireRep v 		= matrix [v]
	showRepWire (a,b) (x,y) = "(" ++ showRepWire a x ++ "," ++ showRepWire b y ++ ")"
	

instance (Size ix, Wire a) => Wire (Matrix ix a) where 
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
--	showWire _ = show
	
instance forall a ix . (Size (WIDTH a), RepWire a, Size ix, Wire a) => RepWire (Matrix ix a) where
	type WIDTH (Matrix ix a) = MUL ix (WIDTH a)
	
--	toWireRep :: Matrix (WIDTH w) Bool -> Matrix ix a
	toWireRep = T.traverse toWireRep
		  . rows 
		  . squash 

--	fromWireRep :: Matrix ix a -> Matrix (WIDTH w) Bool 
	fromWireRep = squash . joinRows . T.traverse fromWireRep 

	showRepWire _ = show . M.toList . fmap (M.S . showRepWire (undefined :: a))

instance (Enum ix, Size ix) => Wire (Unsigned ix) where 
	type X (Unsigned ix) = WireVal (Unsigned ix)
	optX (Just b)	    = return b
	optX Nothing	    = fail "Wire Int"
	unX (WireVal a)     = return a
	unX (WireUnknown)   = fail "Wire Int"
	wireName _	    = "Unsigned"
	wireType x   	    = U (bitSize x)
	
instance (Enum ix, Size ix) => RepWire (Unsigned ix) where 
	type WIDTH (Unsigned ix) = ix
	fromWireRep a = toMatrix a
	toWireRep = return . fromMatrix
	showRepWire _ = show
