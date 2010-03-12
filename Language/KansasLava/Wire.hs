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
import Data.Sized.Signed as SS 
import qualified Data.Sized.Sampled as Sampled
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

	wireCapture :: D w -> [(BaseTy, Driver E)]
	wireCapture (D d) = [(wireType (error "wireCapture" :: w), d)]

	wireGenerate :: [String] -> (D w,[String])
	wireGenerate (v:vs) = (D (Pad (Var v)),vs)

-- D w -> 

-- | "RepWire' is a wire where we know the represnetation used to encode the bits.
class (Wire w, Size (WIDTH w)) => RepWire w where
	-- | What is the width of this wire, in X-bits.
	type WIDTH w

		-- TODO: consider using Integer here.
	toWireRep   :: {- (Size (WIDTH w)) => -} Matrix (WIDTH w) Bool -> Maybe w
	fromWireRep :: {- (Size (WIDTH w)) => -} w -> Matrix (WIDTH w) Bool 

	-- | how we want to present this value, in comments
	-- The first value is the witness.
	showRepWire :: w -> X w -> String


allWireReps :: forall width . (Size width) => [Matrix width Bool]
allWireReps = [U.toMatrix count | count <- counts ]
   where
	counts :: [Unsigned width]
	counts = [0..2^(fromIntegral sz)-1]
	sz = size (error "allWireRep" :: width)

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
	type X Int 	= WireVal Int
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Int"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Int"
	wireName _	= "Int"
	wireType _	= S 32		-- hmm. Not really on 64 bit machines.
		
instance RepWire Int where
	type WIDTH Int	= X32
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral 
	showRepWire _ = show

instance Wire Word8 where 	
	type X Word8 	= WireVal Word8
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Word8"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Word8"
	wireName _	= "Word8"
	wireType _	= U 8
		
instance RepWire Word8 where
	type WIDTH Word8 = X8
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral 
	showRepWire _ = show
	
instance Wire () where 	
	type X () 	= WireVal ()
	optX (Just b)	= return b
	optX Nothing	= fail "Wire ()"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire ()"
	wireName _	= "Unit"
	wireType _	= U 1
		
instance RepWire () where
	type WIDTH () = X1	-- should really be X0
	toWireRep _ = return ()
	fromWireRep () = M.matrix [True]
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
	optX Nothing	= fail "Wire Integer"
	unX a		= a
	wireName _	= "Integer"
	wireType _	= IntegerTy

instance RepWire Integer where
	type WIDTH Integer = X0
	showRepWire _	= show

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
	wireCapture (D d) = [ (wireType (error "wireCapture (a,)" :: a),Port (Var "o0") $ E $ eFst)
			    , (wireType (error "wireCapture (,b)" :: b),Port (Var "o0") $ E $ eSnd)
			    ]
           where
		eFst = Entity (Name "Lava" "fst") 
			      [(Var "o0",wireType (error "wireGenerate (a,)" :: a))]
			      [(Var "i0",wireType (error "wireGenerate (a,b)" :: (a,b)),d)]
			      []
		eSnd = Entity (Name "Lava" "snd") 
			      [(Var "o0",wireType (error "wireGenerate (,b)" :: b))]
			      [(Var "i0",wireType (error "wireGenerate (a,b)" :: (a,b)),d)]
			      []


	wireGenerate vs0 = (D (Port (Var "o0") $ E ePair),vs2)
	   where
		(D p1,vs1) = wireGenerate vs0 :: (D a,[String])
		(D p2,vs2) = wireGenerate vs1 :: (D b,[String])
		ePair = Entity (Name "Lava" "pair")
			      [(Var "o0",wireType (error "wireGenerate (a,b)" :: (a,b)))]
			      [(Var "i0",wireType (error "wireGenerate (a,)" :: a),p1)
			      ,(Var "i1",wireType (error "wireGenerate (,b)" :: b),p2)
			      ]
			      []



instance (t ~ ADD (WIDTH a) (WIDTH b), Size t, Enum t, RepWire a, RepWire b) => RepWire (a,b) where
	type WIDTH (a,b)	= ADD (WIDTH a) (WIDTH b)
--	toWireRep m  		= return $ m ! 0
--	fromWireRep v 		= matrix [v]
	showRepWire ~(a,b) (x,y) = "(" ++ showRepWire a x ++ "," ++ showRepWire b y ++ ")"

-- Not for now; to consider
{-
instance (Wire a) => Wire (Maybe a) where
	type X (Maybe a) = X (Bool, a)
	optX Nothing 	 = (optX (Nothing :: Maybe Bool), optX (Nothing :: Maybe a))
	unX (a,b) 	 = do x <- unX (a :: X Bool) :: Maybe Bool
			      y <- unX (b :: X a) :: Maybe a
			      return $ if x then Just y else Nothing
-}

instance (Size ix, Wire a) => Wire (Matrix ix a) where 
	type X (Matrix ix a) = Matrix ix (X a)
	optX (Just m)	= fmap (optX . Just) m
	optX Nothing	= forAll $ \ ix -> optX (Nothing :: Maybe a)
	unX m		= liftM matrix $ sequence (map (\ i -> unX (m ! i)) (indices m))
	wireName _ 	= "Matrix"
	wireType m	= MatrixTy (size (ix m)) (wireType (a m))
		where
			ix :: Matrix ix a -> ix
			ix = error "ix/Matrix"
			a :: Matrix ix a -> a
			a = error "a/Matrix"
--	showWire _ = show
	
instance forall a ix t . (t ~ WIDTH a, Size t, Size (MUL ix t), Enum (MUL ix t), RepWire a, Size ix, Wire a) => RepWire (Matrix ix a) where
	type WIDTH (Matrix ix a) = MUL ix (WIDTH a)
	
--	toWireRep :: Matrix (WIDTH w) Bool -> Matrix ix a
	toWireRep = T.traverse toWireRep
		  . rows 
		  . squash 

--	fromWireRep :: Matrix ix a -> Matrix (WIDTH w) Bool 
	fromWireRep = squash . joinRows . T.traverse fromWireRep 

	showRepWire _ = show . M.toList . fmap (M.S . showRepWire (error "show/Matrix" :: a))

instance (Enum ix, Size ix) => Wire (Unsigned ix) where 
	type X (Unsigned ix) = WireVal (Unsigned ix)
	optX (Just b)	    = return b
	optX Nothing	    = fail "Wire Int"
	unX (WireVal a)     = return a
	unX (WireUnknown)   = fail "Wire Int"
	wireName _	    = "Unsigned"
	wireType x   	    = U (size (error "Wire/Unsigned" :: ix))
	
instance (Enum ix, Size ix) => RepWire (Unsigned ix) where 
	type WIDTH (Unsigned ix) = ix
	fromWireRep a = U.toMatrix a
	toWireRep = return . U.fromMatrix
	showRepWire _ = show

instance (Enum ix, Size ix) => Wire (Signed ix) where 
	type X (Signed ix) = WireVal (Signed ix)
	optX (Just b)	    = return b
	optX Nothing	    = fail "Wire Int"
	unX (WireVal a)     = return a
	unX (WireUnknown)   = fail "Wire Int"
	wireName _	    = "Signed"
	wireType x   	    = S (size (error "Wire/Signed" :: ix))
	
instance (Enum ix, Size ix) => RepWire (Signed ix) where 
	type WIDTH (Signed ix) = ix
	fromWireRep a = SS.toMatrix a
	toWireRep = return . SS.fromMatrix
	showRepWire _ = show

instance (Size m, Enum ix, Size ix) => Wire (Sampled.Sampled m ix) where 
	type X (Sampled.Sampled m ix) = WireVal (Sampled.Sampled m ix)
	optX (Just b)	    = return b
	optX Nothing	    = fail "Wire Sampled"
	unX (WireVal a)     = return a
	unX (WireUnknown)   = fail "Wire Sampled"
	wireName _	    = "Sampled"
	wireType x   	    = U (size (error "Sampled" :: ix))

instance (Size m, Enum ix, Enum m, Size ix) => RepWire (Sampled.Sampled m ix) where 
	type WIDTH (Sampled.Sampled m ix) = ix
	fromWireRep a = Sampled.toMatrix a
	toWireRep = return . Sampled.fromMatrix
	showRepWire _ = show

-----------------------------------------------------------------------------

log2 :: Int -> Int
log2 0 = 0
log2 1 = 1
log2 n = log2 (n `shiftR` 1) + 1

-- Perhaps not, because what does X0 really mean over a wire, vs X1.
{-
instance Wire X0 where
	type X X0 = X0		-- there is not information here
	optX (Just x)	    = x
	optX Nothing	    = X0
	unX x		    = return x
	wireName _	    = "X0"
	wireType _	    = U 0
-}	
instance (Size x) => Wire (X0_ x) where
	type X (X0_ x)	= WireVal (X0_ x)
	optX (Just x) 	= return x
	optX Nothing	= fail "X0_"
	unX (WireVal a) = return a
	unX WireUnknown = fail "X0_"
	wireName _ 	= "X" ++ show (size (error "wireName" :: X0_ x))
	wireType _ 	= U (log2 $ (size (error "wireType" :: X0_ x) - 1))
	

instance (Size (WIDTH (X0_ x)), Enum (WIDTH (X0_ x)), Integral (X0_ x), Size x) => RepWire (X0_ x) where
	type WIDTH (X0_ x) = LOG (SUB (X0_ x) X1)
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral 
	showRepWire _ = show

instance (Size x) => Wire (X1_ x) where
	type X (X1_ x)	= WireVal (X1_ x)
	optX (Just x) 	= return x
	optX Nothing	= fail "X1_"
	unX (WireVal a) = return a
	unX WireUnknown = fail "X1_"
	wireName _ 	= "X" ++ show (size (error "wireName" :: X1_ x))
	wireType _ 	= U (log2 $ (size (error "wireType" :: X1_ x) - 1))	

instance (Size (WIDTH (X1_ x)), Enum (WIDTH (X1_ x)), Integral (X1_ x), Size x) => RepWire (X1_ x) where
	type WIDTH (X1_ x) = LOG (SUB (X1_ x) X1)
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral 
	showRepWire _ = show
		
-- Some tests

test1 :: (WIDTH X1 ~ X0) => ()
test1 = ()

test2 :: (WIDTH X2 ~ X1) => ()
test2 = ()

test3 :: (WIDTH X3 ~ X2) => ()
test3 = ()

test4 :: (WIDTH X4 ~ X2) => ()
test4 = ()

test5 :: (WIDTH X5 ~ X3) => ()
test5 = ()
-----------------------------------------------------------------------------------------

-- Wire used in simulation *only*, to show ideas.
data ALPHA = ALPHA String
	deriving (Eq, Ord)
	
instance Show ALPHA where
	show (ALPHA str) = str

instance Wire ALPHA where 
	type X ALPHA 	= WireVal ALPHA
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire ALPHA"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire ALPHA"
	wireName _	= "ABC"
	wireType _	= U 0

instance RepWire ALPHA where
	type WIDTH ALPHA	= X0
	toWireRep m  		= return $ ALPHA ""
	fromWireRep v 		= matrix []
	showRepWire _ = show




-----------------------------------------------------------------------------
