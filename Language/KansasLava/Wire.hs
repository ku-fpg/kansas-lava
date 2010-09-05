{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts, DeriveDataTypeable,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp, TypeSynonymInstances  #-}

module Language.KansasLava.Wire where

import Language.KansasLava.StdLogicVector
import Language.KansasLava.Types
import Language.KansasLava.Entity
import Control.Applicative
import Control.Monad
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Sized.Matrix hiding (S)
import qualified Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Data.Sized.Signed as S
import qualified Data.Sized.Sampled as Sampled
import Data.Word
import Data.Bits
import qualified Data.Traversable as T
import qualified Data.Maybe as Maybe
import Data.Dynamic

-- | A 'Wire a' is an 'a' value that we 'Rep'resents, aka we can push it over a wire.
class Rep w where
	-- | a way of adding unknown inputs to this wire.
	type X w

	-- check for bad things
	unX :: X w -> Maybe w

	-- and, put the good or bad things back.
	optX :: Maybe w -> X w

	-- | Each wire has a known type.
	-- TODO: call repTupe
    	wireType :: w -> Type


	-- convert to binary (rep) format
	toRep   :: w -> X w -> RepValue
	-- convert from binary (rep) format
	fromRep :: w -> RepValue -> X w

	-- show the value (in its Haskell form, default is the bits)
	showRep :: w -> X w -> String
	showRep w x = show (toRep w x)

-- D w ->

-- | "RepWire' is a wire where we know the represnetation used to encode the bits.
class (Rep w, Size (WIDTH w)) => RepWire w where
	-- | What is the width of this wire, in X-bits.
	type WIDTH w

		-- TODO: consider using Integer here.
		-- NOTE: no way of having 'X' inside the Bool values ; consider.
	toWireRep   :: Matrix (WIDTH w) Bool -> Maybe w
	fromWireRep :: w -> Matrix (WIDTH w) Bool


	-- toWireXRep (and fromWireXRep) can only be used on/after a whole cycle, and
	-- not intra-cycle. Lazy loops that share Wires will be forced(!)
	toWireXRep :: w -> Matrix (WIDTH w) (X Bool) -> X w
	toWireXRep wt m = optX (if isValidWireRep wt m
			        then toWireRep (fmap (noFail :: X Bool -> Bool) m :: Matrix (WIDTH w) Bool) :: Maybe w
				else Nothing :: Maybe w
			       )
		where noFail :: X Bool -> Bool
		      noFail x = case unX x of
				   Nothing -> error "toWireXRep: non value"
				   Just v -> v

	-- need to pass in witness
	fromWireXRep :: w -> X w -> Matrix (WIDTH w) (X Bool)
	fromWireXRep _wt w = case unX w :: Maybe w of
			   Nothing -> (forAll $ \ _ -> optX (Nothing :: Maybe Bool) :: X Bool) :: Matrix (WIDTH w) (X Bool)
			   Just v -> fmap pureX (fromWireRep v)

	-- | how we want to present this value, in comments
	-- The first value is the witness.
	showRepWire :: w -> X w -> String

-- Checks to see if a representation is completely valid,
-- such that toWireRep on Matrix (WIDTH w) Bool will succeed.
isValidWireRep :: forall w .(Size (WIDTH w)) => w -> Matrix (WIDTH w) (X Bool) -> Bool
isValidWireRep w m = and $ fmap isGood $ M.toList m
   where
	isGood :: X Bool -> Bool
	isGood x  = case (unX x :: Maybe Bool) of
		      Nothing -> False
		      Just _  -> True



allWireReps :: forall width . (Size width) => [Matrix width Bool]
allWireReps = [U.toMatrix count | count <- counts ]
   where
	counts :: [Unsigned width]
	counts = [0..2^(fromIntegral sz)-1]
	sz = size (error "allWireRep" :: width)

-- Give me all possible (non-X) representations (2^n of them).
allReps :: Int -> [RepValue]
allReps n = [ RepValue (fmap WireVal count) | count <- counts n ]
   where
	counts :: Int -> [[Bool]]
	counts 0 = [[]] 
	counts n = [ x : xs |  xs <- counts (n-1), x <- [False,True] ]


allOkayRep :: (Size w) => Matrix w (X Bool) -> Maybe (Matrix w Bool)
allOkayRep m | okay      = return (fmap (\ (WireVal a) -> a) m)
	     | otherwise = Nothing
	where okay = and (map (\ x -> case x of
					   WireUnknown -> False
					   _ -> True) (M.toList m))

-- toWriteRep' :: (Size w) => (Matrix w Bool -> a) -> Matrix w (

liftX0 :: (Rep w1) => w1 -> X w1
liftX0 = pureX

pureX :: (Rep w) => w -> X w
pureX = optX . Just

-- Not possible to use!
--failX :: forall w . (Wire w) => X w
--failX = optX (Nothing :: Maybe w)

-- This is not wired into the class because of the extra 'Show' requirement.

showRepDefault :: forall w. (Show w, Rep w) => w -> X w -> String
showRepDefault w v = case unX v :: Maybe w of
			Nothing -> "?"
			Just v' -> show v'


-- TODO: consider not using the sized types here, and just using standard counters.
toRepFromSigned :: forall w v . (Rep v, Integral v, Size w) => w -> v -> X v -> RepValue
toRepFromSigned w1 w2 v =
		RepValue $ M.toList $ case unX v :: Maybe v of
			    Nothing -> (forAll $ \ i -> WireUnknown) :: M.Matrix w (WireVal Bool)
			    Just v' -> fmap WireVal $ S.toMatrix (fromIntegral v')

fromRepToSigned :: forall w v . (Rep v, Integral v, Size w) => w -> v -> RepValue -> X v 
fromRepToSigned w1 w2 r = optX (fmap (\ xs -> fromIntegral $ S.fromMatrix (M.matrix xs :: Matrix w Bool)) 
				     (getValidRepValue r) :: Maybe v)

toRepFromUnsigned :: forall w v . (Rep v, Integral v, Size w) => w -> v -> X v -> RepValue
toRepFromUnsigned w1 w2 v =
		RepValue $ M.toList $ case unX v :: Maybe v of
			    Nothing -> (forAll $ \ i -> WireUnknown) :: M.Matrix w (WireVal Bool)
			    Just v' -> fmap WireVal $ U.toMatrix (fromIntegral v')

fromRepToUnsigned :: forall w v . (Rep v, Integral v, Size w) => w -> v -> RepValue -> X v 
fromRepToUnsigned w1 w2 r = optX (fmap (\ xs -> fromIntegral $ U.fromMatrix (M.matrix xs :: Matrix w Bool)) 
				     (getValidRepValue r) :: Maybe v)

-------------------------------------------------------------------------------------

instance Rep Bool where
	type X Bool 	= WireVal Bool
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire Bool"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Bool"
	wireType _	= B
	toRep w v	= RepValue [v]
	fromRep w (RepValue [v]) = v
	fromRep w _		 = error "size error for Bool"

instance RepWire Bool where
	type WIDTH Bool	= X1
	toWireRep m	= return $ m ! 0
	fromWireRep v 	= matrix [v]
	showRepWire _ (WireUnknown) = "?"
	showRepWire _ (WireVal True) = "T"
	showRepWire _ (WireVal False) = "F"

instance Rep Int where
	type X Int 	= WireVal Int
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Int"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Int"
--	wireName _	= "Int"
	wireType _	= S 32		-- hmm. Not really on 64 bit machines.

	toRep = toRepFromSigned (witness :: X32)
	fromRep = fromRepToSigned (witness :: X32)	
	showRep = showRepDefault

instance RepWire Int where
	type WIDTH Int	= X32
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral
	showRepWire _ = show

instance Rep Word8 where
	type X Word8 	= WireVal Word8
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Word8"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Word8"
	wireType _	= U 8
	toRep = toRepFromUnsigned (witness :: X8)
	fromRep = fromRepToUnsigned (witness :: X8)	
	showRep = showRepDefault

instance RepWire Word8 where
	type WIDTH Word8 = X8
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral
	showRepWire _ = show

instance Rep Word32 where
	type X Word32 	= WireVal Word32
	optX (Just b)	= return b
	optX Nothing	= fail "Wire Word32"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Word32"
	wireType _	= U 32
	toRep = toRepFromUnsigned (witness :: X32)
	fromRep = fromRepToUnsigned (witness :: X32)	
	showRep = showRepDefault

instance RepWire Word32 where
	type WIDTH Word32 = X32
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral
	showRepWire _ = show

instance Rep () where
	type X () 	= WireVal ()
	optX (Just b)	= return b
	optX Nothing	= fail "Wire ()"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire ()"
--	wireName _	= "Unit"
	wireType _	= V 1	-- should really be V 0 TODO
	toRep _ _ = RepValue []
	fromRep _ _ = return ()
	showRep _ _ = "()"

instance RepWire () where
	type WIDTH () = X1	-- should really be X0
	toWireRep _ = return ()
	fromWireRep () = M.matrix [True]
	showRepWire _ = show

instance Rep Integer where
	type X Integer 	= Integer	-- No fail/unknown value
	optX (Just b)	= b
	optX Nothing	= error "Generic failed in optX"
	unX a		= return a
	wireType _	= GenericTy
	toRep = error "can not turn a Generic to a Rep"
	fromRep = error "can not turn a Rep to a Generic"
	showRep _ v = show v

instance RepWire Integer where
	type WIDTH Integer = X0
	showRepWire _	= show


-------------------------------------------------------------------------------------
-- Now the containers

instance (Rep a, Rep b) => Rep (a,b) where
	type X (a,b) 		= (X a, X b)
	optX (Just (a,b)) 	= (pureX a, pureX b)
	optX Nothing		= (optX (Nothing :: Maybe a), optX (Nothing :: Maybe b))
	unX (a,b) = do x <- unX a
		       y <- unX b
		       return $ (x,y)
--	wireName _ = "Tuple_2"

	wireType ~(a,b) = TupleTy [wireType a, wireType b]
{-
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
-}

	toRep _ (a,b) = RepValue (avals ++ bvals)
		where (RepValue avals) = toRep (witness :: a) a
		      (RepValue bvals) = toRep (witness :: b) b
	fromRep w (RepValue vs) = ( fromRep (witness :: a) (RepValue (take size_a vs))
				  , fromRep (witness :: b) (RepValue (drop size_a vs))
				  )
		where size_a = typeWidth (wireType w)
	showRep _ (a,b) = "(" ++ showRep (witness :: a) a ++ "," ++ showRep (witness :: b) b ++ ")"


instance (t ~ ADD (WIDTH a) (WIDTH b), Size t, Enum t, RepWire a, RepWire b) => RepWire (a,b) where
	type WIDTH (a,b)	= ADD (WIDTH a) (WIDTH b)
--	toWireRep m  		= return $ m ! 0
	fromWireRep (a,b) 	= M.matrix (M.toList (fromWireRep a) ++ M.toList (fromWireRep b))
	fromWireXRep w (a,b)    = M.matrix (M.toList (fromWireXRep (error "witness" :: a) a) ++
					    M.toList (fromWireXRep (error "witness" :: b) b))
	showRepWire ~(a,b) (x,y) = "(" ++ showRepWire a x ++ "," ++ showRepWire b y ++ ")"


instance (Rep a, Rep b, Rep c) => Rep (a,b,c) where
	type X (a,b,c) 		= (X a, X b, X c)
	optX (Just (a,b,c)) 	= (pureX a, pureX b,pureX c)
	optX Nothing		= ( optX (Nothing :: Maybe a),
				    optX (Nothing :: Maybe b),
				    optX (Nothing :: Maybe c) )
	unX (a,b,c)
		  = do x <- unX a
		       y <- unX b
		       z <- unX c
		       return $ (x,y,z)
{-
--	TO ADD
	toRep _ (a,b) = RepValue (avals ++ bvals)
		where (RepValue avals) = toRep (witness :: a) a
		      (RepValue bvals) = toRep (witness :: b) b
	fromRep w (RepValue vs) = ( fromRep (witness :: a) (RepValue (take size_a vs))
				  , fromRep (witness :: b) (RepValue (drop size_a vs))
				  )
		where size_a = typeWidth (wireType w)
-}

	wireType ~(a,b,c) = TupleTy [wireType a, wireType b,wireType c]
	showRep _ (a,b,c) = "(" ++ showRep (witness :: a) a ++ 
			    "," ++ showRep (witness :: b) b ++ 
			    "," ++ showRep (witness :: c) c ++ ")"

instance (t ~ ADD (WIDTH a) (ADD (WIDTH b) (WIDTH c)), Size t, Enum t, RepWire a, RepWire b,RepWire c) => RepWire (a,b,c) where
	type WIDTH (a,b,c)	= ADD (WIDTH a) (ADD (WIDTH b) (WIDTH c))
--	toWireRep m  		= return $ m ! 0
	fromWireRep (a,b,c) 	= M.matrix (M.toList (fromWireRep a) ++ M.toList (fromWireRep b) ++ M.toList (fromWireRep c))
	fromWireXRep w (a,b,c)  = M.matrix (M.toList (fromWireXRep (error "witness" :: a) a) ++
					    M.toList (fromWireXRep (error "witness" :: b) b) ++
					    M.toList (fromWireXRep (error "witness" :: c) c))
	showRepWire ~(a,b,c) (x,y,z) = "(" ++ showRepWire a x ++ "," ++ showRepWire b y ++ "," ++ showRepWire c z ++ ")"


instance (Rep a) => Rep (Maybe a) where
	-- not completely sure about this representation
	type X (Maybe a) = (X Bool, X a)
	optX b		= ( case b of
			      Nothing -> optX (Nothing :: Maybe Bool)
			      Just Nothing   -> optX (Just False :: Maybe Bool)
			      Just (Just {}) -> optX (Just True :: Maybe Bool)
			  , case b of
				Nothing       -> optX (Nothing :: Maybe a)
				Just Nothing  -> optX (Nothing :: Maybe a)
				Just (Just a) -> optX (Just a :: Maybe a)
			  )
	unX (a,b) 	= case unX a :: Maybe Bool of
			    Nothing    -> Nothing
			    Just True  -> Just $ unX b
			    Just False -> Just Nothing
--	wireName _	= "Maybe<" ++ wireName (error "witness" :: a) ++ ">"
	wireType _	= TupleTy [ B, wireType (error "witness" :: a)] 

	toRep _ (a,b) = RepValue (avals ++ bvals)
		where (RepValue avals) = toRep (witness :: Bool) a
		      (RepValue bvals) = toRep (witness :: a) b
	fromRep w (RepValue vs) = ( fromRep (witness :: Bool) (RepValue (take 1 vs))
				  , fromRep (witness :: a) (RepValue (drop 1 vs))
				  )
	showRep w (WireUnknown,a) = "?"
	showRep w (WireVal True,a) = "Just " ++ showRep (witness :: a) a
	showRep w (WireVal False,a) = "Nothing"

--instance Size (ADD X1 a) => Size a where

instance (RepWire a, Size (ADD X1 (WIDTH a))) => RepWire (Maybe a) where
	type WIDTH (Maybe a) = WIDTH (Bool,a)
--	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep Nothing  = M.matrix $ take sz $ repeat False
	    where sz = 1 + size (error "witness" :: (WIDTH a))
	fromWireRep (Just a) = M.matrix (True : M.toList (fromWireRep a))

	fromWireXRep w (en,val) = M.matrix (M.toList (fromWireXRep (error "witness" :: Bool) en) ++
					    M.toList (fromWireXRep (error "witness" :: a) val))

	showRepWire w (WireUnknown,a) = "?"
	showRepWire w (WireVal True,a) = "Just " ++ showRepWire (error "witness" :: a) a
	showRepWire w (WireVal False,a) = "Nothing"


instance (Size ix, Rep a) => Rep (Matrix ix a) where
	type X (Matrix ix a) = Matrix ix (X a)
	optX (Just m)	= fmap (optX . Just) m
	optX Nothing	= forAll $ \ ix -> optX (Nothing :: Maybe a)
	unX m		= liftM matrix $ sequence (map (\ i -> unX (m ! i)) (indices m))
--	wireName _ 	= "Matrix"
	wireType m	= MatrixTy (size (ix m)) (wireType (a m))
		where
			ix :: Matrix ix a -> ix
			ix = error "ix/Matrix"
			a :: Matrix ix a -> a
			a = error "a/Matrix"
--	showWire _ = show

instance forall a ix t . (t ~ WIDTH a, Size t, Size (MUL ix t), Enum (MUL ix t), RepWire a, Size ix, Rep a) => RepWire (Matrix ix a) where
	type WIDTH (Matrix ix a) = MUL ix (WIDTH a)

--	toWireRep :: Matrix (WIDTH w) Bool -> Matrix ix a
	toWireRep = T.traverse toWireRep
		  . columns
		  . squash

--	fromWireRep :: Matrix ix a -> Matrix (WIDTH w) Bool
	fromWireRep = squash . joinColumns . T.traverse fromWireRep

--	fromWireRep :: Matrix ix (X a) -> Matrix (WIDTH w) (X Bool)
	fromWireXRep w = squash . joinColumns . T.traverse (fromWireXRep (error "witness" :: a))


	showRepWire _ = show . M.toList . fmap (M.S . showRepWire (error "show/Matrix" :: a))

instance (Size ix) => Rep (Unsigned ix) where
	type X (Unsigned ix) = WireVal (Unsigned ix)
	optX (Just b)	    = return b
	optX Nothing	    = fail "Wire Int"
	unX (WireVal a)     = return a
	unX (WireUnknown)   = fail "Wire Int"
	wireType x   	    = U (size (error "Wire/Unsigned" :: ix))
	toRep = toRepFromUnsigned (witness :: ix)
	fromRep = fromRepToUnsigned (witness :: ix)	
	showRep = showRepDefault

instance (Enum ix, Size ix) => RepWire (Unsigned ix) where
	type WIDTH (Unsigned ix) = ix
	fromWireRep a = U.toMatrix a
	toWireRep = return . U.fromMatrix
	showRepWire _ = show

instance (Size ix) => Rep (Signed ix) where
	type X (Signed ix) = WireVal (Signed ix)
	optX (Just b)	    = return b
	optX Nothing	    = fail "Wire Int"
	unX (WireVal a)     = return a
	unX (WireUnknown)   = fail "Wire Int"
--	wireName _	    = "Signed"
	wireType x   	    = S (size (error "Wire/Signed" :: ix))
	toRep = toRepFromSigned (witness :: ix)
	fromRep = fromRepToSigned (witness :: ix)	
	showRep = showRepDefault

instance (Size ix) => RepWire (Signed ix) where
	type WIDTH (Signed ix) = ix
	fromWireRep a = S.toMatrix a
	toWireRep = return . S.fromMatrix
	showRepWire _ = show

instance (Size m, Size ix) => Rep (Sampled.Sampled m ix) where
	type X (Sampled.Sampled m ix) = WireVal (Sampled.Sampled m ix)
	optX (Just b)	    = return b
	optX Nothing	    = fail "Wire Sampled"
	unX (WireVal a)     = return a
	unX (WireUnknown)   = fail "Wire Sampled"
--	wireName _	    = "Sampled"		-- We use a sub-module Sampled to implement Sampled
	wireType x   	    = V (size (error "Sampled" :: ix))
--	toRep = toRepFromSigned (witness :: X32)
--	fromRep = fromRepToSigned (witness :: X32)	
	showRep = showRepDefault

instance (Size m, Enum ix, Enum m, Size ix) => RepWire (Sampled.Sampled m ix) where
	type WIDTH (Sampled.Sampled m ix) = ix
	fromWireRep a = Sampled.toMatrix a
	toWireRep = return . Sampled.fromMatrix
	showRepWire _ = show

instance (Size ix) => Rep (StdLogicVector ix) where 
	type X (StdLogicVector ix) = StdLogicVector ix
	optX (Just b)	    = b
	optX Nothing	    = StdLogicVector $ forAll $ \ _ -> WireUnknown
	unX a 		    = return a
	wireType x   	    = V (size (error "Wire/StdLogicVector" :: ix))
	toRep _ (StdLogicVector m) = RepValue (M.toList m)
	fromRep _ (RepValue vs) = StdLogicVector (M.matrix vs)
	showRep = showRepDefault

{-
instance (Size ix) => RepWire (StdLogicVector ix) where
	type WIDTH (StdLogicVector ix) = ix
	fromWireRep (StdLogicVector a) = a
	toWireRep = return . StdLogicVector
	showRepWire _ = show
-}

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
instance (Size x) => Rep (X0_ x) where
	type X (X0_ x)	= WireVal (X0_ x)
	optX (Just x) 	= return x
	optX Nothing	= fail "X0_"
	unX (WireVal a) = return a
	unX WireUnknown = fail "X0_"
--	wireName _ 	= "X" ++ show (size (error "wireName" :: X0_ x))
	wireType _ 	= U (log2 $ (size (error "wireType" :: X0_ x) - 1))


instance (Size (WIDTH (X0_ x)), Enum (WIDTH (X0_ x)), Integral (X0_ x), Size x) => RepWire (X0_ x) where
	type WIDTH (X0_ x) = LOG (SUB (X0_ x) X1)
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral
	showRepWire _ = show

instance (Size x) => Rep (X1_ x) where
	type X (X1_ x)	= WireVal (X1_ x)
	optX (Just x) 	= return x
	optX Nothing	= fail "X1_"
	unX (WireVal a) = return a
	unX WireUnknown = fail "X1_"
--	wireName _ 	= "X" ++ show (size (error "wireName" :: X1_ x))
	wireType _ 	= U (log2 $ (size (error "wireType" :: X1_ x) - 1))	

instance (Size (WIDTH (X1_ x)), Enum (WIDTH (X1_ x)), Integral (X1_ x), Size x) => RepWire (X1_ x) where
	type WIDTH (X1_ x) = LOG (SUB (X1_ x) X1)
	toWireRep = return . fromIntegral . U.fromMatrix
	fromWireRep = U.toMatrix . fromIntegral
	showRepWire _ = show

-- Some tests
{-
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
-}
-----------------------------------------------------------------------------------------

-- Wire used in simulation *only*, to show ideas.
data ALPHA = ALPHA String
	deriving (Eq, Ord)

instance Show ALPHA where
	show (ALPHA str) = str

instance Rep ALPHA where
	type X ALPHA 	= WireVal ALPHA
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire ALPHA"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire ALPHA"
--	wireName _	= "ABC"
	wireType _	= U 0

instance RepWire ALPHA where
	type WIDTH ALPHA	= X0
	toWireRep m  		= return $ ALPHA ""
	fromWireRep v 		= matrix []
	showRepWire _ = show

-----------------------------------------------------------------------------


