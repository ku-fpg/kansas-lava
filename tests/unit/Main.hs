{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Main where
	
import Language.KansasLava
import Language.KansasLava.Testing.Thunk
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Sampled
import Data.Default
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Maybe as Maybe
import Control.Applicative
import Data.Bits

import Utils

main = do
	let opt = def

	let test :: TestSeq
--	    test = TestSeq (testSeq def) 
	    test = TestSeq (testSeq opt) (case testData opt of
					   Nothing -> genToList
					   Just i -> take i . genToRandom)

	-- Just the Eq Stuff
	let t str arb = testOpsEq test str arb

	-- Just the Ord Stuff
	let t str arb = testOpsOrd test str arb

	t "Bool" (arbitrary :: Gen Bool)

	-- Just the Num Stuff
	let t str arb = testOpsNum test str arb

	t "Sampled/8x8" (arbitrary :: Gen (Sampled X8 X8))
	t "Sampled/4x2" (arbitrary :: Gen (Sampled X4 X2))
	t "Sampled/2x2" (arbitrary :: Gen (Sampled X2 X2))
	t "Sampled/2x1" (arbitrary :: Gen (Sampled X2 X1))
	t "Sampled/1x2" (arbitrary :: Gen (Sampled X1 X2))
	t "Sampled/1x4" (arbitrary :: Gen (Sampled X1 X4))
	t "Sampled/8x10"(arbitrary :: Gen (Sampled X8 X10))

	-- Just the Bits Stuff
	let t str arb = testOpsBits test str arb

	t "U1" (arbitrary :: Gen U1)
	t "U2" (arbitrary :: Gen U2)
	t "U3" (arbitrary :: Gen U3)
	t "U4" (arbitrary :: Gen U4)
	t "U5" (arbitrary :: Gen U5)
	t "U6" (arbitrary :: Gen U6)
	t "U7" (arbitrary :: Gen U7)
	t "U8" (arbitrary :: Gen U8)
	t "U32" (arbitrary :: Gen U32)
	t "U64" (arbitrary :: Gen (Unsigned X64))

	-- no S1
	t "S2" (arbitrary :: Gen S2)
	t "S3" (arbitrary :: Gen S3)
	t "S4" (arbitrary :: Gen S4)
	t "S5" (arbitrary :: Gen S5)
	t "S6" (arbitrary :: Gen S6)
	t "S7" (arbitrary :: Gen S7)
	t "S8" (arbitrary :: Gen S8)
	t "S32" (arbitrary :: Gen S32)
	t "S64" (arbitrary :: Gen (Signed X64))

allValues :: forall w . (Rep w) => [w]
allValues = xs
    where 
	xs :: [w]
	xs = Maybe.catMaybes
	   $ fmap (unX :: X w -> Maybe w) 
	   $ fmap (fromRep (witness :: w) :: RepValue -> X w) 
	   $ (allReps (witness :: w))

allBounded :: (Enum w, Bounded w) => [w]
allBounded = [minBound..maxBound]
-------------------------------------------------------------------------------------------------
testMux :: forall a . 
 	(Size (ADD (WIDTH a) (WIDTH a)),
         Enum (ADD (WIDTH a) (WIDTH a)),
	 Eq a, Show a, Rep a) => TestSeq -> String -> Gen (Bool,a,a) -> IO ()	
testMux (TestSeq test toList) nm gen = do
	let (gate,us0,us1) = unzip3 $ toList gen
	let thu = Thunk (mux2 :: Seq Bool -> (Seq a, Seq a) -> Seq a)
		        (\ f -> f (toSeq gate)
				  (toSeq us0, toSeq us1)
			)
 	    res = toSeq (Prelude.zipWith3 (\ c x y -> if c then x else y)
				  gate
				  us0
				  us1
			)
	test nm (length gate) thu res

-------------------------------------------------------------------------------------------------
-- This only tests at the *value* level, and ignores testing unknowns.

testUniOp :: (Rep a, Show a, Eq a, Rep b, Show b, Eq b) => TestSeq -> String -> (a -> b) -> (Comb a -> Comb b) -> Gen a -> IO ()	
testUniOp (TestSeq test toList) nm op lavaOp gen = do
	let us0 = toList gen
	let thu = Thunk (liftS1 lavaOp)
		        (\ f -> f (toSeq us0) 
			)
 	    res = toSeq (fmap op
			      us0
			)
	test nm (length us0) thu res

testBinOp :: (Rep a, Show a, Eq c, Rep b, Show b, Eq b, Rep c, Show c) => TestSeq -> String -> (a -> b -> c) -> (Comb a -> Comb b -> Comb c) -> Gen (a,b) -> IO ()	
testBinOp (TestSeq test toList) nm op lavaOp gen = do
	let (us0,us1) = unzip $ toList gen
	let thu = Thunk (liftS2 lavaOp)
		        (\ f -> f (toSeq us0) (toSeq us1)
			)
 	    res = toSeq (Prelude.zipWith op
				  us0
				  us1
			)
	test nm (length (zip us0 us1)) thu res

testTriOp :: (Rep a, Show a, Eq c, Rep b, Show b, Eq b, Rep c, Show c, Rep d, Show d, Eq d) => TestSeq -> String -> (a -> b -> c -> d) -> (Comb a -> Comb b -> Comb c -> Comb d) -> Gen (a,b,c) -> IO ()	
testTriOp (TestSeq test toList) nm op lavaOp gen = do
	let (us0,us1,us2) = unzip3 $ toList gen
	let thu = Thunk (liftS3 lavaOp)
		        (\ f -> f (toSeq us0) (toSeq us1) (toSeq us2)
			)
 	    res = toSeq (Prelude.zipWith3 op
				  us0
				  us1
				  us2
			)
	test nm (length (zip us0 us1)) thu res

------------------------------------------------------------------------------------------------

testOpsEq :: (Rep w, Eq w, Show w) => TestSeq -> String -> Gen w -> IO ()
testOpsEq test tyName ws = do
	let ws2 = pair ws

	sequence_
	  [ testTriOp test (name ++ "/" ++ tyName) op lavaOp 
			(pure (\ c (a,b) -> (c,a,b)) 
					  <*> arbitrary
					  <*> ws2)
          | (name,op,lavaOp) <- 
		[ ("mux",\ c a b -> if c then a else b,\ c a b -> mux2 c (a,b))
		]
	  ]

------------------------------------------------------------------------------------------------


testOpsOrd :: (Rep w, Ord w, Show w) => TestSeq -> String -> Gen w -> IO ()
testOpsOrd test tyName ws = do
	testOpsEq test tyName ws

------------------------------------------------------------------------------------------------


testOpsNum :: forall w . 
	(Ord w, Rep w, Num w) => TestSeq -> String -> Gen w -> IO ()
testOpsNum test tyName ws = do
	testOpsOrd test tyName ws

	let ws2 = pair ws

	sequence_
	  [ testUniOp test (name ++ "/" ++ tyName) op lavaOp ws
          | (name,op,lavaOp) <- 
		[ ("negate",negate,negate)
		, ("abs",abs,abs)
		, ("signum",signum,signum)
		]
	  ]

	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <- 
		[ ("add",(+),(+))
		, ("sub",(-),(-))
		, ("mul",(*),(*))
		, ("max",max,max)
		, ("min",min,min)
		]
	  ]
	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <- 
		[ ("==",(==),(.==.))
--		, ("/=",(/=),(./=.))
		, (">",(>),(.>.))
		, ("<",(<),(.<.))
		, (">=",(>=),(.>=.))
		, ("<=",(<=),(.<=.))
		]
	  ]

----------------------------------------------------------------------------------------

testOpsBits :: forall w . 
	(Ord w, Rep w, Bits w) => TestSeq -> String -> Gen w -> IO ()
testOpsBits test tyName ws = do
	testOpsNum test tyName ws

	let ws2 = pair ws

	sequence_
	  [ testUniOp test (name ++ "/" ++ tyName) op lavaOp ws
          | (name,op,lavaOp) <- 
		[ ("complement",complement,complement)
		]
	  ]

	
pair :: (Applicative f) => f a -> f (a, a)
pair ws = pure (,) <*> ws <*> ws

triple :: (Applicative f) => f a -> f (a, a, a)
triple ws = pure (,,) <*> ws <*> ws <*> ws
