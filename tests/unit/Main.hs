{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts #-}
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

import Utils

main = do
	let opt = def

	let test :: TestSeq
--	    test = TestSeq (testSeq def) 
	    test = TestSeq (testSeq opt) (case testData opt of
					   Nothing -> genToList
					   Just i -> take i . genToRandom)


	testOps test "U1" (arbitrary :: Gen U1)
	testOps test "U2" (arbitrary :: Gen U2)
	testOps test "U3" (arbitrary :: Gen U3)
	testOps test "U4" (arbitrary :: Gen U4)
	testOps test "U5" (arbitrary :: Gen U5)
	testOps test "U6" (arbitrary :: Gen U6)
	testOps test "U7" (arbitrary :: Gen U7)
	testOps test "U8" (arbitrary :: Gen U8)
	testOps test "U32" (arbitrary :: Gen U32)
	testOps test "U64" (arbitrary :: Gen (Unsigned X64))

	-- no S1
	testOps test "S2" (arbitrary :: Gen S2)
	testOps test "S3" (arbitrary :: Gen S3)
	testOps test "S4" (arbitrary :: Gen S4)
	testOps test "S5" (arbitrary :: Gen S5)
	testOps test "S6" (arbitrary :: Gen S6)
	testOps test "S7" (arbitrary :: Gen S7)
	testOps test "S8" (arbitrary :: Gen S8)
	testOps test "S32" (arbitrary :: Gen S32)
	testOps test "S64" (arbitrary :: Gen (Signed X64))
	
	testOps test "Sampled/8x8" (arbitrary :: Gen (Sampled X8 X8))
	testOps test "Sampled/4x2" (arbitrary :: Gen (Sampled X4 X2))		
	testOps test "Sampled/2x2" (arbitrary :: Gen (Sampled X2 X2))	
	testOps test "Sampled/2x1" (arbitrary :: Gen (Sampled X2 X1))		
	testOps test "Sampled/1x2" (arbitrary :: Gen (Sampled X1 X2))		
	testOps test "Sampled/1x4" (arbitrary :: Gen (Sampled X1 X4))		
	testOps test "Sampled/8x10"(arbitrary :: Gen (Sampled X8 X10))


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

testBinOp :: (Rep a, Show a, Eq a, Rep b, Show b, Eq b) => TestSeq -> String -> (a -> a -> b) -> (Comb a -> Comb a -> Comb b) -> Gen (a,a) -> IO ()	
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

testUniOpNum :: (Num a, Rep a) => TestSeq -> String -> Gen a -> IO ()
testUniOpNum test tyName s0 = 
	sequence_
	  [ testUniOp test (name ++ "/" ++ tyName) op lavaOp s0
          | (name,op,lavaOp) <- 
		[ ("negate",negate,negate)
		, ("abs",abs,abs)
		, ("signum",signum,signum)
		]
	  ]

testBinOpNum :: 
	(Size (ADD (WIDTH a) (WIDTH a)),
         Enum (ADD (WIDTH a) (WIDTH a)),
	Ord a,Num a, Rep a) => TestSeq -> String -> Gen (a,a) -> IO ()
testBinOpNum test tyName gen = do

	testMux test ("mux/" ++ tyName) (pure (\ c (a,b) -> (c,a,b)) 
					  <*> arbitrary
					  <*> gen)

	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp gen
          | (name,op,lavaOp) <- 
		[ ("add",(+),(+))
		, ("sub",(-),(-))
		, ("mul",(*),(*))
		, ("max",max,max)
		, ("min",min,min)
		]
	  ]
	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp gen
          | (name,op,lavaOp) <- 
		[ ("==",(==),(.==.))
--		, ("/=",(/=),(./=.))
		, (">",(>),(.>.))
		, ("<",(<),(.<.))
		, (">=",(>=),(.>=.))
		, ("<=",(<=),(.<=.))
		]
	  ]

testOps :: forall w . 
	(Size (ADD (WIDTH w) (WIDTH w)),
         Enum (ADD (WIDTH w) (WIDTH w)), 
	 Ord w, Rep w, Num w) => TestSeq -> String -> Gen w -> IO ()
testOps test tyName ws = do
	testUniOpNum test tyName ws
	testBinOpNum test tyName (pure (,) <*> ws <*> ws)
