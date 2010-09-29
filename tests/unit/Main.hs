{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
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

import Utils

main = do
	let test :: TestSeq
	    test = TestSeq (testSeq def)

	testMux test

	testOps test "U1" (allBounded :: [U1])
	testOps test "U2" (allBounded :: [U2])
	testOps test "U3" (allBounded :: [U3])
	testOps test "U4" (allBounded :: [U4])
	testOps test "U5" (allBounded :: [U5])
	testOps test "U6" (allBounded :: [U6])
	testOps test "U7" (allBounded :: [U7])
	testOps test "U8" (allBounded :: [U8])

	-- no S1
	testOps test "S2" (allBounded :: [S2])
	testOps test "S3" (allBounded :: [S3])
	testOps test "S4" (allBounded :: [S4])
	testOps test "S5" (allBounded :: [S5])
	testOps test "S6" (allBounded :: [S6])
	testOps test "S7" (allBounded :: [S7])
	testOps test "S8" (allBounded :: [S8])


	testOps test "Sampled/8x8" (allValues :: [Sampled X8 X8])

	testOps test "Sampled/4x2" (allValues :: [Sampled X4 X2])		
	testOps test "Sampled/2x2" (allValues :: [Sampled X2 X2])	
	testOps test "Sampled/2x1" (allValues :: [Sampled X2 X1])		
	testOps test "Sampled/1x2" (allValues :: [Sampled X1 X2])		
	testOps test "Sampled/1x4" (allValues :: [Sampled X1 X4])		
	testOps test "Sampled/8x10"(allValues :: [Sampled X8 X10])
	

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
testMux :: TestSeq -> IO ()	
testMux (TestSeq test) = do
	let gate = cycle [True,False,True,True,False]
	    thu = Thunk (mux2 :: Seq Bool -> (Seq U4, Seq U4) -> Seq U4) 
		        (\ f -> f (toSeq (cycle gate)) 
				  (toSeq u4s0, toSeq u4s1)
			)
 	    res = toSeq (Prelude.zipWith3 (\ c x y -> if c then x else y)
				  gate
				  u4s0
				  u4s1
			)
	test "mux2/U4" 100 thu res

-------------------------------------------------------------------------------------------------
-- This only tests at the *value* level, and ignores testing unknowns.

testUniOp :: (Rep a, Show a, Eq a, Rep b, Show b, Eq b) => TestSeq -> String -> (a -> b) -> (Comb a -> Comb b) -> [a] -> IO ()	
testUniOp (TestSeq test) nm op lavaOp us0 = do
	let thu = Thunk (liftS1 lavaOp)
		        (\ f -> f (toSeq us0) 
			)
 	    res = toSeq (fmap op
			      us0
			)
	test nm (min 100 $ length us0) thu res

testBinOp :: (Rep a, Show a, Eq a, Rep b, Show b, Eq b) => TestSeq -> String -> (a -> a -> b) -> (Comb a -> Comb a -> Comb b) -> [a] -> [a] -> IO ()	
testBinOp (TestSeq test) nm op lavaOp us0 us1 = do
	let thu = Thunk (liftS2 lavaOp)
		        (\ f -> f (toSeq us0) (toSeq us1)
			)
 	    res = toSeq (Prelude.zipWith op
				  us0
				  us1
			)
	test nm (min 1000 $ length (zip us0 us1)) thu res

testUniOpNum :: (Num a, Rep a) => TestSeq -> String -> [a] -> IO ()
testUniOpNum test tyName s0 = 
	sequence_
	  [ testUniOp test (name ++ "/" ++ tyName) op lavaOp s0
          | (name,op,lavaOp) <- 
		[ ("negate",negate,negate)
		]
	  ]

testBinOpNum :: (Ord a,Num a, Rep a) => TestSeq -> String -> [a] -> [a] -> IO ()
testBinOpNum test tyName s0 s1 = do
	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp s0 s1
          | (name,op,lavaOp) <- 
		[ ("add",(+),(+))
		, ("sub",(+),(+))
		, ("mul",(+),(+))
		, ("max",max,max)
		, ("min",min,min)
		]
	  ]
	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp s0 s1
          | (name,op,lavaOp) <- 
		[ ("==",(==),(.==.))
--		, ("/=",(/=),(./=.))
		, (">",(>),(.>.))
		, ("<",(<),(.<.))
		, (">=",(>=),(.>=.))
		, ("<=",(<=),(.<=.))
		]
	  ]
twice :: ([w] -> [w] -> a) -> [w] -> a
twice f ws = f s1 s2
  where
	(s1,s2) = unzip $ unsort
		    [ (s1,s2) 
		    | s1 <- ws
		    , s2 <- ws
		    ]

testOps :: forall w . (Ord w, Rep w, Num w) => TestSeq -> String -> [w] -> IO ()
testOps test tyName ws = do
	let ws' = unsort ws
	testUniOpNum test tyName ws'
	testBinOpNum test tyName `twice` ws'
