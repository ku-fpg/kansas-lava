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

import Utils

main = do
	let test :: TestSeq
	    test = TestSeq (testSeq def)

	testMux test

	testBounded test "U1" (witness :: U1)
	testBounded test "U2" (witness :: U2)
	testBounded test "U3" (witness :: U3)
	testBounded test "U4" (witness :: U4)
	testBounded test "U5" (witness :: U5)
	testBounded test "U6" (witness :: U6)
	testBounded test "U7" (witness :: U7)
	testBounded test "U8" (witness :: U8)

	-- no S1
	testBounded test "S2" (witness :: S2)
	testBounded test "S3" (witness :: S3)
	testBounded test "S4" (witness :: S4)
	testBounded test "S5" (witness :: S5)
	testBounded test "S6" (witness :: S6)
	testBounded test "S7" (witness :: S7)
	testBounded test "S8" (witness :: S8)

	testValues test "Sampled/8x8" ([0..8] :: [Sampled X8 X8])

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

testBinOp :: (Rep a, Show a, Eq a) => TestSeq -> Int -> String -> (a -> a -> a) -> (Comb a -> Comb a -> Comb a) -> [a] -> [a] -> IO ()	
testBinOp (TestSeq test) count nm op lavaOp us0 us1 = do
	let thu = Thunk (liftS2 lavaOp)
		        (\ f -> f (toSeq us0) (toSeq us1)
			)
 	    res = toSeq (Prelude.zipWith op
				  us0
				  us1
			)
	test nm count thu res

testBinOpNum :: (Num a, Rep a) => TestSeq -> Int -> String -> [a] -> [a] -> IO ()
testBinOpNum test count tyName s0 s1 = 
	sequence_
	  [ testBinOp test count (name ++ "/" ++ tyName)  (+) (+) s0 s1
          | (name,op,lavaOp) <- 
		[ ("add",(+),(+))
		, ("sub",(+),(+))
		, ("mul",(+),(+))
		]
	  ]

testValues :: forall w . (Rep w, Num w) => TestSeq -> String -> [w] -> IO ()
testValues test tyName ws = do
	let s1 :: [w]
	    s2 :: [w]
	    (s1,s2) = unzip
		    [ (s1,s2) 
		    | s1 <- ws
		    , s2 <- ws
		    ]
	testBinOpNum test (length ws * length ws) tyName s1 s2

testBounded :: forall w . (Rep w, Num w, Bounded w, Enum w) => TestSeq -> String -> w -> IO ()
testBounded test tyName w = testValues test tyName ([minBound..maxBound] :: [w])



