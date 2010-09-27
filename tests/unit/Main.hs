{-# LANGUAGE RankNTypes #-}
module Main where
	
import Language.KansasLava
import Language.KansasLava.Testing.Thunk
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Default
import Data.List ( sortBy )
import Data.Ord ( comparing )

import Utils

main = do
	let test :: TestSeq
	    test = TestSeq (testSeq def)

	testMux test

	testBinOpNum test "U4" u4s0 u4s1

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

testBinOp :: (Rep a, Show a, Eq a) => TestSeq -> String -> (a -> a -> a) -> (Comb a -> Comb a -> Comb a) -> [a] -> [a] -> IO ()	
testBinOp (TestSeq test) nm op lavaOp us0 us1 = do
	let thu = Thunk (liftS2 lavaOp)
		        (\ f -> f (toSeq us0) (toSeq us1)
			)
 	    res = toSeq (Prelude.zipWith op
				  us0
				  us1
			)
	test nm 100 thu res

testBinOpNum :: (Num a, Rep a) => TestSeq -> String -> [a] -> [a] -> IO ()
testBinOpNum test tyName s0 s1 = 
	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  (+) (+) s0 s1
          | (name,op,lavaOp) <- 
		[ ("add",(+),(+))
		, ("sub",(+),(+))
		, ("mul",(+),(+))
		]
	  ]
