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
	let test :: (Rep a, Show a, Eq a) => String -> Int -> Thunk (Seq a) -> Seq a -> IO ()
	    test = testSeq def
	testMux test



testMux :: (forall a . (Rep a, Show a, Eq a) => String -> Int -> Thunk (Seq a) -> Seq a -> IO ()) -> IO ()	
testMux test = do
	-- insert parsing of command line arguments

	-- Our basic test template
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
	
	