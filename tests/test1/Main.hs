{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables, ParallelListComp
 #-}
import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Control.Applicative

-- import Test.QuickCheck
-- Simple examples
-- TODO: actually test both the Comb and Seq version.

halfAdder :: (Comb Bool,Comb Bool) -> (Comb Bool,Comb Bool)
halfAdder (a,b) = (sum, carry)
  where sum = a `xor2` b
        carry = a `and2` b

{-
for test two
-- testing shallow for bit operations
prop_shallow_bit v n = True
   where
	_types = (v :: Int, n :: X32)

	print [testABit (be (v :: Int)) n | n <- [0..31], v <- [1..100]]

-}

fullAdder :: Comb Bool -> (Comb Bool, Comb Bool) -> (Comb Bool, Comb Bool)
fullAdder c (a,b) = (s2,c2 `xor2` c1)
  where (s1,c1) = halfAdder (a,b)
	(s2,c2) = halfAdder (s1,c)

wordAdder :: (RepWire (Unsigned x)
	     ,Size (WIDTH (Unsigned x))
	     ,Integral (WIDTH (Unsigned x))	
	     )
	   => Comb Bool ->  (Comb (Unsigned x), Comb (Unsigned x)) -> (Comb (Unsigned x), Comb Bool)
wordAdder c_in (a,b) = (fromBoolMatrix res, c_out)
   where
	m   = M.zipWith (,) (toBoolMatrix a) (toBoolMatrix b)
	(res,c_out) = scanR adder (c_in,m)

	adder (c,(a,b)) = (r_out,c_out)
	  where (r_out, c_out) = fullAdder c (a,b)

-- to be moved into sized-types land
-- Assumes the Matrix is not zero sized.
-- we'll also need folds, etc.


scanM :: (Size ix, Bounded ix, Enum ix)
      => ((left,a,right) -> (right,b,left))
      -> (left, Matrix ix a,right)
      -> (right,Matrix ix b,left)
scanM f (l,m,r) =  ( fst3 (tmp ! minBound), snd3 `fmap` tmp, trd3 (tmp ! maxBound) )
  where tmp = forEach m $ \ i a -> f (prev i, a, next i)
	prev i = if i == minBound then l else (trd3 (tmp ! (pred i)))
	next i = if i == maxBound then r else (fst3 (tmp ! (succ i)))
	fst3 (a,_,_) = a
	snd3 (_,b,_) = b
	trd3 (_,_,c) = c



test_mux_1 :: (Signal sig, sig ~ Seq, a ~ Int, Wire a) => sig a -> sig a 
test_mux_1 sig = a
	where a = mux2 high (sig ,latch a)


testAllTruth:: (Testable a) => String -> a -> IO ()
testAllTruth nm fn = do
	putStrLn $ "Testing " ++ nm ++ " function"
	putStrLn $ "======="
	putStrLn $ showAllTT $ truthTable fn
	
testSomeTruth:: (Testable a) => Int -> String -> a -> IO ()
testSomeTruth n nm fn = do
	putStrLn $ "Testing " ++ nm ++ " function"
	putStrLn $ "======="
	putStrLn $ showSomeTT n $ truthTable fn	
	
testReify :: (Ports a) => String -> a -> IO ()
testReify nm fn = do
	putStrLn $ "Testing " ++ nm ++ " reify"
	putStrLn $ "======="
	debugCircuit [] fn

main = do
	let tst :: Comb Bool -> Comb Bool -> (Comb Bool,Comb Bool)
	    tst a b = halfAdder (a,b)
	testAllTruth "halfAdder" tst
	testReify "halfAdder" tst	
	
	let tst :: Comb Bool -> Comb U1 -> Comb U1 -> Comb U1
	    tst a b c = mux2 a (b,c)
	testAllTruth "mux2" tst
	testReify "mux2" tst		

	let tst :: Comb Bool -> Comb Bool -> Comb Bool -> (Comb Bool,Comb Bool)
	    tst a b c = fullAdder a (b,c)
	testAllTruth "fullAdder" tst
	testReify "fullAdder" tst	
	
	let tst :: Comb Bool -> Comb U2 -> Comb U2 -> (Comb U2,Comb Bool)
	    tst a b c = wordAdder a (b,c)
	testAllTruth "wordAdder" tst
	testReify "wordAdder" tst		


	let tst ::Seq SysEnv -> Comb U4 -> Seq U4 -> Seq U4
	    tst = delay
	
	testSomeTruth 50 "delay" $
		let env = takeThenSeq 7 sysEnv env
		    def = 1
		    inp = toSeq $ cycle [0..3]
		 in example tst .*. env .*. def .*. inp
	testReify "delay" tst		


