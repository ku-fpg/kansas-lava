{-# LANGUAGE FlexibleContexts #-}
import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Control.Applicative

-- import Test.QuickCheck
-- Simple examples

halfAdder :: (K Bool,K Bool) -> (K Bool,K Bool)
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

fullAdder :: K Bool -> (K Bool, K Bool) -> (K Bool, K Bool)
fullAdder c (a,b) = (s2,c2 `xor2` c1)
  where (s1,c1) = halfAdder (a,b)
	(s2,c2) = halfAdder (s1,c)

wordAdder :: (RepWire (Unsigned x)
	     ,Size (WIDTH (Unsigned x))
	     ,Integral (WIDTH (Unsigned x))	
	     )
	   => K Bool ->  (K (Unsigned x), K (Unsigned x)) -> (K (Unsigned x), K Bool)
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


main = do
	putStrLn "Testing halfAdder function"
	putStrLn $ unlines [ show (a,b,halfAdder (a,b))
	 			| a <- [true,false]
			        , b <- [true,false] ]
	putStrLn "Testing halfAdder reify"
	debugCircuit [] halfAdder

	putStrLn "Testing fullAdder function"
	putStrLn $ unlines [ show (a,b,c,fullAdder c (a,b))
	 			| a <- [true,false]
	 			, b <- [true,false]
			        , c <- [true,false] ]
	putStrLn "Testing fullAdder reify"
	debugCircuit [] fullAdder

	putStrLn "Testing wordAdder function"
	putStrLn $ unlines [ show (a,b,c,wordAdder a (pureS b,pureS c))
	 			| a <- [true,false]
	 			, b <- [0..3] :: [(U.Unsigned X2)]
			        , c <- [0..3] ]
	putStrLn "Testing wordAdder reify"
	debugCircuit [] (wordAdder :: K Bool ->  (K (Unsigned X2), K (Unsigned X2)) -> (K (Unsigned X2), K Bool))

	--	test_shallow_bit
