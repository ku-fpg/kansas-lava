{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Utils where

import Language.KansasLava
import Language.KansasLava.Testing.Thunk
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Default
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Maybe as Maybe
import Data.List
import Control.Applicative

import qualified System.Random as R
-- Our Unit Tests

-------------------------------------------------------------------------------------

data TestData = Rand Int | Complete

data Options = Options
	{ shallowOpt :: Bool
	, verboseOpt :: Int
	, testOnly   :: Maybe [String]
	, testData   :: Maybe Int	--- cut off for random testing
	}

instance Default Options where
	def = Options
		{ shallowOpt = True
		, verboseOpt = 3
		, testOnly = Nothing
		, testData = Just 1000
		}

-------------------------------------------------------------------------------------
-- Verbose table
-- 1: Failures
-- 2: what was run
-- 3: what worked
-- 4: debugging from failures
-- 9: debugging from everything that happened

-------------------------------------------------------------------------------------

testMe _ Nothing     = True
testMe nm (Just nms) = or [ n `isPrefixOf` nm
			  | n <- nms
			  ]

verbose opt n m | verboseOpt opt >= n = putStrLn m
		| otherwise	      = return ()

-------------------------------------------------------------------------------------

-- Given a circuit that returns an a, and the expected results,
-- do some tests for sanity.

data TestSeq = TestSeq
	(forall a . (Rep a, Show a, Eq a) => String -> Int -> Thunk (Seq a) -> Seq a -> IO ())
	(forall a. Gen a -> [a])

testSeq :: (Rep a, Show a, Eq a) => Options -> (Bool -> IO ()) -> String -> Int -> Thunk (Seq a) -> Seq a -> IO ()
testSeq opts pass nm count th master | testMe nm (testOnly opts) = do
	let verb n m = verbose opts n (nm ++ " :" ++ take n (repeat ' ') ++ m)
	verb 2 $ "testing(" ++ show count ++ ")"
	verb 9 $ show ("master",master)

	-- First run the shallow
        let shallow = runShallow th
	verb 9 $ show ("shallow",shallow)
	if cmpSeqRep count master shallow
	  then do verb 3 $ "shallow passed"
		  pass True
	  else do verb 1 $ "shallow FAILED"
		  pass False
		  verb 4 $ show ("master",master)
		  verb 4 $ show ("shallow",shallow)

				| otherwise = return ()



-------------------------------------------------------------------------------------

-- Not really random, but good enough for basic testing.
unsort :: [x] -> [x]
unsort es = map snd . sortBy (comparing fst) $ zip rs es
  where rs = R.randoms stdGen :: [Integer]
	stdGen = R.mkStdGen 0

-------------------------------------------------------------------------------------

u4s0 :: [U4]
u4s0 = cycle [0..15]

u4s1 :: [U4]
u4s1= cycle $ unsort $ take (16 * 16) (cycle [0..15])

data Gen a = Gen Integer (Integer -> Maybe a)

arbitrary :: forall w . (Rep w) => Gen w
arbitrary = Gen sz integer2rep
  where
	sz = 2^fromIntegral (repWidth (witness :: w))
	integer2rep :: Integer -> Maybe w
	integer2rep v = unX
		$ fromRep
		$ RepValue
		$ take (repWidth (witness :: w))
		$ map WireVal
		$ map odd
		$ iterate (`div` 2)
		$ fromIntegral v

loop :: Integer -> Gen w -> Gen w
loop n (Gen sz f) = Gen (sz * n) (\ i -> f $ i `mod` sz)


-- | makes sure all sequences of two specific elements happen.
-- Random messes this up a bit, but its still an approximation.
dubSeq :: Gen w -> Gen w
dubSeq g = ((\ a b c -> if a then b else c) <$> arbitrary)
	<*> g
	<*> g


instance Functor Gen where
	fmap g (Gen n f) = Gen n (\i -> do r <- f i
					   return $ g r)

instance Applicative Gen where
	pure a = Gen 1 (const $ return a)
	(Gen n1 f1) <*> (Gen n2 f2) = Gen (n1 * n2) (\ i -> do r1 <- f1 (i `mod` n1)
							       r2 <- f2 (i `div` n1)
							       return $ r1 r2)

-- get *all* elements from a Gen
genToList :: Gen a -> [a]
genToList (Gen n f) = Maybe.catMaybes $ fmap f [0..(n-1)]

-- get some (random) elements from a Gen
-- If it is small, then just output all the values.
genToRandom :: Gen a -> [a]
genToRandom (Gen n f)
	| n <= 100 = unsort $ genToList (Gen n f)
	| otherwise = take (fromIntegral (min n largeNumber)) $ Maybe.catMaybes $ fmap f $ R.randomRs (0,n) (R.mkStdGen 0)


largeNumber = 10000
