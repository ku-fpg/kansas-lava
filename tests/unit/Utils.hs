{-# LANGUAGE RankNTypes #-}
module Utils where
	
import Language.KansasLava
import Language.KansasLava.Testing.Thunk
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Default
import Data.List ( sortBy )
import Data.Ord ( comparing )

import qualified System.Random as R
-- Our Unit Tests

-------------------------------------------------------------------------------------

data Options = Options
	{ shallowOpt :: Bool
	, verboseOpt :: Int
	, testOnly   :: Maybe [String]
	}

instance Default Options where
	def = Options
		{ shallowOpt = True
		, verboseOpt = 3
		, testOnly = Nothing
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
testMe nm (Just nms) = nm `elem` nms

verbose opt n m | verboseOpt opt >= n = putStrLn m
		| otherwise	      = return ()

-------------------------------------------------------------------------------------

-- Given a circuit that returns an a, and the expected results,
-- do some tests for sanity.

data TestSeq = TestSeq (forall a . (Rep a, Show a, Eq a) => String -> Int -> Thunk (Seq a) -> Seq a -> IO ())

testSeq :: (Rep a, Show a, Eq a) => Options -> String -> Int -> Thunk (Seq a) -> Seq a -> IO ()
testSeq opts nm count th master | testMe nm (testOnly opts) = do
	let verb n m = verbose opts n (nm ++ " :" ++ take n (repeat ' ') ++ m)
	verb 2 $ "testing(" ++ show count ++ ")"
	verb 9 $ show ("master",master)

	-- First run the shallow
        let shallow = runShallow th
	verb 9 $ show ("shallow",shallow)
	if cmpSeqRep count master shallow
	  then do verb 3 $ "shallow passed"
	  else do verb 1 $ "shallow FAILED"
		  verb 4 $ show ("master",master)
		  verb 4 $ show ("shallow",shallow)
	return ()

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



	