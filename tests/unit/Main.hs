module Main where
	
import Language.KansasLava
import Language.KansasLava.Testing.Thunk
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Default
import Data.List ( sortBy )
import Data.Ord ( comparing )

import qualified System.Random as R
-- Our Unit Tests

data Options = Options
	{ shallowOpt :: Bool
	, verboseOpt :: Int
	, testOnly   :: Maybe [String]
	}

instance Default Options where
	def = Options
		{ shallowOpt = True
		, verboseOpt = 9
		, testOnly = Nothing
		}

type Report = ()

testMe _ Nothing     = True
testMe nm (Just nms) = nm `elem` nms

verbose opt n m | verboseOpt opt >= n = putStrLn m
		| otherwise	      = return ()

-- Given a circuit that returns an a, and the expected results,
-- do some tests for sanity.
testSeq :: (Rep a, Show a, Eq a) => Options -> String -> Int -> Thunk (Seq a) -> Seq a -> IO ()
testSeq opts nm count th golden | testMe nm (testOnly opts) = do
	let verb n m = verbose opts n (nm ++ " :" ++ take n (repeat ' ') ++ m)
	verb 2 $ "testing(" ++ show count ++ ")"

	-- First run the shallow
        let shallow = runShallow th
	if cmpSeqRep count golden shallow
	  then do verb 3 $ "shallow passed"
	  else do verb 1 $ "shallow FAILED"
		  print ("Golden",golden)
		  print ("Shallow",shallow)
	return ()
	
u4s0 :: [U4]
u4s0 = cycle [0..15]

u4s1 :: [U4]
u4s1= cycle $ unsort [0..15] ++ reverse [0..15] -- have more randomness here


-- Not really random, but good enough for basic testing.
unsort :: [x] -> [x]
unsort es = map snd . sortBy (comparing fst) $ zip rs es
  where rs = R.randoms stdGen :: [Integer]
	stdGen = R.mkStdGen 0

main = do
	-- insert parsing of command line arguments
	let test = testSeq def

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
	
	