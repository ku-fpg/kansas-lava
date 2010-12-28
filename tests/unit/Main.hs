{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Main where

import Language.KansasLava
import Language.KansasLava.Testing.Thunk
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Sampled
import qualified Data.Sized.Matrix as M
import Data.Default
import Data.List ( sortBy, sort )
import Data.Ord ( comparing )
import Data.Maybe as Maybe
import Control.Applicative
import Data.Bits
import Control.Concurrent.MVar
import Trace.Hpc.Reflect
import Trace.Hpc.Tix
import Utils



main = do
	let opt = def { verboseOpt = 4	-- 4 == show cases that failed
--	 	      , testOnly = return [ "memory/X2xU4" ]
	 	      }

	count <- newMVar (0,0) :: IO (MVar (Int,Int))

	let pass p = do
		(good,bad) <- takeMVar count
		let (good',bad') = case p of
				 True  -> (good + 1,bad)
				 False -> (good,bad + 1)
		putMVar count (good',bad')


	let test :: TestSeq
	    test = TestSeq (testSeq opt pass)
			(case testData opt of
			   Nothing -> genToList
			   Just i -> take i . genToRandom)

	tests test

	(good,bad) <- takeMVar count
	putStrLn $ "Tests passed: " ++ show good
	putStrLn $ "Tests failed: " ++ show bad

	Tix tix <- examineTix
	let counts = concat [ xs | TixModule _ _ _ xs <- tix ]
	let len = length counts
	let txs = length $ filter (> 0) counts
	putStrLn $ "Raw coverage: " ++ show (floor (100 * fromIntegral txs / fromIntegral len)) ++ "%"

tests test = do
	-- Just the Eq Stuff
	let t str arb = testOpsEq test str arb

	t "StdLogicVector/1" (arbitrary :: Gen (StdLogicVector X1))
	t "StdLogicVector/2" (arbitrary :: Gen (StdLogicVector X2))
	t "StdLogicVector/3" (arbitrary :: Gen (StdLogicVector X3))
	t "StdLogicVector/4" (arbitrary :: Gen (StdLogicVector X4))
	t "StdLogicVector/8" (arbitrary :: Gen (StdLogicVector X8))
	t "StdLogicVector/32" (arbitrary :: Gen (StdLogicVector X32))

	-- Just the Ord Stuff
	let t str arb = testOpsOrd test str arb

	t "Bool" (arbitrary :: Gen Bool)

	-- Just the Num Stuff
	let t str arb = testOpsNum test str arb

	t "Sampled/8x8" (arbitrary :: Gen (Sampled X8 X8))
	t "Sampled/4x2" (arbitrary :: Gen (Sampled X4 X2))
	t "Sampled/2x2" (arbitrary :: Gen (Sampled X2 X2))
	t "Sampled/2x1" (arbitrary :: Gen (Sampled X2 X1))
	t "Sampled/1x2" (arbitrary :: Gen (Sampled X1 X2))
	t "Sampled/1x4" (arbitrary :: Gen (Sampled X1 X4))
	t "Sampled/8x10"(arbitrary :: Gen (Sampled X8 X10))

	-- Just the Bits Stuff
	let t str arb = testOpsBits test str arb

	t "U1" (arbitrary :: Gen U1)
	t "U2" (arbitrary :: Gen U2)
	t "U3" (arbitrary :: Gen U3)
	t "U4" (arbitrary :: Gen U4)
	t "U5" (arbitrary :: Gen U5)
	t "U6" (arbitrary :: Gen U6)
	t "U7" (arbitrary :: Gen U7)
	t "U8" (arbitrary :: Gen U8)
	t "U32" (arbitrary :: Gen U32)
	t "U64" (arbitrary :: Gen (Unsigned X64))

	-- no S1
	t "S2" (arbitrary :: Gen S2)
	t "S3" (arbitrary :: Gen S3)
	t "S4" (arbitrary :: Gen S4)
	t "S5" (arbitrary :: Gen S5)
	t "S6" (arbitrary :: Gen S6)
	t "S7" (arbitrary :: Gen S7)
	t "S8" (arbitrary :: Gen S8)
	t "S32" (arbitrary :: Gen S32)
	t "S64" (arbitrary :: Gen (Signed X64))
	-- Just the Eq Stuff

	-- None

	--  Now registers
	let t str arb = testRegister test str arb

	t "U1" (loop 10 (arbitrary :: Gen U1))
	t "U2" (loop 10 (arbitrary :: Gen U2))
	t "U3" (loop 10 (arbitrary :: Gen U3))
	t "Int" (loop 10 (arbitrary :: Gen Int))
	t "Bool" (loop 10 (arbitrary :: Gen Bool))

	--  Memories
	let t str arb = testConstMemory test str arb

	t "X1xBool" (loop 10 $ dubSeq (arbitrary :: Gen (Maybe (X1,Bool))))
	t "X1xU4" (dubSeq (arbitrary :: Gen (Maybe (X1,U4))))
	t "X2xU4" (dubSeq (arbitrary :: Gen (Maybe (X2,U4))))
	t "X4xU4" (dubSeq (arbitrary :: Gen (Maybe (X4,U4))))
	t "X16xS10" (dubSeq (arbitrary :: Gen (Maybe (X256,S10))))

	let t str arb = testMemory test str arb
	t "X1xBool" (loop 10 $ dubSeq (arbitrary :: Gen (Maybe (X1,Bool),X1)))
	t "X2xU4" (dubSeq (arbitrary :: Gen (Maybe (X2,U4),X2)))
	t "X16xS10" (dubSeq (arbitrary :: Gen (Maybe (X16,S10),X16)))

main_testLabel :: IO ()
main_testLabel = do
	let g :: Seq U4 -> Seq U4 -> Seq U4
	    g a b = output "out" ((+) (input "a" a) (input "b" b))

	c <- reifyCircuit g
	print c
	let sig = circuitSignature c
	if "[(a$0,4U),(b$1,4U)]" == show (sort (sigInputs sig))
          then print "label inputs passed"
          else do print ("labels failed: ",show sig,show (sort (sigInputs sig)))
		  print c
	print ()

allValues :: forall w . (Rep w) => [w]
allValues = xs
    where
	xs :: [w]
	xs = Maybe.catMaybes
	   $ fmap (unX :: X w -> Maybe w)
	   $ fmap (fromRep :: RepValue -> X w)
	   $ (allReps (Witness :: Witness w))

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

testBinOp :: (Rep a, Show a, Eq c, Rep b, Show b, Eq b, Rep c, Show c) => TestSeq -> String -> (a -> b -> c) -> (Comb a -> Comb b -> Comb c) -> Gen (a,b) -> IO ()
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

testTriOp :: (Rep a, Show a, Eq c, Rep b, Show b, Eq b, Rep c, Show c, Rep d, Show d, Eq d) => TestSeq -> String -> (a -> b -> c -> d) -> (Comb a -> Comb b -> Comb c -> Comb d) -> Gen (a,b,c) -> IO ()
testTriOp (TestSeq test toList) nm op lavaOp gen = do
	let (us0,us1,us2) = unzip3 $ toList gen
	let thu = Thunk (liftS3 lavaOp)
		        (\ f -> f (toSeq us0) (toSeq us1) (toSeq us2)
			)
 	    res = toSeq (Prelude.zipWith3 op
				  us0
				  us1
				  us2
			)
	test nm (length (zip us0 us1)) thu res

------------------------------------------------------------------------------------------------

testOpsEq :: (Rep w, Eq w, Show w) => TestSeq -> String -> Gen w -> IO ()
testOpsEq test tyName ws = do
	let ws2 = pair ws

	sequence_
	  [ testTriOp test (name ++ "/" ++ tyName) op lavaOp
			(pure (\ c (a,b) -> (c,a,b))
					  <*> arbitrary
					  <*> ws2)
          | (name,op,lavaOp) <-
		[ ("mux",\ c a b -> if c then a else b,\ c a b -> mux2 c (a,b))
		]
	  ]

	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <-
		[ ("==",(==),(.==.))
--		, ("/=",(/=),(./=.))
		]
	  ]


------------------------------------------------------------------------------------------------


testOpsOrd :: (Rep w, Ord w, Show w) => TestSeq -> String -> Gen w -> IO ()
testOpsOrd test tyName ws = do
	let ws2 = pair ws

	testOpsEq test tyName ws

	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <-
		[ (">",(>),(.>.))
		, ("<",(<),(.<.))
		, (">=",(>=),(.>=.))
		, ("<=",(<=),(.<=.))
		]
	  ]


------------------------------------------------------------------------------------------------


testOpsNum :: forall w .
	(Ord w, Rep w, Num w) => TestSeq -> String -> Gen w -> IO ()
testOpsNum test tyName ws = do
	testOpsOrd test tyName ws

	let ws2 = pair ws

	sequence_
	  [ testUniOp test (name ++ "/" ++ tyName) op lavaOp ws
          | (name,op,lavaOp) <-
		[ ("negate",negate,negate)
		, ("abs",abs,abs)
		, ("signum",signum,signum)
		]
	  ]

	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <-
		[ ("add",(+),(+))
		, ("sub",(-),(-))
		, ("mul",(*),(*))
		, ("max",max,max)
		, ("min",min,min)
		]
	  ]

----------------------------------------------------------------------------------------

testOpsBits :: forall w .
	(Ord w, Rep w, Bits w) => TestSeq -> String -> Gen w -> IO ()
testOpsBits test tyName ws = do
	testOpsNum test tyName ws

	let ws2 = pair ws

	sequence_
	  [ testUniOp test (name ++ "/" ++ tyName) op lavaOp ws
          | (name,op,lavaOp) <-
		[ ("complement",complement,complement)
		]
	  ]

	sequence_
	  [ testBinOp test (name ++ "/" ++ tyName) op lavaOp ws2
          | (name,op,lavaOp) <-
		[ (".&.",(.&.),(.&.))
		, (".|.",(.|.),(.|.))
		, ("xor",(xor),(xor))
		]
	  ]

pair :: (Applicative f) => f a -> f (a, a)
pair ws = pure (,) <*> ws <*> ws

triple :: (Applicative f) => f a -> f (a, a, a)
triple ws = pure (,,) <*> ws <*> ws <*> ws

--------------------------------------------------------------------------------------
-- Testing register and memory
testRegister :: forall w . (Show w, Eq w, Rep w) => TestSeq -> String -> Gen w -> IO ()
testRegister  (TestSeq test toList) tyName ws = do
	let (u0:us0) = toList ws
	let reg = register ::  Comb w -> Seq w -> Seq w
	let thu = Thunk reg
		        (\ f -> f (toComb u0) (toSeq us0)
			)
 	    res = toSeq	(u0 : us0)
	test ("register/" ++ tyName) (length us0) thu res
	return ()

testMemory :: forall w1 w2 . (Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (Maybe (w1,w2),w1) -> IO ()
testMemory (TestSeq test toList) tyName ws = do
	let (writes,reads) = unzip $ toList ws
	let mem = readMemory . writeMemory :: Seq (Maybe (w1,w2)) -> Seq w1 -> Seq w2
	let thu = Thunk mem
		        (\ f -> f (toSeq writes) (toSeq reads)
		        )
	    res :: Seq w2
	    res = toSeq' $
		    [ last $
		     [Nothing] ++
		     [ Just b
		     | Just (a,b) <- take (i - 1) writes
		     , a == fromIntegral r
		     ]
		    | (i,r) <- zip [1..(length writes-1)] reads

		    ]
	test ("memory/" ++ tyName) (length writes) thu res

testConstMemory :: forall w1 w2 . (Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (Maybe (w1,w2)) -> IO ()
testConstMemory (TestSeq test toList) tyName ws = do
	let writes = toList ws
	let mem = memoryToMatrix . writeMemory :: Seq (Maybe (w1,w2)) -> Seq (M.Matrix w1 w2)
	let thu = Thunk mem
		        (\ f -> f (toSeq writes)
		        )
	    res :: M.Matrix w1 (Seq w2)
	    res = M.matrix
		$ [ toSeq' $ 
		    [ last $
		     [Nothing] ++
		     [ Just b
		     | Just (a,b) <- take (i - 1) writes
		     , a == fromIntegral x
		     ]
		    | i <- [1..(length writes-1)]
		    ]
		  | x <- [0..(size (error "witness" :: w1) - 1 )]
		  ]
	test ("memory/const/" ++ tyName) (length writes) thu (pack res)
	return ()

{-
-- Testing FIFOs
--testFIFOs1 :: forall w1 w2 . (Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (Maybe (w1,w2)) -> IO ()
testFIFOs1 (TestSeq test toList) tyName ws = do
	let writes = toList ws
	let f = fifo (witness :: X32) ::  Seq Bool -> HandShake (Seq (Enabled U4)) -> HandShake (Seq (Enabled U4))
	let thu = Thunk f
		$ \ cir -> cir undefined undefined undefined
--	putFIFOContents

	return ()
-}