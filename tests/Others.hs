{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification, DataKinds, TypeOperators #-}
module Others (tests) where

import Language.KansasLava
import Language.KansasLava.Signal(mkShallowXS)
import Test

import qualified Language.KansasLava.Stream as S

import Data.Bits
import Data.Sized.Fin
import Data.Sized.Sampled
import Data.Sized.Signed
import Data.Sized.Unsigned

import GHC.TypeLits

type List a = [a]

tests :: TestSeq -> IO ()
tests test = do
        -- Just the Num Stuff
        let t1 :: (Fractional a, Ord a, Show a, Rep a, SingI (W a)) => String -> [a] -> IO ()
            t1 str arb = testOpsFractional test str arb
        -- With Sampled, use
        --  * powers of two scale, larger than 1
        --  * make sure there are enough bits to represent
        --     both the fractional and non-fractional part.

        t1 "Sampled/X8xX8" (allCases :: [Sampled 8 8])
        t1 "Sampled/X1xX4" (allCases :: [Sampled 1 4])
        t1 "Sampled/X8xX10"(finiteCases 100 :: [Sampled 8 10])
        t1 "Sampled/X128xX16"(finiteCases 100 ::[Sampled 128 16])

        -- Just the Bits Stuff
        let t2 :: (Ord a, Bits a, Num a, Show a, Rep a, SingI (W a)) => String -> List a -> IO ()
            t2 str arb = testOpsBits test str arb


	-- tests Bits, inc the shifts
--        let t2' :: (Ord a, Bits a, Num a, Show a, Rep a, SingI (W a), Integral (W a), Rep (W a), SingI (W (W a))) => String -> List a -> IO ()
        let t2' :: (Ord a, Bits a, Num a, Show a, Rep a, SingI (W a)) => String -> List a -> IO ()
            t2' str arb = testOpsBits2 test str arb

        t2' "U1" (allCases :: List U1)
        t2' "U2" (allCases :: List U2)
        t2' "U3" (allCases :: List U3)
        t2' "U4" (allCases :: List U4)
        t2 "U5" (allCases :: List U5)
        t2 "U6" (allCases :: List U6)
        t2 "U7" (allCases :: List U7)
        t2 "U8" (allCases :: List U8)
        t2 "U32" (finiteCases 100 :: List U32)
{- ghci keeps getting killed during these, Out Of Memory maybe?
        t2 "U64" (finiteCases 100 :: List (Unsigned 64))
-}

        -- no S1
        t2' "S2" (allCases :: List S2)
        t2' "S3" (allCases :: List S3)
        t2' "S4" (allCases :: List S4)
        t2 "S5" (allCases :: List S5)
        t2 "S6" (allCases :: List S6)
        t2 "S7" (allCases :: List S7)
        t2 "S8" (allCases :: List S8)
        t2 "S32" (finiteCases 100 :: List S32)
{- ghci keeps getting killed during these, Out Of Memory maybe?
        t2 "S64" (finiteCases 100 :: List (Signed 64))
-}
        -- Just the Eq Stuff

        -- None

        --  Now registers
        let t3 :: (Eq a, Show a, Rep a, SingI (W a)) => String -> List a -> IO ()
            t3 str arb = testRegister test str arb

        t3 "U1" ( (finiteCases 1000 :: List U1))
        t3 "U2" ( (finiteCases 1000 :: List U2))
        t3 "U3" ( (finiteCases 1000 :: List U3))
        t3 "Int" ( (finiteCases 1000 :: List Int))
        t3 "Bool" ( (finiteCases 1000 :: List Bool))

        let t4 :: (Eq a, Show a, Rep a, SingI (W a)) => String -> List a -> IO ()
            t4 str arb = testDelay test str arb

        t4 "U1" ( (finiteCases 1000 :: List U1))
        t4 "U2" ( (finiteCases 1000 :: List U2))
        t4 "U3" ( (finiteCases 1000 :: List U3))
        t4 "Int" ( (finiteCases 1000 :: List Int))
        t4 "Bool" ( (finiteCases 1000 :: List Bool))


{- We are not ready for this yet
	-- Test the flux capacitor
        let t5 :: (Eq a, Show a, Rep a, Size (W a), Size (ADD (W a) X1))
	       => String -> List (Maybe a) -> (forall c. (Clock c) => Signal c a -> Signal c a) -> IO ()
            t5 str arb op = testFluxCapacitor test str arb op

        t5 "U5/add" ( (take 1000 inifiniteCases :: List (Maybe U5)))
	   	$ \ x -> x + 1

        t5 "U5/delay" ( (take 1000 inifiniteCases :: List (Maybe U5)))
	   	$ delay

        t5 "U5/register" ( (take 1000 inifiniteCases :: List (Maybe U5)))
	   	$ register 0

        t5 "U5/accum" ( (take 1000 inifiniteCases :: List (Maybe U5)))
	   	$ \ x -> let r = register 0 (x + r) in r

-}



data TestMux a = TestMux String (Bool -> a -> a -> a) (forall clk . Signal clk Bool -> Signal clk a -> Signal clk a -> Signal clk a)
data TestCmp a = TestCmp String (a -> a -> Bool) (forall clk . Signal clk a -> Signal clk a -> Signal clk Bool)
--data TestPred a = TestPred String (a -> Bool) (forall clk . Signal clk a -> Signal clk Bool)
data TestUni a = TestUni String (a -> a) (forall clk . Signal clk a -> Signal clk a)
data TestBin a = TestBin String (a -> a -> a) (forall clk . Signal clk a -> Signal clk a -> Signal clk a)
data TestIx a b = TestIx String (a -> b -> Bool) (forall clk . Signal clk a -> Signal clk b -> Signal clk Bool)


-- This only tests at the *value* level, and ignores testing unknowns.

testUniOp :: forall a b .
             (Rep a, Show a, SingI (W a)
             ,Rep b, Show b, SingI (W b))
          => TestSeq
          -> String
          -> (a -> b)
          -> (forall clk . Signal clk a -> Signal clk b)
          -> [a]
          -> IO ()
testUniOp (TestSeq test _) nm opr lavaOp us0 = do
        let driver = do
                outStdLogicVector "i0" (toS us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = lavaOp (i0)
                outStdLogicVector "o0" (o0)

            res = toS (fmap opr us0)

        test nm (length us0) dut (driver >> matchExpected "o0" res)


testBinOp :: forall a b c .
             (Rep a, Show a, SingI (W a)
             ,Rep b, Show b, SingI (W b)
             ,Rep c, Show c, SingI (W c))
          => TestSeq
          -> String
          -> (a -> b -> c)
          -> (forall clk . Signal clk a -> Signal clk b -> Signal clk c)
          -> List (a,b)
          -> IO ()
testBinOp (TestSeq test _) nm opr lavaOp gen = do
        let (us0,us1) = unzip gen
            driver = do
                outStdLogicVector "i0" (toS us0)
                outStdLogicVector "i1" (toS us1)
            dut = do
                i0 <- inStdLogicVector "i0"
                i1 <- inStdLogicVector "i1"
                let o0 = lavaOp (i0) (i1)
                outStdLogicVector "o0" (o0)
            res = toS (Prelude.zipWith opr us0 us1)

        test nm (length gen) dut (driver >> matchExpected "o0" res)

testTriOp :: forall a b c d .
             (Rep a, Show a, SingI (W a)
             ,Rep b, Show b, SingI (W b)
             ,Rep c, Show c, SingI (W c)
             ,Rep d, Show d, SingI (W d))
          => TestSeq
          -> String
          -> (a -> b -> c -> d)
          -> (forall clk . Signal clk a -> Signal clk b -> Signal clk c -> Signal clk d)
          -> List (a,b,c)
          -> IO ()
testTriOp (TestSeq test _) nm opr lavaOp gen = do
        let (us0,us1,us2) = unzip3 gen
            driver = do
                outStdLogicVector "i0" (toS us0)
                outStdLogicVector "i1" (toS us1)
                outStdLogicVector "i2" (toS us2)
            dut = do
                i0 <- inStdLogicVector "i0"
                i1 <- inStdLogicVector "i1"
                i2 <- inStdLogicVector "i2"
                let o0 = lavaOp (i0) (i1) (i2)
                outStdLogicVector "o0" (o0)
            res = toS (Prelude.zipWith3 opr us0 us1 us2)
        test nm (length gen) dut (driver >> matchExpected "o0" res)

------------------------------------------------------------------------------------------------

testOpsEq :: forall w . (Rep w, Eq w, Show w, SingI (W w)) => TestSeq -> String -> List w -> IO ()
testOpsEq test tyName ws = do
        let ws2 = pair ws
	    bs = finiteCases (length ws2) :: [Bool]

        sequence_
          [ testTriOp test (name ++ "/" ++ tyName) opr (lavaOp)
	    	      	[ (b,w1,w2) | (b,(w1,w2)) <- zip bs ws2 ]
          | TestMux name opr lavaOp <-
                [ TestMux "mux" (\ c a b -> if c then a else b) (\ c a b -> mux c (b,a))
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) opr lavaOp ws2
          | TestCmp name opr lavaOp <-
                [ TestCmp "double-equal" (==) (.==.)
                , TestCmp "not-equal"    (/=) (./=.)
                ]
          ]

        return ()

------------------------------------------------------------------------------------------------

testOpsOrd :: (Rep w, Num w, Ord w, Show w, SingI (W w)) => TestSeq -> String -> List w -> IO ()
testOpsOrd test tyName ws = do
        let ws2 = pair ws

        testOpsEq test tyName ws

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) opr lavaOp ws2
          | TestCmp name opr lavaOp <-
                [ TestCmp "greater-than" (>)  (.>.)
                , TestCmp "less-than"    (<)  (.<.)
                , TestCmp "gt-equal"     (>=) (.>=.)
                , TestCmp "lt-equal"     (<=) (.<=.)
                ]
          ]

        return ()

------------------------------------------------------------------------------------------------


testOpsNum :: forall w .
        (Ord w, Rep w, Num w, Show w, SingI (W w)) => TestSeq -> String -> List w -> IO ()
testOpsNum test tyName ws = do
        testOpsOrd test tyName ws

        let ws2 = pair ws

        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) opr lavaOp ws
          | TestUni name opr lavaOp <-
                [ TestUni "negate" negate negate
                , TestUni "abs"    abs    abs
                , TestUni "signum" signum signum
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) opr lavaOp ws2
          | TestBin name opr lavaOp <-
                [ TestBin "add" (+) (+)
                , TestBin "sub" (-) (-)
                , TestBin "mul" (*) (*)
                , TestBin "max" max max
                , TestBin "min" min min
                ]
          ]

        return ()
testOpsFractional :: forall w .
        (Ord w, Rep w, Show w, Fractional w, SingI (W w)) => TestSeq -> String -> [w] -> IO ()
testOpsFractional test tyName ws = do
        testOpsNum test tyName ws

        -- TODO: add in recip
        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) opr lavaOp ws
          | TestUni name opr lavaOp <-
                -- for now, we *only* divide by powers of two, that we *can* divide by (in range)
                [ TestUni ("divide_by_" ++ show n) (/ (fromIntegral n)) (/ (fromIntegral n))
                | n <- [2,4,8,16,32,64,128,256::Integer]
                , let w = fromInteger n :: w
                , (show n ++ ".0") == show w
                ]
          ]

        return ()

----------------------------------------------------------------------------------------

testOpsBits :: forall w .
        (Ord w, Rep w, Num w, Show w, Bits w, SingI (W w)) => TestSeq -> String -> List w -> IO ()
testOpsBits test tyName ws = do
        testOpsNum test tyName ws

        let ws2 = pair ws

        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) opr lavaOp ws
          | TestUni name opr lavaOp <-
                [ TestUni "complement" complement complement
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) opr lavaOp ws2
          | TestBin name opr lavaOp <-
                [ TestBin "bitwise-and" (.&.) (.&.)
                , TestBin "bitwise-or"  (.|.) (.|.)
                , TestBin "xor"         (xor) (xor)
                ]
          ]


        return ()


--testOpsBits2 :: forall w . (Ord w, Rep w, Num w, Show w, Bits w, SingI (W w), Integral (W w), Rep (W w), SingI (W (W w))) => TestSeq -> String -> List w -> IO ()
testOpsBits2 :: forall w . (Ord w, Rep w, Num w, Show w, Bits w, SingI (W w)) => TestSeq -> String -> List w -> IO ()
testOpsBits2 test tyName ws = do
	testOpsBits test tyName ws

        sequence_
          [ testUniOp test (name ++ "/" ++ tyName ++ "/" ++ show rot1)
	    	      opr
		      lavaOp
		      ws
          | rot0 <- take (bitSize (error "witness" :: w)) [0..] :: [Int]
	  , rot1 <- if rot0 == 0 then [rot0] else [rot0,-rot0]
	  , let f :: (Bits a) => (a -> Int -> a) -> (a -> a)
	        f fn = flip fn (fromIntegral rot1)
	  , TestUni name opr lavaOp <-
		[ TestUni "shift" (f shift) (f shift)
		, TestUni "rotate" (f rotate) (f rotate)
		] ++ if rot1 >= 0 then
                [ TestUni "shiftL" (f shiftL) (f shiftL)
		, TestUni "shiftR" (f shiftR) (f shiftR)
		, TestUni "rotateL" (f rotateL) (f rotateL)
		, TestUni "rotateR" (f rotateR) (f rotateR)
		, TestUni "clearBit" (f clearBit) (f clearBit)
		, TestUni "setBit" (f setBit) (f setBit)
		, TestUni "complementBit" (f complementBit) (f complementBit)
                ] else []
          ]

        let ws2 :: List (w,Fin (W w))
            ws2 = zip ws (cycle [0..maxBound])

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName)
	    	      opr
		      lavaOp
		      ws2
	  | TestIx name opr lavaOp <-
		[ TestIx "testABit" (\ a b -> testBit a (fromIntegral b)) (testABit)
                ]
          ]

        return ()

pair :: [a] -> [(a, a)]
pair ws = [ (a,b) | a <- ws, b <- ws ]

--------------------------------------------------------------------------------------
-- Testing register and memory
testRegister :: forall a . (Show a, Eq a, Rep a, SingI (W a)) => TestSeq -> String -> List a -> IO ()
testRegister  (TestSeq test _) tyName ~(u0:us0) = do
        let r = register :: a -> Seq a -> Seq a
            driver = do
                outStdLogicVector "i0" (toS us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = r u0 $ i0
                outStdLogicVector "o0" o0
            res = toS (u0 : us0)
        test ("register/" ++ tyName) (length us0) dut (driver >> matchExpected "o0" res)
        return ()

testDelay :: forall a . (Show a, Eq a, Rep a, SingI (W a)) => TestSeq -> String -> List a -> IO ()
testDelay  (TestSeq test _) tyName (us0) = do
        let dlay = delay :: Seq a -> Seq a
            driver = do
                outStdLogicVector "i0" (toS us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = dlay $ i0
                outStdLogicVector "o0" o0

            res = mkShallowXS (S.Cons unknownX (Just (S.fromList (map pureX us0))))

        test ("delay/" ++ tyName) (length us0) dut (driver >> matchExpected "o0" res)
        return ()

{-
testFluxCapacitor :: forall a . (Show a, Eq a, Rep a, Size (W a), Size (ADD (W a) X1))
	  => TestSeq -> String -> List (Maybe a)  -> (forall c . (Clock c) => Signal c a -> Signal c a) -> IO ()
testFluxCapacitor (TestSeq test toL) tyName ws seqOp = do
      let xs = toL ws

          driver = do
      	    outStdLogicVector "i0" (toS xs :: Seq (Maybe a))
          dut = do
      	    i0 <- inStdLogicVector "i0" :: Fabric (Seq (Maybe a))
	    let o0 = fluxCapacitor seqOp i0 :: Seq (Maybe a)
	    outStdLogicVector "o0" o0


	  -- just the results from the internal function
	  ys :: [Maybe a]
	  ys = fromSeq (seqOp (toS [ x | Just x <- xs ]) :: Seq a)

	  -- The xs0 Nothing => not enabled, for ys0 Nothing => Unknown.
	  fn :: [Maybe a] -> [Maybe a] -> Stream (X (Maybe a))
	  fn (Nothing:xs0) ys0          = Cons (pureX Nothing) (fn xs0 ys0)
	  fn (Just {}:xs0) (Just y:ys0) = Cons (pureX (Just y)) (fn xs0 ys0)
	  fn _ _                        = S.repeat unknownX

	  res :: Seq (Maybe a)
          res = shallowSeq (Cons (pureX Nothing) (fn xs ys))




      test ("flux-capacitor/" ++ tyName ++ "/") (length xs)
      	   		       dut (driver >> matchExpected "o0" res)

      return ()
-}
