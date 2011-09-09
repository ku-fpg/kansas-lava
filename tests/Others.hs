{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Others (tests) where

import Language.KansasLava
import qualified Language.KansasLava.Stream as S

import Data.Bits
import Data.Sized.Ix
import Data.Sized.Sampled
import Data.Sized.Signed
import Data.Sized.Unsigned

type List a = [a]

tests :: TestSeq -> IO ()
tests test = do
        -- Just the Num Stuff
        let t1 :: (Fractional a, Ord a, Rep a, Size (W a)) => String -> [a] -> IO ()
            t1 str arb = testOpsFractional test str arb
        -- With Sampled, use
        --  * powers of two scale, larger than 1
        --  * make sure there are enough bits to represent
        --     both the fractional and non-fractional part.

        t1 "Sampled/X8xX8" (allCases :: [Sampled X8 X8])
        t1 "Sampled/X1xX4" (allCases :: [Sampled X1 X4])
        t1 "Sampled/X8xX10"(finiteCases 100 :: [Sampled X8 X10])
        t1 "Sampled/X128xX16"(finiteCases 100 ::[Sampled X128 X16])

        -- Just the Bits Stuff
        let t2 :: (Ord a, Bits a, Rep a, Size (W a)) => String -> List a -> IO ()
            t2 str arb = testOpsBits test str arb

        t2 "U1" (allCases :: List U1)
        t2 "U2" (allCases :: List U2)
        t2 "U3" (allCases :: List U3)
        t2 "U4" (allCases :: List U4)
        t2 "U5" (allCases :: List U5)
        t2 "U6" (allCases :: List U6)
        t2 "U7" (allCases :: List U7)
        t2 "U8" (allCases :: List U8)
        t2 "U32" (finiteCases 100 :: List U32)
{- ghci keeps getting killed during these, Out Of Memory maybe?
        t2 "U64" (finiteCases 100 :: List (Unsigned X64))
-}

        -- no S1
        t2 "S2" (allCases :: List S2)
        t2 "S3" (allCases :: List S3)
        t2 "S4" (allCases :: List S4)
        t2 "S5" (allCases :: List S5)
        t2 "S6" (allCases :: List S6)
        t2 "S7" (allCases :: List S7)
        t2 "S8" (allCases :: List S8)
        t2 "S32" (finiteCases 100 :: List S32)
{- ghci keeps getting killed during these, Out Of Memory maybe?
        t2 "S64" (finiteCases 100 :: List (Signed X64))
-}
        -- Just the Eq Stuff

        -- None

        --  Now registers
        let t3 :: (Eq a, Show a, Rep a, Size (W a)) => String -> List a -> IO ()
            t3 str arb = testRegister test str arb

        t3 "U1" ( (finiteCases 1000 :: List U1))
        t3 "U2" ( (finiteCases 1000 :: List U2))
        t3 "U3" ( (finiteCases 1000 :: List U3))
        t3 "Int" ( (finiteCases 1000 :: List Int))
        t3 "Bool" ( (finiteCases 1000 :: List Bool))

        let t4 :: (Eq a, Show a, Rep a, Size (W a)) => String -> List a -> IO ()
            t4 str arb = testDelay test str arb

        t4 "U1" ( (finiteCases 1000 :: List U1))
        t4 "U2" ( (finiteCases 1000 :: List U2))
        t4 "U3" ( (finiteCases 1000 :: List U3))
        t4 "Int" ( (finiteCases 1000 :: List Int))
        t4 "Bool" ( (finiteCases 1000 :: List Bool))


{- We are not ready for this yet
	-- Test the flux capacitor
        let t5 :: (Eq a, Show a, Rep a, Size (W a), Size (ADD (W a) X1)) 
	       => String -> List (Maybe a) -> (forall c. (Clock c) => CSeq c a -> CSeq c a) -> IO ()
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

-- This only tests at the *value* level, and ignores testing unknowns.

testUniOp :: forall a b .
             (Rep a, Show a, Size (W a)
             ,Rep b, Show b, Size (W b))
          => TestSeq
          -> String
          -> (a -> b)
          -> (Comb a -> Comb b)
          -> [a]
          -> IO ()
testUniOp (TestSeq test _) nm opr lavaOp us0 = do
        let driver = do
                outStdLogicVector "i0" (toSeq us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = liftS1 lavaOp (i0)
                outStdLogicVector "o0" (o0)
                
            res = toSeq (fmap opr us0)

        test nm (length us0) dut (driver >> matchExpected "o0" res)


testBinOp :: forall a b c .
             (Rep a, Show a, Size (W a)
             ,Rep b, Show b, Size (W b)
             ,Rep c, Show c, Size (W c))
          => TestSeq
          -> String
          -> (a -> b -> c)
          -> (Comb a -> Comb b -> Comb c)
          -> List (a,b)
          -> IO ()
testBinOp (TestSeq test _) nm opr lavaOp gen = do
        let (us0,us1) = unzip gen
            driver = do
                outStdLogicVector "i0" (toSeq us0)
                outStdLogicVector "i1" (toSeq us1)
            dut = do
                i0 <- inStdLogicVector "i0"
                i1 <- inStdLogicVector "i1"
                let o0 = liftS2 lavaOp (i0) (i1)
                outStdLogicVector "o0" (o0)
            res = toSeq (Prelude.zipWith opr us0 us1)

        test nm (length gen) dut (driver >> matchExpected "o0" res)

testTriOp :: forall a b c d .
             (Rep a, Show a, Size (W a)
             ,Rep b, Show b, Size (W b)
             ,Rep c, Show c, Size (W c)
             ,Rep d, Show d, Size (W d))
          => TestSeq
          -> String
          -> (a -> b -> c -> d)
          -> (Comb a -> Comb b -> Comb c -> Comb d)
          -> List (a,b,c)
          -> IO ()
testTriOp (TestSeq test _) nm opr lavaOp gen = do
        let (us0,us1,us2) = unzip3 gen
            driver = do
                outStdLogicVector "i0" (toSeq us0)
                outStdLogicVector "i1" (toSeq us1)
                outStdLogicVector "i2" (toSeq us2)
            dut = do
                i0 <- inStdLogicVector "i0"
                i1 <- inStdLogicVector "i1"
                i2 <- inStdLogicVector "i2"
                let o0 = liftS3 lavaOp (i0) (i1) (i2)
                outStdLogicVector "o0" (o0)
            res = toSeq (Prelude.zipWith3 opr us0 us1 us2)
        test nm (length gen) dut (driver >> matchExpected "o0" res)

------------------------------------------------------------------------------------------------

testOpsEq :: (Rep w, Eq w, Show w, Size (W w)) => TestSeq -> String -> List w -> IO ()
testOpsEq test tyName ws = do
        let ws2 = pair ws
	    bs = finiteCases (length ws2) :: [Bool]

        sequence_
          [ testTriOp test (name ++ "/" ++ tyName) opr lavaOp
	    	      	[ (b,w1,w2) | (b,(w1,w2)) <- zip bs ws2 ]

          | (name,opr,lavaOp) <-
                [ ("mux",\ c a b -> if c then a else b,\ c a b -> mux2 c (a,b))
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) opr lavaOp ws2
          | (name,opr,lavaOp) <-
                [ ("double-equal",(==),(.==.))
                , ("not-equal",(/=),(./=.))
                ]
          ]

------------------------------------------------------------------------------------------------

testOpsOrd :: (Rep w, Ord w, Show w, Size (W w)) => TestSeq -> String -> List w -> IO ()
testOpsOrd test tyName ws = do
        let ws2 = pair ws

        testOpsEq test tyName ws

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) opr lavaOp ws2
          | (name,opr,lavaOp) <-
                [ ("greater-than",(>),(.>.))
                , ("less-than",(<),(.<.))
                , ("gt-equal",(>=),(.>=.))
                , ("lt-equal",(<=),(.<=.))
                ]
          ]


------------------------------------------------------------------------------------------------


testOpsNum :: forall w .
        (Ord w, Rep w, Num w, Size (W w)) => TestSeq -> String -> List w -> IO ()
testOpsNum test tyName ws = do
        testOpsOrd test tyName ws

        let ws2 = pair ws

        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) opr lavaOp ws
          | (name,opr,lavaOp) <-
                [ ("negate",negate,negate)
                , ("abs",abs,abs)
                , ("signum",signum,signum)
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) opr lavaOp ws2
          | (name,opr,lavaOp) <-
                [ ("add",(+),(+))
                , ("sub",(-),(-))
                , ("mul",(*),(*))
                , ("max",max,max)
                , ("min",min,min)
                ]
          ]

testOpsFractional :: forall w .
        (Ord w, Rep w, Fractional w, Size (W w)) => TestSeq -> String -> [w] -> IO ()
testOpsFractional test tyName ws = do
        testOpsNum test tyName ws

        -- TODO: add in recip
        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) opr lavaOp ws
          | (name,opr,lavaOp) <-
                -- for now, we *only* divide by powers of two, that we *can* divide by (in range)
                [ ("divide_by_" ++ show n,(/ (fromIntegral n)),(/ (fromIntegral n)))
                | n <- [2,4,8,16,32,64,128,256::Integer]
                , let w = fromInteger n :: w
                , (show n ++ ".0") == show w
                ]
          ]

----------------------------------------------------------------------------------------

testOpsBits :: forall w .
        (Ord w, Rep w, Bits w, Size (W w)) => TestSeq -> String -> List w -> IO ()
testOpsBits test tyName ws = do
        testOpsNum test tyName ws

        let ws2 = pair ws


        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) opr lavaOp ws
          | (name,opr,lavaOp) <-
                [ ("complement",complement,complement)
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) opr lavaOp ws2
          | (name,opr,lavaOp) <-
                [ ("bitwise-and",(.&.),(.&.))
                , ("bitwise-or",(.|.),(.|.))
                , ("xor",(xor),(xor))
                ]
          ]

pair :: [a] -> [(a, a)]
pair ws = [ (a,b) | a <- ws, b <- ws ]

--------------------------------------------------------------------------------------
-- Testing register and memory
testRegister :: forall a . (Show a, Eq a, Rep a, Size (W a)) => TestSeq -> String -> List a -> IO ()
testRegister  (TestSeq test _) tyName ~(u0:us0) = do
        let r = register :: a -> Seq a -> Seq a
            driver = do
                outStdLogicVector "i0" (toSeq us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = r u0 $ i0
                outStdLogicVector "o0" o0
            res = toSeq (u0 : us0)
        test ("register/" ++ tyName) (length us0) dut (driver >> matchExpected "o0" res)
        return ()

testDelay :: forall a . (Show a, Eq a, Rep a, Size (W a)) => TestSeq -> String -> List a -> IO ()
testDelay  (TestSeq test _) tyName (us0) = do
        let dlay = delay :: Seq a -> Seq a
            driver = do
                outStdLogicVector "i0" (toSeq us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = dlay $ i0
                outStdLogicVector "o0" o0
                
            res = shallowSeq (S.Cons unknownX (S.fromList (map pureX us0)))

        test ("delay/" ++ tyName) (length us0) dut (driver >> matchExpected "o0" res)
        return ()

{-
testFluxCapacitor :: forall a . (Show a, Eq a, Rep a, Size (W a), Size (ADD (W a) X1)) 
	  => TestSeq -> String -> List (Maybe a)  -> (forall c . (Clock c) => CSeq c a -> CSeq c a) -> IO ()
testFluxCapacitor (TestSeq test toL) tyName ws seqOp = do
      let xs = toL ws
      
          driver = do
      	    outStdLogicVector "i0" (toSeq xs :: Seq (Maybe a))
          dut = do
      	    i0 <- inStdLogicVector "i0" :: Fabric (Seq (Maybe a))
	    let o0 = fluxCapacitor seqOp i0 :: Seq (Maybe a)
	    outStdLogicVector "o0" o0


	  -- just the results from the internal function
	  ys :: [Maybe a]
	  ys = fromSeq (seqOp (toSeq [ x | Just x <- xs ]) :: Seq a)

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