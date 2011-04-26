{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Others (tests) where

import Language.KansasLava
--import qualified Data.Stream as S
import qualified Language.KansasLava.Stream as S

import Data.Bits
import Data.Sized.Ix
import Data.Sized.Sampled
import Data.Sized.Signed
import Data.Sized.Unsigned

import Control.Applicative

import Utils

tests :: TestSeq -> IO ()
tests test = do
        -- Just the Num Stuff
        let t :: (Fractional a, Ord a, Rep a, Size (W a)) => String -> Gen a -> IO ()
            t str arb = testOpsFractional test str arb
        -- With Sampled, use
        --  * powers of two scale, larger than 1
        --  * make sure there are enough bits to represent
        --     both the fractional and non-fractional part.

        t "Sampled/X8xX8" (arbitrary :: Gen (Sampled X8 X8))
-- These do not represent every integer in their range, so fail
--        t "Sampled/X4xX2" (arbitrary :: Gen (Sampled X4 X2))
--        t "Sampled/X2xX2" (arbitrary :: Gen (Sampled X2 X2))
--        t "Sampled/X2xX1" (arbitrary :: Gen (Sampled X2 X1))
-- This have a round error; looks like a base case
--        t "Sampled/X1xX2" (arbitrary :: Gen (Sampled X1 X2))
        t "Sampled/X1xX4" (arbitrary :: Gen (Sampled X1 X4))
        t "Sampled/X8xX10"(arbitrary :: Gen (Sampled X8 X10))
        t "Sampled/X128xX16"(arbitrary :: Gen (Sampled X128 X16))

        -- Just the Bits Stuff
        let t :: (Ord a, Bits a, Rep a, Size (W a)) => String -> Gen a -> IO ()
            t str arb = testOpsBits test str arb

        t "U1" (arbitrary :: Gen U1)
        t "U2" (arbitrary :: Gen U2)
        t "U3" (arbitrary :: Gen U3)
        t "U4" (arbitrary :: Gen U4)
        t "U5" (arbitrary :: Gen U5)
        t "U6" (arbitrary :: Gen U6)
        t "U7" (arbitrary :: Gen U7)
        t "U8" (arbitrary :: Gen U8)
        t "U32" (arbitrary :: Gen U32)
{- ghci keeps getting killed during these, Out Of Memory maybe?
        t "U64" (arbitrary :: Gen (Unsigned X64))
-}

        -- no S1
        t "S2" (arbitrary :: Gen S2)
        t "S3" (arbitrary :: Gen S3)
        t "S4" (arbitrary :: Gen S4)
        t "S5" (arbitrary :: Gen S5)
        t "S6" (arbitrary :: Gen S6)
        t "S7" (arbitrary :: Gen S7)
        t "S8" (arbitrary :: Gen S8)
        t "S32" (arbitrary :: Gen S32)
{- ghci keeps getting killed during these, Out Of Memory maybe?
        t "S64" (arbitrary :: Gen (Signed X64))
-}
        -- Just the Eq Stuff

        -- None

        --  Now registers
        let t :: (Eq a, Show a, Rep a, Size (W a)) => String -> Gen a -> IO ()
            t str arb = testRegister test str arb

        t "U1" (loop 10 (arbitrary :: Gen U1))
        t "U2" (loop 10 (arbitrary :: Gen U2))
        t "U3" (loop 10 (arbitrary :: Gen U3))
        t "Int" (loop 10 (arbitrary :: Gen Int))
        t "Bool" (loop 10 (arbitrary :: Gen Bool))
        let t :: (Eq a, Show a, Rep a, Size (W a)) => String -> Gen a -> IO ()
            t str arb = testDelay test str arb

        t "U1" (loop 10 (arbitrary :: Gen U1))
        t "U2" (loop 10 (arbitrary :: Gen U2))
        t "U3" (loop 10 (arbitrary :: Gen U3))
        t "Int" (loop 10 (arbitrary :: Gen Int))
        t "Bool" (loop 10 (arbitrary :: Gen Bool))

-- This only tests at the *value* level, and ignores testing unknowns.

testUniOp :: forall a b .
             (Rep a, Show a, Size (W a)
             ,Rep b, Show b, Size (W b))
          => TestSeq
          -> String
          -> (a -> b)
          -> (Comb a
          -> Comb b)
          -> Gen a
          -> IO ()
testUniOp (TestSeq test toList) nm op lavaOp gen = do
        let us0 = toList gen
        let driver = do
                outStdLogicVector "i0" (toSeq us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = liftS1 lavaOp (i0)
                outStdLogicVector "o0" (o0)
            res = do
                outStdLogicVector "o0" (toSeq (fmap op us0))

        test nm (length us0) driver dut res


testBinOp :: forall a b c .
             (Rep a, Show a, Size (W a)
             ,Rep b, Show b, Size (W b)
             ,Rep c, Show c, Size (W c))
          => TestSeq
          -> String
          -> (a -> b -> c)
          -> (Comb a -> Comb b -> Comb c)
          -> Gen (a,b)
          -> IO ()
testBinOp (TestSeq test toList) nm op lavaOp gen = do
        let (us0,us1) = unzip $ toList gen
            driver = do
                outStdLogicVector "i0" (toSeq us0)
                outStdLogicVector "i1" (toSeq us1)
            dut = do
                i0 <- inStdLogicVector "i0"
                i1 <- inStdLogicVector "i1"
                let o0 = liftS2 lavaOp (i0) (i1)
                outStdLogicVector "o0" (o0)
            res = do
                outStdLogicVector "o0" (toSeq (Prelude.zipWith op us0 us1))

        test nm (length (zip us0 us1)) driver dut res

testTriOp :: forall a b c d .
             (Rep a, Show a, Size (W a)
             ,Rep b, Show b, Size (W b)
             ,Rep c, Show c, Size (W c)
             ,Rep d, Show d, Size (W d))
          => TestSeq
          -> String
          -> (a -> b -> c -> d)
          -> (Comb a -> Comb b -> Comb c -> Comb d)
          -> Gen (a,b,c)
          -> IO ()
testTriOp (TestSeq test toList) nm op lavaOp gen = do
        let (us0,us1,us2) = unzip3 $ toList gen
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
            res = do
                outStdLogicVector "o0" ( toSeq (Prelude.zipWith3 op us0 us1 us2))
        test nm (length (zip3 us0 us1 us2)) driver dut res

------------------------------------------------------------------------------------------------

testOpsEq :: (Rep w, Eq w, Show w, Size (W w)) => TestSeq -> String -> Gen w -> IO ()
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
                [ ("double-equal",(==),(.==.))
--              , ("not-equal",(/=),(./=.))
                ]
          ]


------------------------------------------------------------------------------------------------


testOpsOrd :: (Rep w, Ord w, Show w, Size (W w)) => TestSeq -> String -> Gen w -> IO ()
testOpsOrd test tyName ws = do
        let ws2 = pair ws

        testOpsEq test tyName ws

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <-
                [ ("greater-than",(>),(.>.))
                , ("less-than",(<),(.<.))
                , ("gt-equal",(>=),(.>=.))
                , ("lt-equal",(<=),(.<=.))
                ]
          ]


------------------------------------------------------------------------------------------------


testOpsNum :: forall w .
        (Ord w, Rep w, Num w, Size (W w)) => TestSeq -> String -> Gen w -> IO ()
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

testOpsFractional :: forall w .
        (Ord w, Rep w, Fractional w, Size (W w)) => TestSeq -> String -> Gen w -> IO ()
testOpsFractional test tyName ws = do
        testOpsNum test tyName ws

        -- TODO: add in recip
        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) op lavaOp ws
          | (name,op,lavaOp) <-
                -- for now, we *only* divide by powers of two, that we *can* divide by (in range)
                [ ("divide_by_" ++ show n,(/ (fromIntegral n)),(/ (fromIntegral n)))
                | n <- [2,4,8,16,32,64,128,256::Integer]
                , let w = fromInteger n :: w
                , (show n ++ ".0") == show w
                ]
          ]

----------------------------------------------------------------------------------------

testOpsBits :: forall w .
        (Ord w, Rep w, Bits w, Size (W w)) => TestSeq -> String -> Gen w -> IO ()
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
                [ ("bitwise-and",(.&.),(.&.))
                , ("bitwise-or",(.|.),(.|.))
                , ("xor",(xor),(xor))
                ]
          ]

pair :: (Applicative f) => f a -> f (a, a)
pair ws = pure (,) <*> ws <*> ws

{- unused?
triple :: (Applicative f) => f a -> f (a, a, a)
triple ws = pure (,,) <*> ws <*> ws <*> ws
-}

--------------------------------------------------------------------------------------
-- Testing register and memory
testRegister :: forall a . (Show a, Eq a, Rep a, Size (W a)) => TestSeq -> String -> Gen a -> IO ()
testRegister  (TestSeq test toList) tyName ws = do
        let (u0:us0) = toList ws
            reg = register :: a -> Seq a -> Seq a
            driver = do
                outStdLogicVector "i0" (toSeq us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = reg u0 $ i0
                outStdLogicVector "o0" o0
            res = do
                outStdLogicVector "o0" (toSeq (u0 : us0))
        test ("register/" ++ tyName) (length us0) driver dut res
        return ()

testDelay :: forall a . (Show a, Eq a, Rep a, Size (W a)) => TestSeq -> String -> Gen a -> IO ()
testDelay  (TestSeq test toList) tyName ws = do
        let us0 = toList ws
            dlay = delay :: Seq a -> Seq a
            driver = do
                outStdLogicVector "i0" (toSeq us0)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = dlay $ i0
                outStdLogicVector "o0" o0
            res = do
                outStdLogicVector "o0" (shallowSeq (S.Cons unknownX (S.fromList (map pureX us0))))

        test ("delay/" ++ tyName) (length us0) driver dut res
        return ()
