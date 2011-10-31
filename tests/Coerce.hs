{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Coerce where

import Language.KansasLava
import Language.KansasLava.Test

import Data.Sized.Unsigned
import Data.Sized.Matrix as M hiding (length)
import Data.Sized.Signed

type List a = [a]

tests :: TestSeq -> IO ()
tests test = do

        let t1 :: (Bounded w2, Integral w2, Integral w1, Rep w2, Rep w1, Size (W w1), Size (W w2)) =>
                  String -> Witness w2 -> List w1 -> IO ()

            t1 str witness arb = testUnsigned test str witness arb

        t1 "U1_U1" (Witness :: Witness U1) ((allCases :: List U1))
        t1 "U2_U1" (Witness :: Witness U2) ((allCases :: List U1))
        t1 "U3_U1" (Witness :: Witness U3) ((allCases :: List U1))
        t1 "U1_U2" (Witness :: Witness U1) ((allCases :: List U2))
        t1 "U2_U2" (Witness :: Witness U2) ((allCases :: List U2))
        t1 "U3_U2" (Witness :: Witness U3) ((allCases :: List U2))
        t1 "U1_U3" (Witness :: Witness U1) ((allCases :: List U3))
        t1 "U2_U3" (Witness :: Witness U2) ((allCases :: List U3))
        t1 "U3_U3" (Witness :: Witness U3) ((allCases :: List U3))
        t1 "U4_U8" (Witness :: Witness U4) ((allCases :: List U8))
        t1 "U8_U4" (Witness :: Witness U8) ((allCases :: List U4))

        t1 "U1_S2" (Witness :: Witness U1) ((allCases :: List S2))
        t1 "U2_S2" (Witness :: Witness U2) ((allCases :: List S2))
        t1 "U3_S2" (Witness :: Witness U3) ((allCases :: List S2))
        t1 "U1_S3" (Witness :: Witness U1) ((allCases :: List S3))
        t1 "U2_S3" (Witness :: Witness U2) ((allCases :: List S3))
        t1 "U3_S3" (Witness :: Witness U3) ((allCases :: List S3))
        t1 "U8_S4" (Witness :: Witness U8) ((allCases :: List S4))

        t1 "X2_X2" (Witness :: Witness X2) ((allCases :: List X2))
        t1 "X2_X3" (Witness :: Witness X2) ((allCases :: List X3))
        t1 "X2_X4" (Witness :: Witness X2) ((allCases :: List X4))
        t1 "X2_X5" (Witness :: Witness X2) ((allCases :: List X5))

        t1 "X3_X2" (Witness :: Witness X3) ((allCases :: List X2))
        t1 "X3_X3" (Witness :: Witness X3) ((allCases :: List X3))
        t1 "X3_X4" (Witness :: Witness X3) ((allCases :: List X4))
        t1 "X3_X5" (Witness :: Witness X3) ((allCases :: List X5))

        t1 "X4_X2" (Witness :: Witness X4) ((allCases :: List X2))
        t1 "X4_X3" (Witness :: Witness X4) ((allCases :: List X3))
        t1 "X4_X4" (Witness :: Witness X4) ((allCases :: List X4))
        t1 "X4_X5" (Witness :: Witness X4) ((allCases :: List X5))

        t1 "X5_X2" (Witness :: Witness X5) ((allCases :: List X2))
        t1 "X5_X3" (Witness :: Witness X5) ((allCases :: List X3))
        t1 "X5_X4" (Witness :: Witness X5) ((allCases :: List X4))
        t1 "X5_X5" (Witness :: Witness X5) ((allCases :: List X5))

        let t2 :: (Bounded w1, Bounded w2, Integral w2, Integral w1, Rep w2, Rep w1, Size (W w1), Size (W w2)) =>
                  String -> Witness w2 -> List w1 -> IO ()
            t2 str witness arb = testSigned test str witness arb

        t2 "S2_U1" (Witness :: Witness S2) ((allCases :: List U1))
        t2 "S3_U1" (Witness :: Witness S3) ((allCases :: List U1))
        t2 "S2_U2" (Witness :: Witness S2) ((allCases :: List U2))
        t2 "S3_U2" (Witness :: Witness S3) ((allCases :: List U2))
        t2 "S2_U3" (Witness :: Witness S2) ((allCases :: List U3))
        t2 "S3_U3" (Witness :: Witness S3) ((allCases :: List U3))
        t2 "S4_U8" (Witness :: Witness S4) ((allCases :: List U8))
        t2 "S8_U4" (Witness :: Witness S8) ((allCases :: List U4))

        t2 "S2_S2" (Witness :: Witness S2) ((allCases :: List S2))
        t2 "S3_S2" (Witness :: Witness S3) ((allCases :: List S2))
        t2 "S2_S3" (Witness :: Witness S2) ((allCases :: List S3))
        t2 "S3_S3" (Witness :: Witness S3) ((allCases :: List S3))
        t2 "S4_S8" (Witness :: Witness S4) ((allCases :: List S8))
        t2 "S8_S4" (Witness :: Witness S8) ((allCases :: List S4))

        let t3 :: (Eq w2, Eq w1, Show w1, Show w2, Rep w2, Rep w1, W w2 ~ W w1, Size (W w1)) =>
                 String -> Witness w2 -> List w1 -> IO ()
            t3 str witness arb = testBitwise test str witness arb

        t3 "S16_M_X4_S4"    (Witness :: Witness S16) ((allCases :: List (Matrix X4 S4)))
        t3 "U15_M_X3_S5"    (Witness :: Witness U15) ((allCases :: List (Matrix X3 S5)))
        t3 "U3_M_X3_Bool"   (Witness :: Witness U3) ((allCases :: List (Matrix X3 Bool)))
        t3 "U1_M_X1_Bool"   (Witness :: Witness U1) ((allCases :: List (Matrix X1 Bool)))
        t3 "Bool_M_X1_Bool" (Witness :: Witness Bool) ((allCases :: List (Matrix X1 Bool)))

        t3 "M_X4_S4_S16"    (Witness :: Witness (Matrix X4 S4)) ((allCases :: List S16))
        t3 "M_X3_S5_U15"    (Witness :: Witness (Matrix X3 S5)) ((allCases :: List U15))
        t3 "M_X3_Bool_U3"   (Witness :: Witness (Matrix X3 Bool)) ((allCases :: List U3))
        t3 "M_X1_Bool_U1"   (Witness :: Witness (Matrix X1 Bool)) ((allCases :: List U1))
        t3 "M_X1_Bool_Bool" (Witness :: Witness (Matrix X1 Bool)) ((allCases :: List Bool))

        t3 "U3_x_U2_U5"     (Witness :: Witness (U3,U2)) ((allCases :: List U5))
        t3 "U5_U3_x_U2"     (Witness :: Witness U5) ((allCases :: List (U3,U2)))
        t3 "U4_U3_x_Bool"   (Witness :: Witness U4) ((allCases :: List (U3,Bool)))

        t3 "Bool_U1"        (Witness :: Witness Bool) ((allCases :: List U1))
        t3 "U1_Bool"        (Witness :: Witness U1) ((allCases :: List Bool))

        t3 "Bool_Bool"      (Witness :: Witness Bool) ((allCases :: List Bool))
        t3 "U8_U8"          (Witness :: Witness U8)   ((allCases :: List U8))

        let t4 :: (Eq w2, Eq w1, Show w1, Show w2, Rep w2, Rep w1, W w2 ~ W w1, Size (W w1)) =>
                 String -> Witness w2 -> List w1 -> (w1 -> w2) -> IO ()
            t4 str witness arb f = testCoerce test str witness arb f

        t4 "Bool_U1"        (Witness :: Witness Bool) ((allCases :: List U1))
			$ \ u1 -> u1 == 1
        t4 "U1_Bool"        (Witness :: Witness U1) ((allCases :: List Bool))
			$ \ b -> if b then 1 else 0
        t4 "Bool_Bool"      (Witness :: Witness Bool) ((allCases :: List Bool)) id
        t4 "U8_U8"          (Witness :: Witness U8)   ((allCases :: List U8)) id

        return ()


testUnsigned :: forall w1 w2 . (Num w2, Integral w1, Integral w2, Bounded w2, Eq w1, Rep w1, Eq w2, Show w2, Rep w2, Size (W w1), Size (W w2))
            => TestSeq -> String -> Witness w2 -> List w1 -> IO ()
testUnsigned (TestSeq test _) tyName Witness ws = do
        let ms = ws
            cir = unsigned :: Seq w1 -> Seq w2
            driver = do
                outStdLogicVector "i0" (toS ms)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (i0)
                outStdLogicVector "o0" (o0)

            -- will always pass; it *is* the semantics here
            res :: Seq w2
            res = cir $ toS' [ if toInteger m > toInteger (maxBound :: w2)
                                 || toInteger m < toInteger (minBound :: w2)
                                then fail "out of bounds"
                                else return m
                               | m <- ms
                               ]
        test ("unsigned/" ++ tyName) (length ms) dut (driver >> matchExpected "o0" res)
        return ()

testSigned :: forall w1 w2 . (Num w2, Integral w1, Bounded w1, Integral w2, Bounded w2, Eq w1, Rep w1, Eq w2, Show w2, Rep w2, Size (W w1), Size (W w2))
            => TestSeq -> String -> Witness w2 -> List w1 -> IO ()
testSigned (TestSeq test _) tyName Witness ws = do
        let ms = ws
            cir = signed :: Seq w1 -> Seq w2
            driver = do
                outStdLogicVector "i0" (toS ms)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (i0)
                outStdLogicVector "o0" (o0)

            -- will always pass; it *is* the semantics here
            res :: Seq w2
            res = cir $ toS' [ if (fromIntegral m :: Int) > fromIntegral (maxBound :: w2)
                                 || (fromIntegral m :: Int) < fromIntegral (minBound :: w2)
                                 then fail "out of bounds"
                                 else return m
                               | m <- ms
                               ]
        test ("signed/" ++ tyName) (length ms) dut (driver >> matchExpected "o0" res)
        return ()

testBitwise :: forall w1 w2 . (Eq w1, Rep w1, Eq w2, Show w1, Show w2, Rep w2, W w1 ~ W w2, Size (W w2))
            => TestSeq -> String -> Witness w2 -> List w1 -> IO ()
testBitwise (TestSeq test _) tyName Witness ws = do
        let ms = ws
            cir = bitwise :: Seq w1 -> Seq w2
            driver = do
                outStdLogicVector "i0" (toS ms)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (i0)
                outStdLogicVector "o0" (o0)
            -- will always pass; it *is* the semantics here
            res :: Seq w2
            res = cir $ toS ms
        test ("bitwise/" ++ tyName) (length ms) dut (driver >> matchExpected "o0" res)
        return ()

testCoerce :: forall w1 w2 . (Eq w1, Rep w1, Eq w2, Show w1, Show w2, Rep w2, W w1 ~ W w2, Size (W w2))
            => TestSeq -> String -> Witness w2 -> List w1 -> (w1 -> w2) -> IO ()
testCoerce (TestSeq test _) tyName Witness ws f = do
        let ms =  ws
            cir = coerce f :: Seq w1 -> Seq w2
            driver = do
                outStdLogicVector "i0" (toS ms)
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (i0)
                outStdLogicVector "o0" (o0)
            -- will always pass; it *is* the semantics here
            res :: Seq w2
            res = cir $ toS ms
        test ("coerce/" ++ tyName) (length ms) dut (driver >> matchExpected "o0" res)
        return ()
