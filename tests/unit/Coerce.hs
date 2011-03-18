{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Coerce where

import Language.KansasLava

import Utils
import Data.Sized.Unsigned
import Data.Sized.Matrix as M hiding (length)
import Data.Sized.Signed

tests :: TestSeq -> IO ()
tests test = do

        let t :: (Bounded w2, Integral w2, Integral w1, Rep w2, Rep w1, Size (W w1), Size (W w2)) =>
                  String -> Witness w2 -> Gen w1 -> IO ()

            t str witness arb = testUnsigned test str witness arb

        t "U1_U1" (Witness :: Witness U1) (dubSeq (arbitrary :: Gen U1))
        t "U2_U1" (Witness :: Witness U2) (dubSeq (arbitrary :: Gen U1))
        t "U3_U1" (Witness :: Witness U3) (dubSeq (arbitrary :: Gen U1))
        t "U1_U2" (Witness :: Witness U1) (dubSeq (arbitrary :: Gen U2))
        t "U2_U2" (Witness :: Witness U2) (dubSeq (arbitrary :: Gen U2))
        t "U3_U2" (Witness :: Witness U3) (dubSeq (arbitrary :: Gen U2))
        t "U1_U3" (Witness :: Witness U1) (dubSeq (arbitrary :: Gen U3))
        t "U2_U3" (Witness :: Witness U2) (dubSeq (arbitrary :: Gen U3))
        t "U3_U3" (Witness :: Witness U3) (dubSeq (arbitrary :: Gen U3))
        t "U4_U8" (Witness :: Witness U4) (dubSeq (arbitrary :: Gen U8))
        t "U8_U4" (Witness :: Witness U8) (dubSeq (arbitrary :: Gen U4))

        t "U1_S2" (Witness :: Witness U1) (dubSeq (arbitrary :: Gen S2))
        t "U2_S2" (Witness :: Witness U2) (dubSeq (arbitrary :: Gen S2))
        t "U3_S2" (Witness :: Witness U3) (dubSeq (arbitrary :: Gen S2))
        t "U1_S3" (Witness :: Witness U1) (dubSeq (arbitrary :: Gen S3))
        t "U2_S3" (Witness :: Witness U2) (dubSeq (arbitrary :: Gen S3))
        t "U3_S3" (Witness :: Witness U3) (dubSeq (arbitrary :: Gen S3))
        t "U8_S4" (Witness :: Witness U8) (dubSeq (arbitrary :: Gen S4))

        t "X2_X2" (Witness :: Witness X2) (dubSeq (arbitrary :: Gen X2))
        t "X2_X3" (Witness :: Witness X2) (dubSeq (arbitrary :: Gen X3))
        t "X2_X4" (Witness :: Witness X2) (dubSeq (arbitrary :: Gen X4))
        t "X2_X5" (Witness :: Witness X2) (dubSeq (arbitrary :: Gen X5))

        t "X3_X2" (Witness :: Witness X3) (dubSeq (arbitrary :: Gen X2))
        t "X3_X3" (Witness :: Witness X3) (dubSeq (arbitrary :: Gen X3))
        t "X3_X4" (Witness :: Witness X3) (dubSeq (arbitrary :: Gen X4))
        t "X3_X5" (Witness :: Witness X3) (dubSeq (arbitrary :: Gen X5))

        t "X4_X2" (Witness :: Witness X4) (dubSeq (arbitrary :: Gen X2))
        t "X4_X3" (Witness :: Witness X4) (dubSeq (arbitrary :: Gen X3))
        t "X4_X4" (Witness :: Witness X4) (dubSeq (arbitrary :: Gen X4))
        t "X4_X5" (Witness :: Witness X4) (dubSeq (arbitrary :: Gen X5))

        t "X5_X2" (Witness :: Witness X5) (dubSeq (arbitrary :: Gen X2))
        t "X5_X3" (Witness :: Witness X5) (dubSeq (arbitrary :: Gen X3))
        t "X5_X4" (Witness :: Witness X5) (dubSeq (arbitrary :: Gen X4))
        t "X5_X5" (Witness :: Witness X5) (dubSeq (arbitrary :: Gen X5))

        let t :: (Bounded w1, Bounded w2, Integral w2, Integral w1, Rep w2, Rep w1, Size (W w1), Size (W w2)) =>
                  String -> Witness w2 -> Gen w1 -> IO ()
            t str witness arb = testSigned test str witness arb

        t "S2_U1" (Witness :: Witness S2) (dubSeq (arbitrary :: Gen U1))
        t "S3_U1" (Witness :: Witness S3) (dubSeq (arbitrary :: Gen U1))
        t "S2_U2" (Witness :: Witness S2) (dubSeq (arbitrary :: Gen U2))
        t "S3_U2" (Witness :: Witness S3) (dubSeq (arbitrary :: Gen U2))
        t "S2_U3" (Witness :: Witness S2) (dubSeq (arbitrary :: Gen U3))
        t "S3_U3" (Witness :: Witness S3) (dubSeq (arbitrary :: Gen U3))
        t "S4_U8" (Witness :: Witness S4) (dubSeq (arbitrary :: Gen U8))
        t "S8_U4" (Witness :: Witness S8) (dubSeq (arbitrary :: Gen U4))

        t "S2_S2" (Witness :: Witness S2) (dubSeq (arbitrary :: Gen S2))
        t "S3_S2" (Witness :: Witness S3) (dubSeq (arbitrary :: Gen S2))
        t "S2_S3" (Witness :: Witness S2) (dubSeq (arbitrary :: Gen S3))
        t "S3_S3" (Witness :: Witness S3) (dubSeq (arbitrary :: Gen S3))
        t "S4_S8" (Witness :: Witness S4) (dubSeq (arbitrary :: Gen S8))
        t "S8_S4" (Witness :: Witness S8) (dubSeq (arbitrary :: Gen S4))

        let t :: (Eq w2, Eq w1, Show w2, Rep w2, Rep w1, W w2 ~ W w1, Size (W w1)) =>
                 String -> Witness w2 -> Gen w1 -> IO ()
            t str witness arb = testCoerce test str witness arb

        t "S16_M_X4_S4"    (Witness :: Witness S16) (dubSeq (arbitrary :: Gen (Matrix X4 S4)))
        t "U15_M_X3_S5"    (Witness :: Witness U15) (dubSeq (arbitrary :: Gen (Matrix X3 S5)))
        t "U3_M_X3_Bool"   (Witness :: Witness U3) (dubSeq (arbitrary :: Gen (Matrix X3 Bool)))
        t "U1_M_X1_Bool"   (Witness :: Witness U1) (dubSeq (arbitrary :: Gen (Matrix X1 Bool)))
        t "Bool_M_X1_Bool" (Witness :: Witness Bool) (dubSeq (arbitrary :: Gen (Matrix X1 Bool)))

        t "M_X4_S4_S16"    (Witness :: Witness (Matrix X4 S4)) (dubSeq (arbitrary :: Gen S16))
        t "M_X3_S5_U15"    (Witness :: Witness (Matrix X3 S5)) (dubSeq (arbitrary :: Gen U15))
        t "M_X3_Bool_U3"   (Witness :: Witness (Matrix X3 Bool)) (dubSeq (arbitrary :: Gen U3))
        t "M_X1_Bool_U1"   (Witness :: Witness (Matrix X1 Bool)) (dubSeq (arbitrary :: Gen U1))
        t "M_X1_Bool_Bool" (Witness :: Witness (Matrix X1 Bool)) (dubSeq (arbitrary :: Gen Bool))

        t "U3_x_U2_U5"     (Witness :: Witness (U3,U2)) (dubSeq (arbitrary :: Gen U5))
        t "U5_U3_x_U2"     (Witness :: Witness U5) (dubSeq (arbitrary :: Gen (U3,U2)))
        t "U4_U3_x_Bool"   (Witness :: Witness U4) (dubSeq (arbitrary :: Gen (U3,Bool)))

        t "Bool_U1"        (Witness :: Witness Bool) (dubSeq (arbitrary :: Gen U1))
        t "U1_Bool"        (Witness :: Witness U1) (dubSeq (arbitrary :: Gen Bool))

        t "Bool_Bool"      (Witness :: Witness Bool) (dubSeq (arbitrary :: Gen Bool))
        t "U8_U8"          (Witness :: Witness U8)   (dubSeq (arbitrary :: Gen U8))

        return ()


testUnsigned :: forall w1 w2 . (Num w2, Integral w1, Integral w2, Bounded w2, Eq w1, Rep w1, Eq w2, Show w2, Rep w2, Size (W w1), Size (W w2))
            => TestSeq -> String -> Witness w2 -> Gen w1 -> IO ()
testUnsigned (TestSeq test toList) tyName Witness ws = do
        let ms = toList ws
            cir = unsigned :: Seq w1 -> Seq w2
            driver = do
                outStdLogicVector "i0" (coerce (toSeq ms) :: Seq (Unsigned (W w1)))
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (coerce i0)
                outStdLogicVector "o0" (coerce o0)
            res = do
                outStdLogicVector "o0" (coerce shallow)

            -- shallow will always pass; it *is* the semantics here
            shallow :: Seq w2
            shallow = cir $ toSeq' [ if toInteger m > toInteger (maxBound :: w2)
                                     || toInteger m < toInteger (minBound :: w2)
                                     then fail "out of bounds"
                                     else return m
                                   | m <- ms
                                   ]
        test ("unsigned/" ++ tyName) (length ms) driver dut res
        return ()

testSigned :: forall w1 w2 . (Num w2, Integral w1, Bounded w1, Integral w2, Bounded w2, Eq w1, Rep w1, Eq w2, Show w2, Rep w2, Size (W w1), Size (W w2))
            => TestSeq -> String -> Witness w2 -> Gen w1 -> IO ()
testSigned (TestSeq test toList) tyName Witness ws = do
        let ms = toList ws
            cir = signed :: Seq w1 -> Seq w2
            driver = do
                outStdLogicVector "i0" (coerce (toSeq ms) :: Seq (Unsigned (W w1)))
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (coerce i0)
                outStdLogicVector "o0" (coerce o0)
            res = do
                outStdLogicVector "o0" (coerce shallow)

            -- shallow will always pass; it *is* the semantics here
            shallow :: Seq w2
            shallow = cir $ toSeq' [ if (fromIntegral m :: Int) > fromIntegral (maxBound :: w2)
                                     || (fromIntegral m :: Int) < fromIntegral (minBound :: w2)
                                     then fail "out of bounds"
                                     else return m
                                   | m <- ms
                                   ]
        test ("signed/" ++ tyName) (length ms) driver dut res
        return ()

testCoerce :: forall w1 w2 . (Eq w1, Rep w1, Eq w2, Show w2, Rep w2, W w1 ~ W w2, Size (W w2))
            => TestSeq -> String -> Witness w2 -> Gen w1 -> IO ()
testCoerce (TestSeq test toList) tyName Witness ws = do
        let ms = toList ws
            cir = coerce :: Seq w1 -> Seq w2
            driver = do
                outStdLogicVector "i0" (coerce (toSeq ms) :: Seq (Unsigned (W w1)))
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (coerce i0)
                outStdLogicVector "o0" (coerce o0)
            res = do
                outStdLogicVector "o0" (coerce shallow)

            -- shallow will always pass; it *is* the semantics here
            shallow :: Seq w2
            shallow = cir $ toSeq ms
        test ("coerce/" ++ tyName) (length ms) driver dut res
        return ()

