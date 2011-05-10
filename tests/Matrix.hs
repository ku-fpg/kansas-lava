{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Matrix where

import Language.KansasLava

{-
import Data.Sized.Arith
import Data.Sized.Unsigned
import qualified Data.Sized.Matrix as M hiding (length)
import Data.Sized.Ix
-}

tests :: TestSeq -> IO ()
tests _test = do
{-
        let t str arb = testMatrix1 test str arb

        t "X1xU4" (dubSeq (arbitrary :: Gen (M.Matrix X2 U4)))

        let t str arb = testMatrix2 test str arb

        t "X1xU4" (dubSeq (arbitrary :: Gen (M.Matrix X2 U4)))
-}
        return ()

{-
testMatrix1 :: forall w1 w2 .
               ( Integral w1, Size w1, Eq w1, Rep w1
               , Num w2, Eq w2, Show w2, Rep w2
               , Size (W w2), Size (MUL w1 (W w2)))
            => TestSeq
            -> String
            -> Gen (M.Matrix w1 w2)
            -> IO ()
testMatrix1 (TestSeq test toList') tyName ws = do
        let ms = toList' ws
            cir = sum . M.toList . unpack :: Seq (M.Matrix w1 w2) -> Seq w2
            driver = do
                outStdLogicVector "i0" (coerce (toSeq ms) :: Seq (Unsigned (MUL w1 (W w2))))
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (coerce i0)
                outStdLogicVector "o0" (coerce o0)
            res = do
                outStdLogicVector "o0" (coerce (toSeq [ sum $ M.toList m | m <- ms ] :: Seq w2))

        test ("matrix/1/" ++ tyName) (length ms) driver dut res

testMatrix2 :: forall w1 w2 .
               ( Integral w1, Size w1, Eq w1, Rep w1
               , Eq w2, Show w2, Rep w2, Num w2
               , Size (MUL w1 (W w2)))
            => TestSeq
            -> String
            -> Gen (M.Matrix w1 w2)
            -> IO ()
testMatrix2 (TestSeq test toList') tyName ws = do
        let ms = toList' ws
            cir = pack . (\ m -> M.forAll $ \ i -> m M.! i) . unpack :: Seq (M.Matrix w1 w2) -> Seq (M.Matrix w1 w2)
            driver = do
                outStdLogicVector "i0" (coerce (toSeq ms) :: Seq (Unsigned (MUL w1 (W w2))))
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir (coerce i0)
                outStdLogicVector "o0" (coerce o0)
            res = do
                outStdLogicVector "o0" (coerce (toSeq [ m | m <- ms ] :: Seq (M.Matrix w1 w2)))

        test ("matrix/2/" ++ tyName) (length ms) driver dut res

-}
