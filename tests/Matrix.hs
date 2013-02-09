{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification, DataKinds, TypeOperators #-}

module Matrix where

import Language.KansasLava
import Test

import Data.Sized.Unsigned
import qualified Data.Sized.Matrix as M hiding (length)
import Data.Sized.Fin

import Data.Array.IArray

import GHC.TypeLits

type List a = [a]

type instance (1 + 1) = 2
type instance (1 + 3) = 4
type instance (2 * 4) = 8
type instance (3 * 4) = 12

tests :: Tests ()
tests = do

        let t1 :: (SingI w1,
                   SingI (W w2),
                   Rep w2, Eq w2, Num w2, Show w2,
                   SingI (w1 * (W w2))
                  ) => String -> List (M.Vector w1 w2) -> Tests ()
	    t1 str arb = testMatrix1 str arb

        t1 "X1xU4" (allCases :: List (M.Vector 1 U4))
        t1 "X2xU4" (allCases :: List (M.Vector 2 U4))
        t1 "X3xU4" (allCases :: List (M.Vector 3 U4))

        let t2 :: (SingI w1,
                   SingI (W w2),
                   Rep w2, Eq w2, Num w2, Show w2,
                   SingI (w1 * (W w2))
                  ) => String -> List (M.Vector w1 w2) -> Tests ()
	    t2 str arb = testMatrix2 str arb

        t2 "X1xU4" (allCases :: List (M.Vector 1 U4))
        t2 "X2xU4" (allCases :: List (M.Vector 2 U4))
        t2 "X3xU4" (allCases :: List (M.Vector 3 U4))

        let t3 :: (Rep w, Show w, SingI (1 + W w)) => String -> List (Maybe w) -> Tests ()
	    t3 str arb = testMatrix3 str arb

        t3 "U3" (allCases :: List (Maybe U3))
        t3 "Bool" (allCases :: List (Maybe Bool))

        return ()


testMatrix1 :: forall w1 w2 .
               ( SingI w1,
                 Num w2, Eq w2, Show w2, Rep w2,
                 SingI (W w2),
                 SingI (w1 * (W w2)))
            => String
            -> List (M.Vector w1 w2)
            -> Tests ()
testMatrix1 tyName ws = do
        let ms = ws
            cir = sum . elems . unpack :: Seq (M.Vector w1 w2) -> Seq w2
            driver = do
                outStdLogicVector "i0" (bitwise (toS ms) :: Seq (Unsigned (w1 * (W w2))))
            dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = cir i0
                outStdLogicVector "o0" o0
            res = toS [ sum $ elems m | m <- ms ] :: Seq w2

        test ("matrix/1/" ++ tyName) (length ms) dut (driver >> matchExpected "o0" res)

testMatrix2 :: forall w1 w2 .
               ( SingI w1,
                 Eq w2, Show w2, Rep w2, Num w2,
               SingI (w1 * (W w2))
               )
            => String
            -> List (M.Vector w1 w2)
            -> Tests ()
testMatrix2 tyName ws = do
        let ms = ws
            cir = pack . (\ m -> M.forAll $ \ i -> m ! i) . unpack :: Seq (M.Vector w1 w2) -> Seq (M.Vector w1 w2)
            driver = do
		return ()
                outStdLogicVector "i0" (bitwise (toS ms) :: Seq (Unsigned (w1 * (W w2))))
            dut = do
		return ()
                i0 <- inStdLogicVector "i0"
                let o0 = cir i0
                outStdLogicVector "o0" o0
            res = toS [ m | m <- ms ] :: Seq (M.Vector w1 w2)

        test ("matrix/2/" ++ tyName) (length ms) dut (driver >> matchExpected "o0" res)


testMatrix3 :: forall w1 .
	(Rep w1, Show w1, SingI (1 + (W w1)))
            => String
            -> List (Maybe w1)
            -> Tests ()
testMatrix3 tyName ws = do
        let ms = ws
	    cir :: Seq (Enabled w1) -> Seq (Enabled w1)
            cir = mapEnabled (\ m -> unpackMatrix m ! (0 :: (Fin 2)))
		. mapEnabled (\ x -> packMatrix (M.matrix [ x, x ]))
            driver = do
		return ()
                outStdLogicVector "i0" (bitwise (toS ms) :: Seq (Enabled w1))
            dut = do
		return ()
                i0 <- inStdLogicVector "i0"
                let o0 = cir i0
                outStdLogicVector "o0" o0
            res = toS [ m | m <- ms ] :: Seq (Enabled w1)

        test ("matrix/3/" ++ tyName) (length ms) dut (driver >> matchExpected "o0" res)
