{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Matrix where

import Language.KansasLava
import Language.KansasLava.Testing.Thunk

import Utils
import Data.Sized.Unsigned
import qualified Data.Sized.Matrix as M hiding (length)
import Data.Sized.Ix

tests :: TestSeq -> IO ()
tests test = do
        let t str arb = testMatrix1 test str arb

        t "X1xU4" (dubSeq (arbitrary :: Gen (M.Matrix X2 U4)))

        let t str arb = testMatrix2 test str arb

        t "X1xU4" (dubSeq (arbitrary :: Gen (M.Matrix X2 U4)))

        return ()


testMatrix1 :: forall w1 w2 . (Num w2, Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (M.Matrix w1 w2) -> IO ()
testMatrix1 (TestSeq test toList') tyName ws = do
        let ms = toList' ws
        let cir = sum . M.toList . unpack :: Seq (M.Matrix w1 w2) -> Seq w2
        let thu = Thunk cir
                        (\ cir -> cir (toSeq ms)
                        )
            res :: Seq w2
            res = toSeq [ sum $ M.toList m | m <- ms ]
        test ("matrix/1/" ++ tyName) (length ms) thu res

testMatrix2 :: forall w1 w2 . (Num w2, Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (M.Matrix w1 w2) -> IO ()
testMatrix2 (TestSeq test toList') tyName ws = do
        let ms = toList' ws
        let cir = pack . (\ m -> M.forAll $ \ i -> m M.! i) . unpack :: Seq (M.Matrix w1 w2) -> Seq (M.Matrix w1 w2)
        let thu = Thunk cir
                        (\ cir -> cir (toSeq ms)
                        )
            res :: Seq (M.Matrix w1 w2)
            res = toSeq [ m | m <- ms ]
        test ("matrix/2/" ++ tyName) (length ms) thu res


