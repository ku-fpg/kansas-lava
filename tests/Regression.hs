{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification, DataKinds, TypeOperators, KindSignatures #-}

module Regression where

import Language.KansasLava hiding ((:=))
import Language.KansasLava.Test


import Data.Sized.Fin
import Data.Sized.Unsigned
import Data.Sized.Matrix

import GHC.TypeLits

tests :: TestSeq -> IO ()
tests (TestSeq test _) = do
        let driver1 = outStdLogicVector "i0" res1

	    res1 = undefinedS :: Seq (Fin 1)

        test "regression/1/funMap/Matrix" 1000 fab1 (driver1 >> matchExpected "o0" res1)

cir1 :: Signal CLK (Fin 1) -> Signal CLK (Matrix (Fin 16) U8)
cir1 = funMap fn
  where fn _ = return $ matrix [0..15]

type instance (16 * 8) = 144

fab1 :: Fabric ()
fab1 = do
    a <- inStdLogicVector "i0"
    let b = cir1 a
    outStdLogicVector "o0" b

