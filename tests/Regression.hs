{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification, DataKinds, TypeOperators, KindSignatures #-}

module Regression where

import Language.KansasLava hiding ((:=))
import Language.KansasLava.Test
import Language.KansasLava.RTL

import Data.Sized.Sized
import Data.Sized.Unsigned
import Data.Sized.Matrix

import GHC.TypeLits

tests :: TestSeq -> IO ()
tests (TestSeq test _) = do
        let driver1 = outStdLogicVector "i0" res1

	    res1 = undefinedS :: Seq (Sized 1)

        test "regression/1/funMap/Matrix" 1000 fab1 (driver1 >> matchExpected "o0" res1)

        let res2 = toS $ cycle (True : replicate 15 False)

        test "regression/2/RTL" 1000 fab2 (return () >> matchExpected "o0" res2)

cir1 :: Signal CLK (Sized 1) -> Signal CLK (Matrix (Sized 16) U8)
cir1 = funMap fn
  where fn _ = return $ matrix [0..15]

type instance (16 * 8) = 144

fab1 :: Fabric ()
fab1 = do
    a <- inStdLogicVector "i0"
    let b = cir1 a
    outStdLogicVector "o0" b

cir2 :: Seq Bool
cir2 = runRTL $ do
	count <- newReg (0 :: (Unsigned 4))
--        CASE [ OTHERWISE $ do count := reg count + 1 ]  -- TODO: fix this
	count := reg count + 1
	return  (reg count .==. 0)

fab2 :: Fabric ()
fab2 = outStdLogicVector "o0" cir2
