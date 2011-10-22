{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Regression where

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix

tests :: TestSeq -> IO ()
tests (TestSeq test _) = do
        let driver = outStdLogicVector "i0" res

	    res = undefinedS :: Seq X0

        test "regression/1/funMap/Matrix" 1000 fab1 (driver >> matchExpected "o0" res)

cir1 :: Signal CLK X0 -> Signal CLK (Matrix X16 U8)
cir1 = funMap fn 
  where fn _ = return $ matrix [0..15]

fab1 :: Fabric ()
fab1 = do
    a <- inStdLogicVector "i0"
    let b = cir1 a 
    outStdLogicVector "o0" b

