{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Wakarusa where

import Language.KansasLava
import Language.KansasLava.Test
import Language.KansasLava.Fabric
import Language.KansasLava.Wakarusa

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix

tests :: TestSeq -> IO ()
tests (TestSeq test _) = do
        let driver = return ()
	    res = delay 99 :: Seq Int 
	 in test "wakarusa/unit/1" 1000 
                (compileToFabric prog1)
                (driver >> matchExpected "o0" res)

        let driver = return ()
	    res = delay $ delay 9 :: Seq Int 
	 in test "wakarusa/unit/2" 1000 
                (compileToFabric prog2)
                (driver >> matchExpected "o0" res)

        return ()

------------------------------------------------------------------------

prog1 :: STMT ()
prog1 = do
        o0 :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled)
        loop <- LABEL
        o0  := 99
        GOTO loop

        FORK loop

------------------------------------------------------------------------

prog2 :: STMT ()
prog2 = do
        o0 :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled)
        VAR v0  :: VAR Int <- SIGNAL $ var 9
        loop <- LABEL
        o0 := v0
        GOTO loop
        FORK loop

------------------------------------------------------------------------
