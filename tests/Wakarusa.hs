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

        let driver = outStdLogicVector "i0" (toS [100..] :: Seq Int)
	    res = delay $ toS [100..] :: Seq Int 
	 in test "wakarusa/unit/3" 1000 
                (compileToFabric prog3)
                (driver >> matchExpected "o0" res)

        let driver = return ()
	    res = delay $ delay $ 10 :: Seq Int 
	 in test "wakarusa/unit/4" 1000 
                (compileToFabric prog4)
                (driver >> matchExpected "o0" res)

        let driver = return ()
	    res = delay $ toS [0..] :: Seq Int 
	 in test "wakarusa/unit/5" 1000 
                (compileToFabric prog5)
                (driver >> matchExpected "o0" res)

        let driver = return ()
	    res = delay $ delay $ toS (concat [ [n,n,n] | n <- [1..]]) :: Seq Int 
	 in test "wakarusa/unit/6" 1000 
                (compileToFabric prog6)
                (driver >> matchExpected "o0" res)

        let driver = outStdLogic "i0" (toS $ map Ack $ cycle [False,False,True])
	    res = toS $ concat [ [ Just x, Just x, Just x ] | x <- [1..] ] :: Seq (Maybe Int)
	 in test "wakarusa/unit/7" 1000 
                (compileToFabric prog7)
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

prog3 :: STMT ()
prog3 = do
        o0 :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled)
        i0 :: EXPR Int  <- INPUT (inStdLogicVector "i0")
        loop <- LABEL
        o0 := i0 ||| GOTO loop
        FORK loop

------------------------------------------------------------------------

prog4 :: STMT ()
prog4 = do
        o0     :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled)
        VAR v0 :: VAR Int   <- SIGNAL $ var 9
        loop <- LABEL
        v0 := 10
        o0 := v0
        GOTO loop
        FORK loop

------------------------------------------------------------------------

prog5 :: STMT ()
prog5 = do
        o0     :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled)
        VAR v0 :: VAR Int   <- SIGNAL $ var 0
        loop <- LABEL
        (v0 := v0 + 1) 
          ||| (o0 := v0) 
          ||| GOTO loop
        FORK loop

------------------------------------------------------------------------

prog6 :: STMT ()
prog6 = do
        o0     :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled . probeS "o0")
        VAR v0 :: VAR Int   <- SIGNAL $ var 0
        loop <- LABEL
        v0 := v0 + 1
        o0 := v0
        GOTO loop
        FORK loop

------------------------------------------------------------------------

-- a small value writer using the Ack protocol.
prog7 :: STMT ()
prog7 = do
        wAckBox@(WritableAckBox oB iB) :: WritableAckBox Int <- connectWritableAckBox "o0" "i0"
        VAR v0 :: VAR Int   <- SIGNAL $ var 1
        loop <- LABEL
        putAckBox wAckBox v0 
                ||| (v0 := v0 + 1)
                ||| GOTO loop
        FORK loop

