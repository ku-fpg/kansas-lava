{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts, Rank2Types, ExistentialQuantification #-}


import Language.KansasLava

import System.IO.Unsafe

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Dynamic

import Data.Bits

import Data.Sized.Arith as A
import Data.Sized.Matrix as M

import Data.Sized.Unsigned as U

import Language.KansasLava.VHDL.Testbench

-- Lots of tests for primitives

testSomeTruth:: (Testable a) => Int -> String -> a -> IO ()
testSomeTruth n nm fn = do
	putStrLn $ "Testing " ++ nm ++ " function"
	putStrLn $ "======="
	putStrLn $ showSomeTT n $ truthTable fn	
	
testReify :: (Ports a) => String -> a -> IO ()
testReify nm fn = do
	putStrLn $ "Testing " ++ nm ++ " reify"
	putStrLn $ "======="
	debugCircuit [] fn


testSome nm tst = do
	return ()
	

main = do

	let tst ::Rst -> Comb U4 -> Seq U4 -> Seq U4
	    tst = register

	testReify "register" tst		

	testSomeTruth 50 "register" $
		let env = takeThenSeq 7 shallowRst env
		    def = 1
		    inp = toSeq $ cycle [0..3]
		 in example (examine "registerX" tst) .*. env .*. def .*. inp

	dumpBitTrace "exam/" 50

	mkTestbench [] [] "registerX" tst

