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

import System.Environment
import System.Directory
import System.Cmd

import qualified System.Posix.Env as Posix

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
	debugCircuit [OptimizeReify] fn


numberOfCycles :: Int
numberOfCycles = 50


dumpDir = "examine/"

testSome
  :: (Ports a, Testable a1, Examine a) 
  => String -> a -> (Example a -> a1) -> IO ()
testSome nm tst f = do
	testReify nm tst		
	testSomeTruth numberOfCycles nm $ f (example (examine nm tst))
  	createDirectoryIfMissing True (dumpDir ++ nm ++ "/")	-- this should move into dumpBitTrace
	dumpBitTrace (dumpDir ++ nm ++ "/") numberOfCycles
	mkTestbench [OptimizeReify] [] nm tst	-- inc optimizations?

--	system $ "cp " ++ dumpDir ++ nm ++ "*.bits " ++ dumpDir ++ nm 
--	system $ "cp " ++ dumpDir ++ nm ++ "*.info " ++ dumpDir ++ nm 

	return ()	

main = do
	Posix.setEnv "LAVA_SIM_PATH" dumpDir True
  	createDirectoryIfMissing True dumpDir

	let env = takeThenSeq 7 shallowRst env
	    inp :: Seq U4
	    inp  = toSeq $ cycle [0..15]
	    inp2 :: Seq U4
	    inp2 = toSeq $ cycle $ reverse [0..15]

	    eInp :: Seq (Enabled U4)
	    eInp = toEnabledSeq 
		 $ Prelude.zipWith (\ a b -> if b then Just a else Nothing)
	 		           (cycle [0..15]) 
				   (cycle [False,True,False,False,True])

	let nop x = return ()

	-- And the tests (comment them out wiht nop)

	nop $ testSome "regX" 
		(register :: Rst -> Comb U4 -> Seq U4 -> Seq U4)
		(\ reg -> reg .*. env .*. 10 .*. inp)
		
	nop $ testSome "delayX" 
		(delay :: Seq U4 -> Seq U4)
		(\ f -> f .*. inp)
		
	nop $ testSome "muxX" 
		((\ a b c -> mux2 a (b,c)) :: Seq Bool -> Seq U4 -> Seq U4 -> Seq U4)
		(\ f -> f .*. toSeq (cycle [True,False,True,True,False]) .*. inp .*. inp2)	

	nop $ testSome "enabledRegisterX"
		(enabledRegister :: Rst -> Comb U4 -> Seq (Enabled U4) -> Seq U4)
		(\ f -> f .*. env .*. 10 .*. eInp)


