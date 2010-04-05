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
testSome nm tst f
	-- Hack to speed up the generation of our tests
  | nm /= "arithmeticX" = putStrLn $ "Ignoring " ++ show nm

  | otherwise = do
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
	    env' = takeThenSeq 40 shallowRst env'

	    inp :: Seq U4
	    inp  = toSeq $ cycle [0..15]
	    inp2 :: Seq U4
	    inp2 = toSeq $ cycle $ reverse [0..15]
	    inp3 :: Seq U4
	    inp3 = toSeq $ step 3 $ cycle $ reverse $ 0 : [0..15]

	    step n (x:xs) = x : (step n $ drop (n - 1) xs)

	    eInp :: Seq (Enabled U4)
	    eInp = toEnabledSeq 
		 $ Prelude.zipWith (\ a b -> if b then Just a else Nothing)
	 		           (cycle [0..15]) 
				   (cycle [False,True,False,False,True])

	let nop x = return ()

	-- And the tests (comment them out wiht nop)

	testSome "regX" 
		(register :: Rst -> Comb U4 -> Seq U4 -> Seq U4)
		(\ reg -> reg .*. env .*. 10 .*. inp)
		
	testSome "delayX" 
		(delay :: Seq U4 -> Seq U4)
		(\ f -> f .*. inp)
		
	testSome "muxX" 
		((\ a b c -> mux2 a (b,c)) :: Seq Bool -> Seq U4 -> Seq U4 -> Seq U4)
		(\ f -> f .*. toSeq (cycle [True,False,True,True,False]) .*. inp .*. inp2)	

	testSome "arithmeticX"
		((\ a b -> pack (matrix [a + b, a - b] :: Matrix X2 (Seq U4))) :: Seq U4 -> Seq U4 -> Seq (Matrix X2 U4))
		(\ f -> f .*. inp .*. inp3)

	testSome "enabledRegisterX"
		(enabledRegister :: Rst -> Comb U4 -> Seq (Enabled U4) -> Seq U4)
		(\ f -> f .*. env .*. 10 .*. eInp)

	testSome "pipeToMemoryX"
		((\ rst pipe -> memoryToMatrix (pipeToMemory rst pipe)) :: Rst -> Seq (Pipe X8 U4) -> Seq (Matrix X8 U4))
		(\ f -> f .*. env' 
			  .*. toEnabledSeq (concat
					   [ [ return (x,y), Nothing ]
					   | (x,y) <- cycle $ [(i:: X8,(fromIntegral i * fromIntegral i) :: U4)
							      | i <- [0..7]
							      ]
					   ])
	        )
	testSome "pipeToMemory2X"
		(pipeToMemory :: Rst -> Seq (Pipe X2 U4) -> Seq X2 -> Seq U4)
		(\ f -> f .*. env' 
			  .*. toEnabledSeq (concat
					   [ [ return (x,y) ]
					   | (x,y) <- cycle $ [(i:: X2, j :: U4)
							      | (i,j) <- zip (cycle [0,0,1,1,1,0,1,1]) [0..7]
							      ]
					   ])
			  .*. toSeq (cycle [0,1])
		)

