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
{-


{-# NOINLINE eChan #-}

eChan :: Chan (String -> Int -> Int -> IO ())
eChan = unsafePerformIO $ newChan


--runE :: IO () -> IO ()
--runE m = do
--	m

type Seq' a = [a]

{-# NOINLINE examine #-}
examine :: forall a b . (RepWire a, RepWire b) => String -> (Seq a -> Seq b) -> (Seq a -> Seq b)
examine nm fn arg = unsafePerformIO $ do
	let res = fn arg
	writeChan eChan (\ path n depth -> writeBitfilePair (path  ++ nm ++ "_" ++ show n) depth (pack (arg,res) :: Seq (a,b)))
	return $ res

dumpBitTrace :: String -> Int -> IO ()
dumpBitTrace path depth = do
	let loop n = do
		ok <- isEmptyChan eChan
		if ok then return () else do
			f <- readChan eChan
			f path n depth
			loop (succ n)

	loop 0

		
deriving instance Typeable1 Seq
deriving instance Typeable1 Unsigned
-}


testAllTruth:: (Testable a) => String -> a -> IO ()
testAllTruth nm fn = do
	putStrLn $ "Testing " ++ nm ++ " function"
	putStrLn $ "======="
	putStrLn $ showAllTT $ truthTable fn
	
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


main = do
	let tst ::Rst -> Comb U4 -> Seq U4 -> Seq U4
	    tst = register
	testSomeTruth 50 "register" $
		let env = takeThenSeq 7 shallowRst env
		    def = 1
		    inp = toSeq $ cycle [0..3]
		 in example (examine "register" tst) .*. env .*. def .*. inp

	testReify "register" tst		
	dumpBitTrace "exam/" 20

	mkTestbench [] [] "register" tst

--	print (test (toSeq [1..15]))
--	dumpBitTrace "exam/" 20

--test :: Seq U4 -> Seq U4
--test x = liftS1 (snd . unpack)  . examine "map" fut . liftS1 (\ v -> pack (true, pack (v,v*v))) $ x

-- device under test
dut :: Seq (Pipe U4 U4) -> Seq (Enabled U4)
dut s = pack (e,a `xor` d)
  where	(e,p) = unpack s
	(a,d) = unpack p

{-
 - 1. run test, with observations on.

-}