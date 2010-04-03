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

test :: Seq U4 -> Seq U4
test x = liftS1 (snd . unpack)  . examine "map" fut . liftS1 (\ v -> pack (true, pack (v,v*v))) $ x

fut :: Seq (Pipe U4 U4) -> Seq (Enabled U4)
fut s = pack (e,a `xor` d)
  where	(e,p) = unpack s
	(a,d) = unpack p

fib n = if n < 2 then 1 else fib (n-1)+fib(n-2)

main :: IO ()
main = do
	print (test (toSeq [1..15]))

	mkTestbench [] [] "XX" fut

	dumpBitTrace "exam/" 20

	
