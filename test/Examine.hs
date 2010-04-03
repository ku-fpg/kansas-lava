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


{-# NOINLINE eChan #-}

eChan :: Chan (String, Dynamic)
eChan = unsafePerformIO $ newChan


--runE :: IO () -> IO ()
--runE m = do
--	m

type Seq' a = [a]

{-# NOINLINE examine #-}
examine :: forall a b . (Wire a, Wire b, Typeable a, Typeable b) => String -> (Seq a -> Seq b) -> (Seq a -> Seq b)
examine nm fn arg = unsafePerformIO $ do
	print $ "debugging " ++ show nm
	let res = fn arg
	writeChan eChan (nm,toDyn (pack (arg,res) :: Seq (a,b)))
	return $ res
		
deriving instance Typeable1 Seq
deriving instance Typeable1 Unsigned

test :: Seq U4 -> Seq U4
test x = liftS1 (snd . unpack)  . examine "map" fut . liftS1 (\ v -> pack (true, pack (v,v*v))) $ x

fut :: Seq (Pipe U4 U4) -> Seq (Enabled U4)
fut s = pack (e,a `xor` d)
  where	(e,p) = unpack s
	(a,d) = unpack p

fib n = if n < 2 then 1 else fib (n-1)+fib(n-2)


data Watch = forall a b. (RepWire a, Typeable a, RepWire b, Typeable b) =>  Watch String (Seq (a,b))

watcher :: [Watch] -> IO ()
watcher watch = do
	let loop n = do
		ok <- isEmptyChan eChan
		if ok then return () else do
			(nm,a) <- readChan eChan
			putStrLn $ "Considering 'examine " ++ show nm ++ "'"
			case [ w | w@(Watch nm' _) <- watch, nm == nm' ] of
			    [Watch _ a'] -> writeBitfilePair (nm ++ "_" ++ show n) 20 (fromDyn a a')
--	                    [] -> error $ "can not find watcher for "
	
	loop 0

main :: IO ()
main = do
	print (test (toSeq [1..15]))

	mkTestbench [] [] "XX" fut

	watcher [Watch "map" (error "XXX" :: Seq (Pipe U4 U4,Enabled U4)) ]


writeBitfilePair :: forall a b . (RepWire a, RepWire b) => String -> Int -> Seq (a,b) -> IO ()
writeBitfilePair nm count fn = do
	let (inp,outp) = unpack fn
	writeBitfile (nm ++ ".inp.bits") count inp
	writeBitfile (nm ++ ".out.bits") count outp
	print ()
	
