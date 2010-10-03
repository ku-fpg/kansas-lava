{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module FSL where
-- Use %ghci FSL.hs -i..
-- To load

import Language.KansasLava
import Language.KansasLava.Stream
import Data.Sized.Unsigned
import Debug.Trace
import Data.Maybe as Maybe
import System.IO.Unsafe
import Control.Concurrent.MVar
import System.IO
import Control.Concurrent

----------------------------------------------------------------------------------------------------
-- Shallow Consumers


-- A test, that takes the output from our FSL generator,
-- and returns the back edge (the IsRead, and the values)
testFSLRead :: Env () -> Seq (Enabled Int) -> (Seq IsRead,Seq Int)
testFSLRead env sq = (mkIsRead low,res)
  where (en,val) = unpack sq
        res = register env 0 $ mux2 en (res+1,res)

-- The DUT for the testFSLRead, that includes the backedge/looping.
dut env generator = result
   where
	out1             = generator readSig
	(readSig,result) = testFSLRead env out1
	
{-
dut2 env generator = result
   where
	out1             = generator readSig
	(readSig,result) = testFSLRead env out1	
-}

-- Our main test
main = do
--	print (dut shallowEnv (toVariableSrc [1..] [1..10]))
	t <- reifyCircuit s2
	return ()
	t' <- optimizeCircuit (OptimizationOpts 0) t
--	print t'
	writeDotCircuit "x.dot" t'
	writeVhdlCircuit [] "x" "x.vhd" t'

--t1 = fromVariableSink (repeat 1000) (fmapSink (*3) sink0)
--  where src0 = toVariableSrc  (repeat 1000) ([1..]::[Int])
--	sink0 = srcToSink shallowEnv (fmapSrc (*2) src0)


	


-- A one-cell mvar-like FIFO.


--				Seq (IsFull -> Enabled a)	



mvar :: (Rep a) => Env () -> (Seq IsFull -> Seq (Enabled a)) -> (Seq IsRead -> Seq (Enabled a))
mvar env f isRead = value
  where 
	value = register env (pureS Nothing) $
			cASE [ ( (state .==. low) `and2` (isEnabled inp)
			       , inp		-- load new value
			       )
			     , ( (state .==. low) `and2` (bitNot (isEnabled inp))
			       , pureS Nothing	-- load no value
			       )
			     , ( (state .==. high) `and2` (isRead .==. pureS (IsRead True))
			       , pureS Nothing	-- output value
			       )
			     , ( (state .==. high) `and2` (isRead .==. pureS (IsRead False))
			       , value		-- keep value
			       )
			     ] (pureS Nothing)
	inp 	      = f $ mkIsFull state
	-- the state is just a bit
	(state,datum) = unpack value



--mux2 (funMap fsm $ pack (isFull,isRead)) (value,inp)
--	      inp        = f $ mkIsFull full
--	      (full,val) = unpack value
--	      fsm :: (IsFull,IsRead) -> Maybe Bool
--	      fsm = (IsFull True,IsRead True) = return False	-- 


cASE :: (Rep b, Signal seq) => [(seq Bool,seq b)] -> seq b -> seq b
cASE [] def = def
cASE ((p,e):pes) def = mux2 p (e,cASE pes def)

unsafeUnapply :: a -> (b, b -> a)
unsafeUnapply a = unsafePerformIO $ do
	v <- newEmptyMVar
	let f b = unsafePerformIO $ do
			putMVar v b
			return a
	let b = unsafePerformIO $ do
			takeMVar v
	return $ (b,f)


-- This is how to use the new ports stuff
testX = do
	print "This *should* hang, but check `XX`"
	v1 <- newShallowFIFO
	v2 <- newShallowFIFO
	forkIO $ readFileToFIFO "FSL.hs" v1
	forkIO $ writeFileFromFIFO "XXX" v2
	
	src <- fifoToSrc v1
	let circ = fmapSrc (toStdLogicVector . ((+0) :: Comb U8 -> Comb U8). fromStdLogicVector) src
	sinkToFifo v2 (srcToSink shallowEnv circ)
	

{-
-- Err, how does this finish/close? There is no end of file!
writeFileWith :: (c ~ StdLogicVector X8) => String -> (r -> [c]) -> r -> IO ()
writeFileWith str toIn st = do
	let vals :: [Maybe U8]
	    vals = map fromSLV $ toIn st
	print vals
-}

--	return ()
{-	str <- readFile str
	let vals = map Char.ord str
	print vals
	return (toOut $ map fromIntegral vals)	
-}	
