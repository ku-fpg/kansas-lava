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
import Data.Word
import Data.Default

----------------------------------------------------------------------------------------------------
-- Shallow Consumers

circuit :: Comb Byte -> Comb Byte
circuit inpB = outpB
  where
	inp :: Comb U8
	inp = fromStdLogicVector inpB

	outp :: Comb U8
	outp = inp * 37

	outpB :: Comb Byte
	outpB = toStdLogicVector outp

big_circuit :: Env () -> Src Byte -> Sink Byte
big_circuit env src = srcToSink env $ fmapSrc circuit src


main = do
	print "This *should* hang, but check the file `LAVA_OUT`"
	v1 <- newShallowFIFO
	v2 <- newShallowFIFO
	forkIO $ readFileToFIFO "FSL.hs" v1
	forkIO $ writeFileFromFIFO "LAVA_OUT" v2
	
	src <- fifoToSrc v1
	sinkToFifo v2 (srcToSink shallowEnv (fmapSrc circuit src)) 

-- Our other test
main2 = do
--	print (dut shallowEnv (toVariableSrc [1..] [1..10]))
	t <- reifyCircuit big_circuit
	return ()
	t' <- optimizeCircuit def t
	writeDotCircuit "x.dot" t'
	writeVhdlCircuit [] "x" "x.vhd" t'

