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
import Language.KansasLava.Handshake
-- Example

circuit :: Comb Byte -> Comb Byte
circuit inpB = outpB
  where
	inp :: Comb U8
	inp = fromStdLogicVector inpB

	outp :: Comb U8
	outp = inp -- * 37

	outpB :: Comb Byte
	outpB = toStdLogicVector outp

big_circuit :: Env () -> Handshake Byte -> Handshake Byte
big_circuit env src = liftS1 circuit src

main = do
	print "This *should* hang, but check the file `LAVA_OUT`"
	v1 <- newShallowFIFO
	v2 <- newShallowFIFO
	forkIO $ readFileToFIFO "FSL.hs" v1
	forkIO $ writeFileFromFIFO "LAVA_OUT" v2
	
	src <- fifoToHandshake v1
	handshakeToFifo v2 (big_circuit shallowEnv src)

-- Our other test
main2 = do
	t <- reifyCircuit big_circuit
	return ()
	t' <- optimizeCircuit def t
	writeDotCircuit "x.dot" t'
	writeVhdlCircuit [] "x" "x.vhd" t'

