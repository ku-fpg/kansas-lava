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
import Data.Bits
import Data.Word
import Data.Default
import Data.Sized.Ix
import Data.Sized.Matrix
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
	
	
main3 :: IO ()
main3 = do
	let rd_ready = toSeq $ cycle [True,False,False]
--	let wr_data  = toSeq $ concat [ [Just x{-,Nothing-}] | x <- [(0xf::X100) .. ]]

	print rd_ready
--	print wr_data
	let Handshake inp = toHandshake' (cycle [1..4]) [Just x | x <- [(0x0::X100) .. ]]

	let out rd_ready = rd_data
	       where
	    	(wr_ready,rd_data,_,debug) = fifo4 (witness :: SZ)
						   shallowEnv (rd_ready,wr_data)
	    	wr_data = inp wr_ready


	
--	print $ fromHandshake' (cycle [1..5]) (Handshake out)
	print $ take 100 [ x | Just x <- fromHandshake' (cycle $ reverse [6..10]) (Handshake out) ]
--	print ("rd_data",take 20 $ fromSeq $ rd_data)
--	putStrLn debug



	return ()

-- liftEnable :: (Env () -> Enabled a -> Enabled b) -> Handshake a -> Handshake b

type SZ = SUB C X1
type C = X2

-- First be specific
fifo4 :: forall a counter ix . 
         (Size counter, Size ix, ix ~ SUB counter X1, counter ~ ADD ix X1, Rep a, Rep counter, Rep ix, Num counter, Num ix) 
      => ix
      -> Env () -> (Seq Bool,Seq (Enabled a)) -> (Seq Bool,Seq (Enabled a),Seq counter,String)
fifo4 _ env (out_ready,inp) = (inp_ready,out,in_counter,debug)
  where
	debug :: String
	debug = unlines
	      [ show ("in_counter",in_counter)
	      , show ("out_counter0",out_counter0)
	      , show ("wr_addr",wr_addr)
	      , show ("rd_addr0",rd_addr0)
	      , show ("out_done0",out_done0)
	      ]

	mem :: Seq ix -> Seq a
	mem = pipeToMemory env env wr

	inp_done0 :: Seq Bool
	inp_done0 = inp_ready `and2` isEnabled inp

	inp_done1 :: Seq Bool
	inp_done1 = register env false 
		  $ inp_done0
		
	inp_done2 :: Seq Bool
	inp_done2 = register env false 
		  $ inp_done1

	wr :: Seq (Enabled (ix,a))
	wr = packEnabled (inp_ready `and2` isEnabled inp)
			 (pack (wr_addr,enabledVal inp))

	wr_addr :: Seq ix
	wr_addr = register env 0
		$ mux2 inp_done0 (wr_addr+1,wr_addr)

	rd_addr0 :: Seq ix
	rd_addr0 = mux2 out_done0 (rd_addr1+1,rd_addr1)

	rd_addr1 = register env 0 rd_addr0

	out_done0 :: Seq Bool
	out_done0 = out_ready `and2` (isEnabled out)

	out_done1 :: Seq Bool
	out_done1 = register env false 
--		  $ register env false 
		  $ out_done0

	in_counter :: Seq counter
	in_counter = register env 0 
		$ in_counter + mux2 inp_done0 (1,0)
			     - mux2 out_done0 (1,0)

	out_counter0 :: Seq counter
	out_counter0 = out_counter1
			      + mux2 inp_done2 (1,0)
		 	      - mux2 out_done0 (1,0)

	out_counter1 = register env 0 out_counter0
	
	out_counter2 = register env 0 out_counter1

	out :: Seq (Enabled a)
	out = packEnabled (out_counter1 .>. 0) (mem rd_addr0)

	inp_ready :: Seq Bool
	inp_ready = in_counter .<. (fromIntegral (size (witness :: ix)))


