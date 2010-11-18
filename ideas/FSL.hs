{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

module Main where
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
import Data.Sized.Arith
import Data.Sized.Matrix
import Language.KansasLava.Handshake

import Language.KansasLava.Testing.Bench
import Language.KansasLava.Testing.Thunk
import Language.KansasLava.Testing

import Data.List as List
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

big_circuit ::  Handshake Byte -> Handshake Byte
big_circuit src = fmap (error "" $ circuit) src

main1 = do
	print "This *should* hang, but check the file `LAVA_OUT`"
	v1 <- newShallowFIFO
	v2 <- newShallowFIFO
	forkIO $ readFileToFIFO "FSL.hs" v1
	forkIO $ writeFileFromFIFO "LAVA_OUT" v2
	
	src <- shallowFifoToHandShake v1
	handShakeToShallowFifo v2 (big_circuit src)

-- Our other test
main2 = do
	t <- reifyCircuit big_circuit
	return ()
	t' <- optimizeCircuit def t
	writeDotCircuit "x.dot" t'
	writeVhdlCircuit [] "x" "x.vhd" t'
	


--pairFIFO ::  HandShake U8 -> HandShake U4
--pairFIFO env hs (


	
main3 :: IO ()
main3 = do
	let rd_ready = toSeq $ cycle [True,False,False]
--	let wr_data  = toSeq $ concat [ [Just x{-,Nothing-}] | x <- [(0xf::X100) .. ]]

	print rd_ready
--	print wr_data
	let hs = toHandShake' (cycle [0]) [Just (fromIntegral x) | x <- [(0x0::Integer) .. ]]

	let hs2 = fifo (witness :: X16)
		       (toSeq ((take 10 $ repeat False) ++ (take 1 $ repeat True) ++ (take 100 $ repeat False)))
		       hs


{-
	let thu :: Thunk (Seq Int)
	    thu = Thunk (fifo' (witness :: X16) ::  (Seq Bool,Seq (Enabled Byte)) -> (Seq Bool,Seq (Enabled Byte),Seq X17))
			(\ f -> {-let (wr_ready, rd_data, _) = f shallowEnv (rd_ready,inp wr_ready)
				in-} 0 --  pack (wr_ready,rd_data :: Seq (Enabled Byte))
			)
n-}			
	
	
--	print $ fromHandshake' (cycle [1..5]) (Handshake out)
	let xs = take 100 [ x | x <- fromHandShake' (repeat 0) (hs2) ] :: [Maybe U8]
	print $ xs
	print $ List.nub xs
--	print ("rd_data",take 20 $ fromSeq $ rd_data)
--	putStrLn debug

--	runDeep "fifo" 1000 (Thunk (
	
{-	
	        -> Int                 -- ^ Number of cycles to simulate.
        -> Thunk b
        -> (Circuit -> IO Circuit) -- ^ any operations on the circuit before VHDL generation
        -> (FilePath -> IO ()) -- ^ Invocation function, given a path to the testbench and charged with actually executing the test. Can assume path exists.
        -> IO ()
-}

	return ()
{-
main4 = do
	let cir ::  Seq Bool -> HandShake (Seq (Enabled Byte)) -> HandShake (Seq (Enabled Byte))
	    cir = fifo (witness :: X32)

	let cir ::  Seq Bool -> (Seq X33, Seq (Enabled Byte)) -> HandShake ((Seq X32, Seq Bool), Seq (Enabled Byte))
	    cir = fifoBE (witness :: X32)
	
	c0 <- reifyCircuit cir
	cOpt <- optimizeCircuit def c0 
	writeVhdlCircuit [] "myfifo" "myfifo.vhdl" cOpt
	writeDotCircuit "x.dot" cOpt

--	mkTestbench "myfifo" "testme" cOpt
-}
-- liftEnable :: ( Enabled a -> Enabled b) -> Handshake a -> Handshake b

--type SZ = SUB C X1
--type C = X2
{-
-- First be specific
fifo4 :: forall a counter ix . 
         (Size counter
	, Size ix
	, counter ~ ADD ix X1
	, Rep a
	, Rep counter
	, Rep ix
	, Num counter
	, Num ix
	) 
      => ix
      -> Env () 
      -> (Seq Bool,Seq (Enabled a)) 
      -> (Seq Bool,Seq (Enabled a),Seq counter,String)
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

-}

main = handShakeLambdaBridge $ \ hs -> HandShake $ \ _ -> (toSeq [Just (fromIntegral x) | x <- [1..]])

{-

foo1 :: forall a d a1 a2 . (StdLogic a, StdLogic a1, StdLogic a2, WIDTH a ~ ADD (WIDTH a1) (WIDTH a2), Rep d, Rep a, Rep a1, Rep a2) 
	=> Seq (Pipe a d) -> Seq (Pipe (a1,a2) d)
foo1 = mapEnabled (mapPacked f)
   where f :: (Comb a,Comb d) -> (Comb (a1,a2),Comb d)
         f (a,d) = (factor a,d)

foo1' = foo1 :: Seq (Pipe X8 U8) -> Seq (Pipe (X2,X4) U8)


foo2 :: forall a a1 a2 d . (Rep a1, Rep a2, Rep d, Size a1) => Seq (Pipe (a1,a2) d) -> Matrix a1 (Seq (Pipe a2 d))
foo2 inp = forAll $ \ i -> let (g,v)   = unpackEnabled inp
			       (a,d)   = unpack v
			       (a1,a2) = unpack a
			   in packEnabled (g .&&. (a1 .==. pureS i))
					  (pack (a2,d))

foo2' = foo2 :: Seq (Pipe (X2,X4) U8) -> Matrix X2 (Seq (Pipe X4 U8))

solution :: forall a a1 a2 d .
     (WIDTH a ~ ADD (WIDTH a1) (WIDTH a2),
      Rep a,
      Rep a1,
      Rep a2,
      Rep d,
      Size a1,
      StdLogic a,
      StdLogic a1,
      StdLogic a2,
      StdLogic d) =>
     Seq (Pipe a d) -> Seq a2 -> Seq (Matrix a1 d)
solution inp look = id
	 $ pack
	 $ fmap (\ f -> f look)
	 $ fmap pipeToMemory
	 $ foo2
	 $ foo1
	 $ inp

solution' = solution :: Seq (Pipe X8 U1) -> Seq X1 -> Seq (Matrix X8 U1)
-}


{-
main10 = do
	x <- reifyCircuit solution'
	x' <- optimizeCircuit def x
	writeDotCircuit "x.dot" x'
-}

type DX = U1

main11 = do
	let hs :: HandShake (Seq (Enabled DX))
	    hs = toHandShake' (cycle [0,1,2]) 
			      (cycle  [Just (if testBit x i then 1 else 0)
				      | x <- [0..] :: [Word8]
			      , i <- [0..7]
				      ])

	let hs1 :: HandShake (Seq (Enabled (Matrix X8 U1)))
	    hs1 = fifoToMatrix (witness :: X16) (witness :: X2) (toSeq $ repeat False) hs


   	let hs2 :: HandShake (Seq (Enabled Byte))
	    hs2 = fmap (mapEnabled (liftS1 (f . mapPacked (fmap (\ x -> x .==. 1))))) hs1
	
	    f :: Comb (Matrix X8 Bool) -> Comb Byte
	    f = toStdLogicVector


--	let hs' :: HandShake (Seq (Enabled DX))
--	    hs' = fifo (witness :: X8) (toSeq $ repeat False) hs

	print $ fromHandShake' (cycle [0,1,2]) hs2
	return ()


