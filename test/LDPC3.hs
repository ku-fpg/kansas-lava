{-# LANGUAGE ScopedTypeVariables,TypeFamilies, UndecidableInstances #-}
import Language.KansasLava hiding (head)

--import Language.KansasLava.Applicative

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Sampled
import Data.Sized.Arith
import Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Data.Bits
import Control.Applicative
import Data.Maybe  as Maybe
import Language.KansasLava
import Data.Word
import Language.KansasLava.Seq as Seq
import Debug.Trace

type SZ = X4

data DecodeCntl 
	= DecodeWait		-- 0 + 0
	| DecodeRst		-- 0 + 1	restart
	| DecodeRest		-- 0 + 2
	| DecodeLoad SZ		-- 1 + x
	| DecodeShare SZ	-- 2 + x
	| DecodeResult SZ	-- 3 + x
	deriving (Show, Eq, Ord)

instance Wire DecodeCntl where
	type X DecodeCntl = WireVal DecodeCntl	-- choice: top level failure
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire DecodeCntl"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire DecodeCntl"
	wireName _ = "DecodeCntl"
	wireType _ = U (undefined)

opCode :: Integer -> X4 -> Matrix (ADD X2 (WIDTH X4)) Bool
opCode n x = 
	(U.toMatrix (fromIntegral n)  :: Matrix X2 Bool)
		`append`
	(fromWireRep x)


instance RepWire DecodeCntl where	
	type WIDTH DecodeCntl = ADD X2 (WIDTH X4)

	-- encoding
	fromWireRep DecodeWait       = opCode 0 0
	fromWireRep DecodeRst        = opCode 0 1
	fromWireRep DecodeRest       = opCode 0 2
	fromWireRep (DecodeLoad x)   = opCode 1 x
	fromWireRep (DecodeShare x)  = opCode 2 x
	fromWireRep (DecodeResult x) = opCode 3 x


	toWireRep n = do op <- toWireRep ((n `cropAt` 0) :: Matrix X2 Bool) :: Maybe X4
			 x <- toWireRep ((n `cropAt` 2) :: Matrix X2 Bool) :: Maybe X4
		         case (op,x) of
			    (0,0) -> return $ DecodeWait
			    (0,1) -> return $ DecodeRst
			    (0,2) -> return $ DecodeRest
			    (1,x) -> return $ DecodeLoad x
			    (2,x) -> return $ DecodeShare x
			    (3,x) -> return $ DecodeResult x
			    _ -> Nothing	
	showRepWire _ = show

incDecodeCntrl :: DecodeCntl -> DecodeCntl
incDecodeCntrl DecodeWait = DecodeWait
incDecodeCntrl DecodeRst  = DecodeLoad minBound
incDecodeCntrl (DecodeLoad n) 
	| n < maxBound 	= DecodeLoad (succ n)
	| otherwise	= DecodeShare minBound
incDecodeCntrl (DecodeShare n) 
	| n < maxBound  = DecodeShare (succ n)
	| otherwise	= DecodeResult minBound
incDecodeCntrl (DecodeResult n) 
	| n < maxBound  = DecodeResult (succ n)
	| otherwise	= DecodeWait
incDecodeCntrl (DecodeRest) = DecodeRest

inc :: K DecodeCntl -> K DecodeCntl
inc = funMap (return . incDecodeCntrl)

-- to move
stateMachine :: forall a . (Show a, RepWire a, Size (WIDTH a), Enum (WIDTH a), Wire a) => (a -> a) -> Signal (Enabled a) -> Signal a
stateMachine fn enSig = now
  where
	(en,sig) = unpack enSig :: (Signal Bool, Signal a)
	now = mux2 en (sig,latch fn')

	fn' = funMap (return . fn) now

boot :: Signal (Enabled DecodeCntl)
boot = shallowSignal $ Seq.fromList
 		     $ [ (optX (Just True), optX (Just $ DecodeLoad 0)) ]
			++ repeat (optX (Just False), optX (Nothing :: Maybe DecodeCntl))

{-
gtg :: Signal Bool
gtg = shallowSignal $ Seq.fromList (repeat $ optX $ Just True)

incs :: Signal Int
incs = shallowSignal $ Seq.fromList [ optX $ Just n | n <- [0..] :: [Int]]
				
xx :: Signal (Int,Int)
xx = shallowSignal $ Seq.fromList $ repeat (optX (Nothing :: Maybe (Int,Int)))

mux2' :: forall sig a . (SIGNAL sig, sig ~ Signal, Wire a) => sig Bool -> (sig a,sig a) -> sig a
mux2' i (t,e)
	= liftS3 kFn i t e

kFn :: forall a . (Wire a) => K Bool -> K a ->  K a -> K a
kFn = \ ~(K i ei) 
        ~(K t et)
        ~(K e ee) -> 
		K (case unX i :: Maybe Bool of
			          Nothing -> optX (Nothing :: Maybe a)
				  Just True -> t
				  Just False -> e
			     ) (error "XX")


x = liftS3 kFn gtg incs (delay sysEnv (0 :: K Int) x)

liftS3' :: (K a -> K b -> K c -> K d) -> Signal a -> Signal b -> Signal c -> Signal d
liftS3' f ~(Signal a ea) ~(Signal b eb) ~(Signal c ec) = Signal (pure f' <*> a <*> b <*> c) ex
      where
	K _ ex = f (deepK ea) (deepK eb) (deepK ec)
	f' a b c = let ~(K x _) = f (shallowK a) (shallowK b) (shallowK c)
	           in x
-}

{-

decode :: forall a x . 
         (a ~ OurFloat, Size x, Num x, Enum x, x ~ X4) 
       =>  x 
        -> Signal SysEnv
        ->
	  ( Signal (DecodeCntl x)
	  , Pipe x a		-- lambda (in)
	  , Pipe x a		-- global (in, share)
	  ) ->
	  ( Pipe x a		-- local (out, share)
	  , Pipe x a		-- lambda (out)
	  )
decode x tm (cntl, inp, glob) = ( loc, out )
   where
	-- function, remember
	in_mem = pipeToMem tm inp

	out_mem = in_mem
	
	out_enabled = fullEnabled cntl $ \ e -> 
			case e of
			   DecodeResult x -> return x
			   _ -> Nothing
			 

	out :: Pipe x a
	out = memToPipe tm out_enabled out_mem
	() = ()
	loc = ( (pureD False, pureD 0), pureD 0)

type OurFloat = Sampled X8 X8
-}

main = print "Hello"

-------------------------


-- How to test
pip :: Signal (Pipe Word8 Int)
pip = shallowSignal $ Seq.fromList $ Prelude.map (optX :: Maybe (Enabled (Word8,Int)) -> X (Enabled (Word8,Int)))
	 [ return (True,(0,99))
	 , return (True,(1,99))
	 , return (True,(2,99))
	 , return (True,(3,99))
	 , Nothing
	 , return (True,(0,100))
	 , return (True,(1,99))
	 , return (True,(0,99))
	 ] ++ repeat (optX (Nothing :: Maybe (Pipe Word8 Int)))

ff :: Signal Word8
ff = shallowSignal $ Seq.fromList $ repeat (optX (Just 0 :: Maybe Word8) :: X Word8)

r :: Memory Word8 Int
r = pipeToMemory sysEnv pip

