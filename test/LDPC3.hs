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



type SZ = X4

data DecodeCntl 
	= DecodeWait		-- 0 + 0
	| DecodeRst		-- 0 + 1	restart
	| DecodeRest		-- 0 + 2
	| DecodeLoad X4		-- 1 + x
	| DecodeShare X4	-- 2 + x
	| DecodeResult X4	-- 3 + x
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

