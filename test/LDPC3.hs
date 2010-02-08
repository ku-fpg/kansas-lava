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


--				      (fromWireRep x :: Matrix (WIDTH x) Bool)) :: Matrix (WIDTH (DecodeCntl x)) Bool
	
--
--		((M.matrix [False,False] :: Matrix X2 Bool) `M.append` pure False) :: Matrix (ADD X2 (WIDTH x)) Bool

main = print "Hello"