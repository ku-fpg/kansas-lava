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
	| DecodeLoad (Unsigned SZ)		-- 1 + x
	| DecodeShare (Unsigned SZ)	-- 2 + x
	| DecodeResult (Unsigned SZ)	-- 3 + x
	deriving (Show, Eq, Ord)

instance Wire DecodeCntl where
	type X DecodeCntl = WireVal DecodeCntl	-- choice: top level failure
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire DecodeCntl"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire DecodeCntl"
	wireName _ = "DecodeCntl"
	wireType _ = U (undefined)


opCode :: Integer -> Unsigned X4 -> Matrix X6 Bool
opCode n x = 
	(U.toMatrix (fromIntegral n)  :: Matrix X2 Bool)
		`append`
	(fromWireRep x)

instance RepWire (DecodeCntl) where	
	type WIDTH (DecodeCntl) = X6


	

	-- encoding
--	fromWireRep DecodeWait     = matrix 
--	fromWireRep DecodeRst      = U.toMatrix $ fromIntegral $ 0 + 1 * 4
--	fromWireRep DecodeRest     = U.toMatrix $ fromIntegral $ 0 + 2 * 4
--	fromWireRep (DecodeLoad x) = opCode 0 (fromWireRep x)

--				      (fromWireRep x :: Matrix (WIDTH x) Bool)) :: Matrix (WIDTH (DecodeCntl x)) Bool
	
--
--		((M.matrix [False,False] :: Matrix X2 Bool) `M.append` pure False) :: Matrix (ADD X2 (WIDTH x)) Bool

main = print "Hello"