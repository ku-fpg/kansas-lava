{-# LANGUAGE ScopedTypeVariables,TypeFamilies, UndecidableInstances, FlexibleContexts, FlexibleInstances, ExistentialQuantification #-}
import Language.KansasLava -- hiding (head)

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
import Language.KansasLava.Stream as Seq
import Debug.Trace
import qualified Data.List as List

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
	wireType _ = U 4 -- TODO: derving this

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

inc :: Comb DecodeCntl -> Comb DecodeCntl
inc = funMap (return . incDecodeCntrl)

-- to move
stateMachine :: forall a . (Show a, RepWire a, Size (WIDTH a), Enum (WIDTH a), Wire a) => (a -> a) -> Seq (Enabled a) -> Seq a
stateMachine fn enSig = now
  where
	(en,sig) = unpack enSig :: (Seq Bool, Seq a)
	now = mux2 en (sig,latch fn')

	fn' = funMap (return . fn) now

boot :: Seq (Enabled DecodeCntl)
boot = toSeqX $ (pureX True, pureX (DecodeLoad 0))
	      : repeat (pureX False, optX (Nothing :: Maybe DecodeCntl))
	



decode :: forall a x . 
         (a ~ OurFloat, Size x, Num x, Enum x, x ~ X4) 
       => Seq SysEnv
        ->
	  ( Seq DecodeCntl
	  , Seq (Pipe x a)		-- lambda (in)
	  , Seq (Pipe x a)		-- global (in, share)
	  ) ->
	  ( Seq (Pipe x a)		-- local (out, share)
	  , Seq (Pipe x a)		-- lambda (out)
	  , Seq Bool			-- status
	  )
decode tm (cntl, inp, glob) = ( loc, out, okay)
   where
	in_mem :: Memory x a
	in_mem = pipeToMemory tm inp

	out_mem = in_mem

	okay = high
	
	in_enabled = fullEnabled cntl $ \ e -> 
			case e of
			   DecodeLoad x -> return x
			   _ -> Nothing

	out_enabled = fullEnabled cntl $ \ e -> 
			case e of
			   DecodeResult x -> return x
			   _ -> Nothing
			 

	out :: Seq (Pipe x a)
	out = memoryToPipe out_enabled out_mem

	loc :: Seq (Pipe x a)
	loc = pureS ( False, (0,0))

type OurFloat = Sampled X8 X8

main = print "Hello"

-------------------------
testDecode = truthTable 
	(example decode' .*. sysEnv .*. cntr .*. inp .*. glob)
   where
	decode' e c i g = decode e (c,i,g)
	cntr = stateMachine (incDecodeCntrl) boot
	inp  = toSeq'
	 [ return (True,(0,1))
	 , return (True,(1,2))
	 , return (True,(2,3))
	 , return (True,(3,4))
	 , Nothing
	 , Nothing
	 , Nothing
	 , Nothing
	 , Nothing
	 , Nothing
	 , Nothing
	 ] 
	glob = pureS (False, (0,0))
	

-- How to test
pip :: Seq (Pipe Word8 Int)
pip = toSeq'
	 [ return (True,(0,99))
	 , return (True,(1,99))
	 , return (True,(2,99))
	 , return (True,(3,99))
	 , Nothing
	 , return (True,(0,100))
	 , return (True,(1,99))
	 , return (True,(0,99))
	 ] 
ff :: Seq Word8
ff = toSeq (repeat 0)

r :: Memory Word8 Int
r = pipeToMemory sysEnv pip

{-
-- Testing ideas
data FN b = forall a . (RepWire a) => FN (Seq a -> b) (Seq a)

data FN' b = forall a . (RepWire a) => FN' b (Seq a)

data FN2 b = FN2 b [FNA]
	deriving Show

data FNA = forall a . (Show a, RepWire a) => FNA (Seq a)

----------------------------------------------------------------------------------
data Example a = Example a [ExampleArg]

data ExampleArg = forall a . (Show a, RepWire a) => ExampleArg (Seq a)

--pureEF :: a -> Example a
--(<*>) :: Example (a -> b) -> Example a -> Example b

example :: a -> Example a
example a = Example a []

class TestArg a where
	testArg :: a -> [ExampleArg]

instance (Show a, RepWire a) => TestArg (Seq a) where
	testArg a = [ExampleArg a]

infixl 2 .*.

(.*.) :: TestArg a => Example (a -> b) -> a -> Example b
(.*.) (Example f args) arg = Example (f arg) (args ++ testArg arg)

test =  example xor2 .*. low .*. high

class Testable a where
	truthTable :: a -> [TTL]
	

instance (RepWire a) => Testable (Seq a) where
	truthTable sq = [ ResV  v | v <- showStreamList sq ]
	
instance (RepWire a) => Testable (Comb a) where
	truthTable c =  [ ResV $ showRepWire (undefined :: a) (combValue c) ]

instance (Testable b) => Testable (Example b) where
	truthTable (Example fn (ExampleArg arg:args)) 
		= [ SeqValue s t | (s,t) <- zip ss tt ]
	   where
		ss = showStreamList arg
		tt = truthTable (Example fn args)
	truthTable (Example fn [])
		= truthTable fn

instance (Enum (WIDTH w), Size (WIDTH w), RepWire w, Testable b) => Testable (Comb w -> b) where
	truthTable fn = [ CombValue (showRepWire (undefined :: w) a)
				    (truthTable (fn (shallowComb a)))
		 	| a <- args
		        ]
            where
		reps :: [Matrix (WIDTH w) Bool]
		reps = allWireReps
		args0 :: [Maybe w]
		args0 = [ toWireRep rep | rep <- allWireReps ]
		args = map optX (args0 ++ [Nothing])
		
		-- map optX [Just True, Just False, Nothing ]
{-
		 case (truthTable args, truthTable (fn arg)) of
		    (ResLine vals, ResLine res) -> SeqLine [ (v,ResLine [r])
					                   | (v,r) <- zip vals res
						           ]
		    _ -> error "XX"
-}
allWireReps :: forall width . (Enum width, Size width) => [Matrix width Bool]
allWireReps = [U.toMatrix count | count <- counts ]
   where
	counts :: [Unsigned width]
	counts = [0..2^(fromIntegral sz)-1]
	sz = size (error "allWireRep" :: width)
		


data TTL = CombValue String [TTL]
	 | SeqValue  String TTL
	 | ResV      String		-- what about multiple results?
	deriving Show


{-
showTT :: TTL -> String
showTT (CombLine xs) = "CombLine " ++ show xs
showTT (SeqLine xs)  = "SeqLine " ++ show (take 10 xs)
showTT (ResLine xs)  = show xs
-}
----------------------------------------------------------------------------------

instance Show FNA where
	show (FNA x) = show x

fn :: fn -> FN2 fn
fn f = FN2 f []

--infixl `app`
app' :: (RepWire a, Show a) => FN2 (Seq a -> b) -> Seq a -> FN2 b
app' (FN2 f args) arg = FN2 (f arg) (args ++ [FNA arg])

{-
class APP a where
	app :: FNX a b -> Seq a -> FN2 b
-}
--test = fn xor2 `app` low `app` high

data FNC a b = FNC (Comb a -> b) [X a]

ex = FN bitNot (toSeq [True,False,True,False,True])
ex2 = FNC bitNot (map ( optX . Just ) [True,False,True,False,True])
ex3 = FN xor2 (toSeq [True,False,True,False,True])

--ex2 = FN (FN xor2 (toSeq [True,False,True,False,True]))
--		  (toSeq [False,True,True,False])



{-
generateTT :: FN Bool Bool -> TT
generateTT (FN fn arg) = List.transpose [ argS, resS ]
  where
	argS = showStreamList arg
	resS = showStreamList (fn arg)
-}
{-
class Testable a where
	truthTable :: a -> TT

instance (RepWire a) => Testable (Seq a) where
	truthTable sq = ResLine  $ showStreamList sq
	
instance (RepWire a) => Testable (Comb a) where
	truthTable c = ResLine [showRepWire (undefined :: a) (combValue c)]

instance (RepWire a, Testable b) => Testable (FNC a b) where
	truthTable (FNC fn args) = CombLine
 		 [ (showRepWire (undefined :: a) a, truthTable (fn (shallowComb a)))
		 | a <- args
		 ]		

instance (Testable b) => Testable (FN b) where
	truthTable (FN fn arg) = 
		 case (truthTable arg, truthTable (fn arg)) of
		    (ResLine vals, ResLine res) -> SeqLine [ (v,ResLine [r])
					                   | (v,r) <- zip vals res
						           ]
		    _ -> error "XX"

{-
	truthTable (FNC fn args) = resTT
          where resTT = 
		 [ a ++ res
		 | a <- args
		 , 
		 | (a,res) <- zip (truthTable arg)
				  (truthTable (fn arg))
		 ]		
		
-}		

instance (Testable b) => Testable (Comb Bool -> b) where
	truthTable fn = truthTable (FNC fn args)
            where
		args = map optX [Just True, Just False, Nothing ]
-}

-}

	
