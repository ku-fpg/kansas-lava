{-# LANGUAGE ScopedTypeVariables,TypeFamilies #-}
import Language.KansasLava hiding (head)

--import Language.KansasLava.Applicative

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Sampled
import Data.Sized.Arith
import Data.Bits
import Control.Applicative
import Data.Maybe  as Maybe
import Language.KansasLava.Memory

-- A pipe is a segmented value, pushed through a narrow pipe
type Pipe a d = (Enabled a,Signal d)

type Mem a d = Signal (MemOp a d) -> Signal d

type Enabled a = (Signal Bool, Signal a)

type Mem2 a d = Signal a -> Signal d

-- Right now, you can only read or write one at a time.
-- Fix using duel ported RAM.

pipeToMem :: (Ord a, OpType a, OpType d, Enum a, Enum d, Bounded a) => Time -> Pipe a d -> Mem a d
pipeToMem tm pipe@((en,aSig),dSig) memOp = bram [ (x,toEnum 0) | x <- [minBound .. maxBound]] tm memOp'
  where
	memOp' = mux2 en (writeMem aSig dSig)  memOp 

memToPipe ::  (OpType a, OpType d) => Time -> Enabled a -> Mem a d -> Pipe a d
memToPipe tm (en,aSig) mem = ((en, aSig) , mem (readMem aSig))

test_pipe :: Pipe X4 Int
test_pipe = ( ( with "en_pipe" $ ([False,False,False,True] ++ repeat False)
	      , with "en_addr" $ [0,0,0,0] ++ repeat 0 
	    )
	    , with "en_val" $ [0..]
	    )
	
test_mem :: Time -> Mem X4 Int
test_mem tm = pipeToMem tm test_pipe 

{-
test_memOp :: 
--memOp
   where

	memOp :: Signal (MemOp X4 Int)
        memOp = readMem (with "memOp" $ cycle [0..0])
-}

----------------------------------------------------------------------------------------

pipeToMem2 :: forall a d . (OpType a, OpType d) => Time -> Pipe a d -> Mem2 a d
pipeToMem2 (Time clk rst) ((en,addr),dat) addr2 = res


-- o0 $ entity2 (op s1 nm) defaultInputs [Var "o0"]  f s1 s2

  where 
    res :: Signal d
    res = Signal undefined (Port (Var "o0") $ E $ entity)

    entity :: Entity BaseTy E
    entity = 
	Entity (Name "Mem" "mem2") 
		[ (Var "o0",bitTypeOf res)]
		[ (Var "clk",bitTypeOf clk,signalDriver clk)
		, (Var "rst",bitTypeOf rst,signalDriver rst)
		, (Var "en",bitTypeOf en,signalDriver en)
		, (Var "addr",bitTypeOf addr,signalDriver addr)
		, (Var "dat",bitTypeOf dat,signalDriver dat)
		, (Var "addr2",bitTypeOf addr2,signalDriver addr2)
		] 
		[]

memToPipe2 ::  (OpType a, OpType d) => Enabled a -> Mem2 a d -> Pipe a d
memToPipe2 (en,aSig) mem2 = ((en, aSig) , mem2 aSig)


data DecodeCntl x
	= DecodeWait		-- 0 + 0
	| DecodeRst		-- 0 + 1	restart
	| DecodeRest		-- 0 + 2
	| DecodeLoad x		-- 1 + x
	| DecodeShare x		-- 2 + x
	| DecodeResult x	-- 3 + x
	deriving (Show, Eq, Ord)

{-
instance (Ord x, Bounded x, Enum x) => Konstant (DecodeCntl x) where
	pureD d = liftD0 $ K d (Lit (fromIntegral $ fromEnum d))
-}

allDecodeCntl :: (Bounded x, Enum x) => [DecodeCntl x]
allDecodeCntl = 
	 [DecodeWait, DecodeRst, DecodeRest ] ++
	 [DecodeLoad x | x <- [minBound .. maxBound]] ++
	 [DecodeShare x | x <- [minBound .. maxBound]] ++
	 [DecodeResult x | x <- [minBound .. maxBound]]

instance (Enum x, Bounded x) => Bounded (DecodeCntl x) where
	minBound = head allDecodeCntl
	maxBound = last allDecodeCntl

instance (Ord x, Bounded x, Enum x) => Enum (DecodeCntl x) where
    fromEnum = genFromEnum allDecodeCntl
    toEnum   = genToEnum allDecodeCntl

genFromEnum :: (Ord a) => [a] -> a -> Int
genFromEnum xs a = case lookup a [ (x,i) | (i,x) <- zip [0..] xs ] of
		      Just v -> v
		      Nothing -> error $ "opps"

genToEnum :: [a] -> Int -> a
genToEnum xs a = xs !! a

instance OpType x => OpType (DecodeCntl x) where
	op _ nm = Name "DecodeCntl" nm
	bitTypeOf a = U (max n 2 + 2)
	   where
		U n = bitTypeOf (f a)
		f :: Signal (DecodeCntl a) -> Signal a
		f = undefined
	initVal = undefined

log2 :: Int -> Int
log2 0 = 0
log2 1 = 1
log2 n = log2 (n `shiftR` 1) + 1

{-
  0 -> ???		""
  1 -> 0		0
  2 -> 1		01
  3 -> 2		
  4 -> 2		
  5 -> 3  
  6 -> 3
  7 -> 3
  8 -> 3
  9 -> 4
-}

instance OpType X0 where
	op _ nm = Name "X0" nm
	bitTypeOf a = U 0
	initVal = undefined
instance (Size x, OpType x) => OpType (X0_ x) where
	op _ nm = Name "X0_" nm -- should be compound
	bitTypeOf a = U $ log2 (sz - 1)
	   where sz = size (undefined :: (X0_ x))
	initVal = undefined
instance (Size x, OpType x) => OpType (X1_ x) where
	op _ nm = Name "X1_" nm -- should be compound
	bitTypeOf a = U $ log2 (sz - 1)
	   where sz = size (undefined :: (X1_ x))
	initVal = undefined


example :: Signal (DecodeCntl X32) -> Signal (DecodeCntl X32)
example = fullMap incDecodeCntrl 

incDecodeCntrl :: (Ord x, Bounded x, Enum x, Num x) => DecodeCntl x -> DecodeCntl x
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
--incDecodeCntrl other = error $ show other

isLoad :: (Num x) => DecodeCntl x -> Bool
isLoad (DecodeLoad x) = True
isLoad _	      = False

getLoad :: (Num x) => DecodeCntl x -> Maybe x
getLoad  (DecodeLoad x) = Just x
getLoad  _              = Nothing


fullEnabled :: (Bounded a, OpType a, Enum a, Show a, OpType b, Enum b, Show b) => Signal a -> (a -> Maybe b) -> Enabled b
fullEnabled s f = ( fullMap (Maybe.isJust . f) s
	          , fullMapWithMaybe f s
	          )


stateMachine :: (OpType a) => Time -> Enabled a -> Signal a -> (Signal a -> Signal a) -> Signal a
stateMachine tm en@(gtg,val) def loop = now
  where
	now = mux2 gtg val (delay tm def (loop now))
	brk :: Signal Bool
	brk = pureD False

{-
counter :: (OpType a) => Time -> Signal a -> (Signal a -> Signal a) -> Signal a
counter tm def loop = now
    where
	now = mux2 gtg val (delay tm def (loop now))
	brk :: Signal Bool
	brk = pureD False
-}
-- the controlling logic, that generates the various signal for other components to use
controller :: (x ~ X4) => Time -> Signal Bool -> Signal (DecodeCntl x)
controller clk en = stateMachine clk (en,pureD DecodeRst) (pureD DecodeWait) (fullMap incDecodeCntrl)
	
test1 = stateMachine clock (with "en" $ [False,False,False,False,True,False] ++ cycle [False] ,pure DecodeRst)
 	            (pureD DecodeWait) example 
test2 clk en def = stateMachine clk en def example

-- latch on the good vals, undefined until the first value has arrived.
enabledToSignal :: (OpType a) => Time -> Enabled a -> Signal a
enabledToSignal tm (gtg,sig) = sig'
  where sig' = mux2 gtg sig (latch tm sig')

decode :: forall a x . 
         (a ~ OurFloat, Size x, Num x, Enum x, x ~ X4) 
       =>  x 
        -> Time
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

test_decode_lambda_in :: (x ~ X4, a ~ OurFloat) => Pipe x a
test_decode_lambda_in = 
	( ( with "go" $ [ False, False, False ] ++ take 4 (repeat True) ++ repeat False
	  , with "ix" $ [ 0, 0, 0 ] ++ [0..3] 
	  )
	, with "val" $ [ 0, 0, 0 ] ++ [0..3]
	)

test_decode_global_in :: (x ~ X4, a ~ OurFloat) => Pipe x a
test_decode_global_in =
	( ( with "go" $ [ False, False, False ] ++ take 4 (repeat False) ++ repeat False
	  , with "ix" $ [ 0, 0, 0 ] ++ [0..3] 
	  )
	, with "val" $ [ 0, 0, 0 ] ++ [0..3]
	)

test_decode_control :: (x ~ X4) => Signal (DecodeCntl x)
test_decode_control = controller clock go 
   where
	go = with "go" $ ([False,False,False,True] ++ repeat False)
	

test_decode = decode 0 clock ( test_decode_control, test_decode_lambda_in, test_decode_global_in )

test = debugCircuit [] (decode 0)
