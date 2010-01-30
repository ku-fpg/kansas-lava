{-# LANGUAGE ScopedTypeVariables #-}
import Language.KansasLava hiding (head)

--import Language.KansasLava.Applicative

import Data.Sized.Ix
import Data.Sized.Arith
import Data.Bits
import Control.Applicative
import Data.Maybe  as Maybe

-- A pipe is a segmented value, pushed through a narrow pipe
type Pipe a d = (Enabled a,Signal d)

type Mem a d = Signal (MemOp a d) -> Signal d

type Enabled a = (Signal Bool, Signal a)

pipeToMem :: Time -> Pipe a d -> Mem a d
pipeToMem tm pipe memOp = undefined

memToPipe :: Time -> Pipe () a -> Mem a d -> Pipe a d
memToPipe = undefined

--memToPipe :: Enabled a -> Mem a d -> Pipe a d
--memToPipe (


data DecodeCntl x
	= DecodeWait		-- 0 + 0
	| DecodeRst		-- 0 + 1	restart
	| DecodeRest		-- 0 + 2
	| DecodeLoad x		-- 1 + x
	| DecodeShare x		-- 2 + x
	| DecodeResult x	-- 3 + x
	deriving (Show, Eq, Ord)

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


fullEnabled :: (Bounded a, OpType a, Enum a, Show a, OpType b, Enum b, Show b) => (a -> Maybe b) -> Signal a -> Enabled b
fullEnabled f s = ( fullMap (Maybe.isJust . f) s
	          , fullMapWithMaybe f s
	          )

counter :: (OpType a) => Time -> Enabled a -> Signal a -> (Signal a -> Signal a) -> Signal a
counter tm en@(gtg,val) def loop = now
  where
	now = mux2 gtg val (delay tm def (loop now))
	brk :: Signal Bool
	brk = pure False

{-
-- the controlling logic, that generates the various signal for other components to use
controller :: Time -> Signal Bool -> Signal (DecodeCntl X4)
controller clk en = counter clk (en,pure DecodeRst)
	where
-}		

test = counter clock (with "en" $ [False,False,False,False,True,False] ++ cycle [False] ,pure DecodeRst)
 	            (pure DecodeWait) example 
test2 clk en def = counter clk en def example


{-
decode :: (a ~ Unsigned x, Size x, Num x, Enum x) 
        => x -> 
	  ( Signal Ctrl
	  , Pipe x a
	  , Pipe x a
	  ) ->
	  ( Pipe x a
	  , Pipe x a
	  )
decode x (cntl, inp, glob) = ( out, loc )
   where
-}