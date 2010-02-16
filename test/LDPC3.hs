{-# LANGUAGE ScopedTypeVariables,TypeFamilies, UndecidableInstances, FlexibleContexts, FlexibleInstances, ExistentialQuantification #-}
import Language.KansasLava -- hiding (head)

--import Language.KansasLava.Applicative

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Sampled as S
import qualified Data.Sized.Signed as Signed
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
import qualified Data.Map as Map

type SZ = X4
type FLOAT = Sampled X8 X8

data DecodeCntl 
	= DecodeWait		-- completed, rest state
	| DecodeRst SZ		-- reset element # of the 'ne' memory
	| DecodeLoad SZ		-- 1 + x
	| DecodePostLoad X4	-- wait 2 cycles
	| DecodeShare SZ	-- 2 + x
	| DecodePostShare X4	-- wait 2 cycles
	| DecodeResult SZ	-- 3 + x
	deriving (Show, Eq, Ord)

-- This is our state machine rep.
-- You always go next to your next element in your list.
incDecodeCntrlLists :: [[DecodeCntl]]
incDecodeCntrlLists =
	[ [DecodeWait,DecodeWait]
	, [DecodeRst n | n <- [0..maxBound]]
	    ++ [DecodeLoad x | x <- [0..maxBound]]
	    ++ [DecodePostLoad x | x <- [1,0]]
	    ++ [DecodeShare x | x <- [0..maxBound]]	
	    ++ [DecodePostShare x | x <- [1,0]]			
	    ++ [DecodeResult x | x <- [0..maxBound]]	
	    ++ [DecodeWait]
	]

-- force DecodeWait to be 0
allDecodeCntl :: [DecodeCntl]
allDecodeCntl = List.nub (DecodeWait : concat incDecodeCntrlLists)

fromWireRepEnv :: Map.Map DecodeCntl (Matrix (WIDTH DecodeCntl) Bool)
fromWireRepEnv = Map.fromList $ zip allDecodeCntl $ map U.toMatrix [0..]
toWireRepEnv   :: Map.Map (Matrix (WIDTH DecodeCntl) Bool) DecodeCntl
toWireRepEnv = Map.fromList $ map (\(a,b) -> (b,a)) $ Map.toList fromWireRepEnv

instance Wire DecodeCntl where
	type X DecodeCntl = WireVal DecodeCntl	-- choice: top level failure
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire DecodeCntl"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire DecodeCntl"
	wireName _ = "DecodeCntl"
	wireType _ = U 4 -- TODO: derving this

instance RepWire DecodeCntl where	
	type WIDTH DecodeCntl = X5

-- encoding
	fromWireRep op = case Map.lookup op fromWireRepEnv of
			   Just m -> m
			   Nothing -> forAll $ \ _  -> False
	toWireRep op = Map.lookup op toWireRepEnv 
	showRepWire _ = show

stateBuilder :: (Ord a, Show a) => [[a]] -> a -> a
stateBuilder transs = \ x -> 
	case Map.lookup x mp of
	  Nothing -> error $ "Can not find transition for " ++ show x
	  Just x' -> x'
  where mp = Map.fromList [ (x,y) | trans <- transs, (x,y) <- zip trans (List.tail trans) ]

incDecodeCntrl = stateBuilder incDecodeCntrlLists

-- Slow down the control signal, using 'Wait' as the default
delayCntlSeq :: Int -> Seq SysEnv -> Seq DecodeCntl -> Seq DecodeCntl
delayCntlSeq 0 e s = s
delayCntlSeq n e s = delay e (pureS DecodeWait) s


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
boot = toSeqX $ (pureX True, pureX (DecodeRst 0))
	      : repeat (pureX False, optX (Nothing :: Maybe DecodeCntl))
	



decode :: forall a x . 
         (a ~ OurFloat, Size x, Num x, Enum x, x ~ X4) 
       => Seq SysEnv
        ->
	  ( Seq DecodeCntl
	  , Seq (Pipe x a)		-- lambda (in)
	  , Seq (Pipe x a)		-- global (in, share)
	  ) ->
	   ( Seq (Matrix x a)
	   , Seq a
	   )
{-	  
	  ( Seq (Pipe x a)		-- local (out, share)
	  , Seq (Pipe x a)		-- lambda (out)
	  , Seq Bool			-- status
	  )
-}
decode tm (cntl, inp, glob) = (memoryToMatrix ne_mem,
	--out
	ne_mem act_val
	--int_a
	)
   where
	in_mem :: Memory x a
	in_mem = pipeToMemory tm inp


	rst_ne :: Seq (Enabled x)
	rst_ne = fullEnabled cntl $ \ e ->
			case e of
			   DecodeRst x -> return x
			   _ -> Nothing

	out_mem = in_mem

	-- The ne memory cells 
	ne_mem :: Memory x a
	ne_mem = pipeToMemory tm (enabledToPipe (\ x -> pack (x,1::Seq a)) rst_ne)

--	intermedute = in_mem act_enabled - ne_mem act_enabled

	act_enabled = fullEnabled cntl $ \ e ->
			case e of
			   DecodeShare x -> return x
			   _ -> Nothing

	-- -(lam ! j) + (ne ! (m,j))

	okay = high
	
	in_enabled = fullEnabled cntl $ \ e -> 
			case e of
			   DecodeLoad x -> return x
			   _ -> Nothing

	out_enabled = fullEnabled cntl $ \ e -> 
			case e of
			   DecodeResult x -> return x
			   _ -> Nothing
			 

	(int_en,int_pipe) = unpack (memoryToPipe act_enabled out_mem)
	(int_x,int_a) = unpack int_pipe
	
	(act_en,act_val) = unpack act_enabled

	res = ne_mem act_val - int_a

	out :: Seq a
	out = res
	
	loc :: Seq (Pipe x a)
	loc = pureS ( False, (0,0))
{-
 ne' = forAll $ \ (m,n) -> 
		if a_rref ! (m,n) == 1
                then - 0.75 * (foldr1 metric [-(lam ! j) + (ne ! (m,j))
--		then - 2 * atanh (product [ - tanh (((lam ! j) - (ne ! (m,j))) / 2)
			                  | j <- X.all
					  , a_rref ! (m,j) == 1
					  , j /= n
			                  ])
		else 0
		
--	    lam' :: Matrix y a
	    lam' = forAll $ \ n ->
		(orig_lam ! n)  + sum [ ne' ! (m,n) 
					   | m <-  X.all
					   , a_rref ! (m,n) == 1
					   ]
-}					
type OurFloat = Sampled X8 X8


hack :: (Wire x, RepWire v, Num v) => Seq (Enabled x) -> Seq (Pipe x v)
hack sq = pack (en, pack (x,0))
  where (en,x) = unpack sq
	

main = putStrLn $ showSomeTT 30 $ testDecode 


-------------------------
testDecode = truthTable 
	(example decode' .*. sysEnv .*. cntr .*. inp .*. glob)
   where
	decode' e c i g = decode e (c,i,g)
	cntr = stateMachine (incDecodeCntrl) boot
	inp  = toEnabledSeq $ 
	 take 4 (repeat Nothing) ++
	 [ return $ (0,1)
	 , return $ (1,2)
	 , return $ (2,3)
	 , return $ (3,4)
 	 ] ++ repeat Nothing

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


------------------------------------------------------------

-- This is the "inner loop", so we want to be able to 
-- hand generate good code here.

-- I am far from convinsed by this *function* (aka the math)
-- is correct, but will do as a placeholder till we can get it correct.
-- We were taking about needing to store two values, for example,
-- which has no bering here.

metric :: (Signal sig) => Maybe (sig FLOAT) -> Maybe (sig FLOAT) -> Maybe (sig FLOAT)
metric Nothing (Just s2) = return s2
metric (Just s1) Nothing = return s1
metric Nothing   Nothing = Nothing
metric (Just s1) (Just s2) = return $ liftS2 metricComb s1 s2

metricComb :: Comb FLOAT -> Comb FLOAT -> Comb FLOAT 
metricComb (Comb ox xe) (Comb oy ye) =
			Comb ((optX :: Maybe FLOAT -> X FLOAT) $
			      (do x <- unX ox :: Maybe FLOAT
			 	  y <- unX oy :: Maybe FLOAT
				  return $ signum(x) * signum(y) * (min (abs x) (abs y))))
	     		(entity2 (Name "LDPC" "metric") xe ye)
	

--- BAD

type S16 = Signed.Signed X16

bar :: Seq S16
bar = cm
    where cm  = delay sysEnv  0 cm'
          cm' = cm + 2


	
