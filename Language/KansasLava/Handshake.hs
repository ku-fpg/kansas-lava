{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Handshake where

import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Types
import Language.KansasLava.Signal

import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Stream as S

import Control.Applicative
import Data.Sized.Unsigned as UNSIGNED
import Data.Sized.Signed as SIGNED
import Data.Sized.Sampled as SAMPLED
import Data.Sized.Arith as Arith
import qualified Data.Sized.Matrix as M
import Data.Sized.Ix as X

import Language.KansasLava.Comb
import Data.List as List
import Language.KansasLava.Wire
import Language.KansasLava.Seq
import Language.KansasLava.Protocols
import Language.KansasLava.Shallow.FIFO 

import Language.KansasLava.Utils
import Control.Applicative

-----------------------------------------------------------------------------------------------

-- Need to add concept of clock

type Handshake a = HandShake (Seq (Enabled a))


data HandShake a = HandShake { unHandShake :: Seq Bool -> a }

infix 4 <~~

(<~~) :: HandShake a -> Seq Bool -> a
(HandShake h) <~~ a = h a

instance Functor HandShake where
	fmap f (HandShake g) = HandShake (f . g)

instance Applicative HandShake where
	pure a = HandShake $ const a					-- K
	(HandShake f) <*> (HandShake g) = HandShake (\ s -> f s (g s))	-- S

instance Monad HandShake where
	return a = HandShake $ const a					-- K
	(HandShake f) >>= k = HandShake $ \ s -> unHandShake (k (f s)) s


{-
instance Signal Handshake where
  liftS0 comb = Handshake $ \ _ -> enabledS $ liftS0 comb

  liftS1 f (Handshake shake) = Handshake $ \ rd -> liftS1 (mapEnabled f) (shake rd)

  liftS2 f (Handshake s1) (Handshake s2) = undefined
	-- This is where the magic will happen, giving a token passing implementation for free

  deepS _ = error "not possible to extract a Handshake without backedge"
-}


liftHandShake :: (Comb a -> Comb b) -> Handshake a -> Handshake b
liftHandShake = undefined

----------------------------------------------------------------------------------------------------
{-
-- | @toHandshake'@ takes a list of stutters (counts of time to delay the output of a value) and
-- a list of values to be handshaken, including @Nothing@'s, that represent 
toHandshake' :: (Rep a) => [Int] -> [Maybe a] -> Handshake a
toHandshake' stutter xs = Handshake $ \ ready -> toSeq (fn stutter xs (fromSeq ready))
	where
	   -- We rely on the semantics of pattern matching to not match (x:xs)
	   -- if (0:ps) does not match.
	   fn (0:ps) (x:xs) c
		    = x : case (c,x) of -- read c after issuing x
			(Nothing:rs,_)         -> error "toVariableHandshake: bad protocol state (1)"
			(Just True:rs,_)       -> fn ps xs rs         -- has been written
			(_:rs,_)               -> fn (0:ps) (x:xs) rs -- not written yet

	   fn (p:ps) xs c
		    = Nothing : case c of
			(Nothing:rs)         -> error "toVariableHandshake: bad protocol state (2)"
			(Just _:rs) 	     -> fn (pred p:ps) xs rs -- nothing read
-}

toHandShake' :: (Rep a) => [Int] -> [Maybe a] -> HandShake (Seq (Enabled a))
toHandShake' stutter xs = HandShake $ \ ready -> toSeq (fn stutter xs (fromSeq ready))
	where
	   -- We rely on the semantics of pattern matching to not match (x:xs)
	   -- if (0:ps) does not match.
	   fn (0:ps) (x:xs) c
		    = x : case (c,x) of -- read c after issuing x
			(Nothing:rs,_)         -> error "toVariableHandshake: bad protocol state (1)"
			(Just True:rs,_)       -> fn ps xs rs         -- has been written
			(_:rs,_)               -> fn (0:ps) (x:xs) rs -- not written yet

	   fn (p:ps) xs c
		    = Nothing : case c of
			(Nothing:rs)         -> error "toVariableHandshake: bad protocol state (2)"
			(Just _:rs) 	     -> fn (pred p:ps) xs rs -- nothing read



fromHandShake' :: forall a . (Rep a) => [Int] -> HandShake (Seq (Enabled a)) -> [Maybe a]
fromHandShake' stutter (HandShake sink) = map snd internal
   where
	val :: Seq (Enabled a)
	val = sink full

	full :: Seq Bool
	full = toSeq (map fst internal)

	internal :: [(Bool,Maybe a)]
	internal = fn stutter (fromSeq val)
	
	fn :: [Int] -> [Maybe (Enabled a)] -> [(Bool,Maybe a)]
	fn (0:ps) ~(x:xs) = (True,rep) : rest
	   where
		(rep,rest) = case x of
			       Nothing       -> error "fromVariableHandshake: bad reply to ready status"
			       Just Nothing  -> (Nothing,fn (0:ps) xs)
			       Just (Just v) -> (Just v,fn ps xs)
	fn (p:ps) ~(x:xs) = (False,Nothing) : fn (pred p:ps) xs

{-
fromHandshake' :: forall a . (Rep a) => [Int] -> Handshake a -> [Maybe a]
fromHandshake' stutter (Handshake sink) = map snd internal
   where
	val :: Seq (Enabled a)
	val = sink full

	full :: Seq Bool
	full = toSeq (map fst internal)

	internal :: [(Bool,Maybe a)]
	internal = fn stutter (fromSeq val)
	
	fn :: [Int] -> [Maybe (Enabled a)] -> [(Bool,Maybe a)]
	fn (0:ps) ~(x:xs) = (True,rep) : rest
	   where
		(rep,rest) = case x of
			       Nothing       -> error "fromVariableHandshake: bad reply to ready status"
			       Just Nothing  -> (Nothing,fn (0:ps) xs)
			       Just (Just v) -> (Just v,fn ps xs)
	fn (p:ps) ~(x:xs) = (False,Nothing) : fn (pred p:ps) xs
-}

----------------------------------------------------------------------------------------------------

-- | This function takes a ShallowFIFO object, and gives back a Handshake.
-- ShallowFIFO is typically connected to a data generator or source, like a file.

shallowFifoToHandShake :: (Show a, Rep a) => ShallowFIFO a -> IO (HandShake (Seq (Enabled a)))
shallowFifoToHandShake fifo = do
	xs <- getFIFOContents fifo
	return (toHandShake' (repeat 0) (xs ++ repeat Nothing))

handShakeToShallowFifo :: (Show a, Rep a) => ShallowFIFO a -> HandShake (Seq (Enabled a)) -> IO ()
handShakeToShallowFifo fifo sink = do
	putFIFOContents fifo (fromHandShake' (repeat 0) sink)
	return ()


--liftEnabledToHandshake :: Enabled a -> Handshake a
--fifo1 :: Env () -> Handshake a -> Handshake a
--fifo1 
{-
fifo :: forall a counter ix . 
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
      -> Handshake a
      -> Handshake (a,counter)
fifo w env (Handshake inp) = Handshake out
   where
	out :: Seq Bool -> Seq (Enabled a,counter)
	out out_ready = pack (out_data,counter)
	   where
		in_data = inp in_ready
		(in_ready,out_data,counter) = fifo' w env (out_ready,in_data)

-- IDEA: Handl
data Handshake c = Handshake { unHandshake :: Seq Bool -> c (Seq (Enabled a)) }

-}


fifo' :: forall a counter ix . 
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
      -> (Seq Bool,Seq (Enabled a),Seq counter)
fifo' _ env (out_ready,inp) = (inp_ready,out,in_counter1)
  where
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
		  $ out_done0

	in_counter0 :: Seq counter
	in_counter0 = in_counter1 
			+ mux2 inp_done0 (1,0)
		   	- mux2 out_done0 (1,0)

	in_counter1 :: Seq counter
	in_counter1 = register env 0 in_counter0

	out_counter0 :: Seq counter
	out_counter0 = out_counter1
			+ mux2 inp_done2 (1,0)
		 	- mux2 out_done0 (1,0)

	out_counter1 = register env 0 out_counter0
	
	out :: Seq (Enabled a)
	out = packEnabled (out_counter1 .>. 0) (mem rd_addr0)

	inp_ready :: Seq Bool
	inp_ready = in_counter1 .<. fromIntegral (size (witness :: ix))



fifoFE :: forall a counter ix . 
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
      -> Seq Bool
      -> (HandShake (Seq (Enabled a)),Seq Bool)
	 -- ^ HS, and Seq trigger when to decrement the counter
      -> Seq (Enabled (ix,a))
	 -- ^ inc_counter * backedge for HandShake.
fifoFE _ env rst (HandShake hs,out_done0) = wr
  where
	resetable x = mux2 rst (0,x)

	inp = hs inp_ready

--	mem :: Seq ix -> Seq a
--	mem = pipeToMemory env env wr

	inp_done0 :: Seq Bool
	inp_done0 = inp_ready `and2` isEnabled inp

	wr :: Seq (Enabled (ix,a))
	wr = packEnabled (inp_ready `and2` isEnabled inp)
			 (pack (wr_addr,enabledVal inp))

	wr_addr :: Seq ix
	wr_addr = resetable
		$ register env 0
		$ mux2 inp_done0 (wr_addr+1,wr_addr)

	in_counter0 :: Seq counter
	in_counter0 = resetable
		    $ in_counter1 
			+ mux2 inp_done0 (1,0)
		   	- mux2 out_done0 (1,0)

	in_counter1 :: Seq counter
	in_counter1 = register env 0 in_counter0
	
--	out :: Seq (Enabled a)
--	out = packEnabled (out_counter1 .>. 0) (mem rd_addr0)

	inp_ready :: Seq Bool
	inp_ready = (in_counter1 .<. fromIntegral (size (witness :: ix)))
			`and2`
		    (bitNot rst)

fifoBE :: forall a counter ix . 
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
      -> Seq Bool	-- ^ reset
--      -> (Comb Bool -> Comb counter -> Comb counter)
--      -> Seq (counter -> counter)
      -> (Seq counter,Seq (Enabled a))
	-- inc from FE
	-- input from Memory read
      -> HandShake ((Seq ix, Seq Bool), Seq (Enabled a))
	-- address for Memory read
	-- dec to FE
	-- output for HandShake
fifoBE _ env rst (out_counter1,mem_rd) = HandShake $ \ out_ready ->
    let
	resetable x = mux2 rst (0,x)

	rd_addr0 :: Seq ix
	rd_addr0 = resetable 
		 $ mux2 out_done0 (rd_addr1+1,rd_addr1)

	rd_addr1 = register env 0 
		 $ rd_addr0

	out_done0 :: Seq Bool
	out_done0 = out_ready `and2` (isEnabled out)

	out :: Seq (Enabled a)
	out = packEnabled (out_counter1 .>. 0 `and2` bitNot rst `and2` isEnabled mem_rd) (enabledVal mem_rd)
    in
        ((rd_addr0, out_done0) , out)

fifoCounter :: forall counter . (Num counter, Rep counter) => Env () -> Seq Bool -> Seq Bool -> Seq Bool -> Seq counter
fifoCounter env rst inc dec = counter1
    where
	resetable x = mux2 rst (0,x)

	counter0 :: Seq counter
	counter0 = resetable
		 $ counter1
			+ mux2 inc (1,0)
			- mux2 dec (1,0)

	counter1 = register env 0 counter0


fifo :: forall a counter ix . 
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
      -> Seq Bool
      -> HandShake (Seq (Enabled a))
      -> HandShake (Seq (Enabled a))
fifo w env rst hs = HandShake $ \ out_ready ->
    let
	resetable x = mux2 rst (low,x)

	wr :: Seq (Maybe (ix, a))
	wr = fifoFE w env rst (hs,out_done0)

	inp_done2 :: Seq Bool
	inp_done2 = resetable $ register env false $ resetable $ register env false $ resetable $ isEnabled wr

	mem :: Seq ix -> Seq (Enabled a)
	mem = enabledS . pipeToMemory env env wr

	((rd_addr0,out_done0),out) = fifoBE w env rst (out_counter,mem rd_addr0) <~~ out_ready


	out_counter = fifoCounter env rst inp_done2 out_done0
    in
	out

{-
-- decrement does not work, no fliping between memories
fifoPair'' :: forall a counter ix . 
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
      -> (Seq Bool,Seq (Enabled (a,a)))
fifoPair'' w env (out_ready,inp) = (inp_ready,out)
  where
	(wr,inp_ready) = fifoFE' w env (error "X") (out_done0,inp)

	inp_done0 = isEnabled wr

	memA :: Seq ix -> Seq a
	memA = pipeToMemory env env wr

	memB :: Seq ix -> Seq a
	memB = pipeToMemory env env wr

	(rd_addr0,out_done0,out,_) = fifoBE' w env (out_ready,inp_done0,pack (memA rd_addr0,memB rd_addr0))
-}
---------------------------

data CounterCntl = IncCounter | DecCounter | RstCounter | MaxCounter 
	deriving (Eq, Ord, Enum, Show)

instance Rep CounterCntl where
    data X CounterCntl     = XCounterCntl CounterCntl
			   | XCounterCntlUnknown
    optX (Just b)             = XCounterCntl $ b
    optX Nothing              = XCounterCntlUnknown
    unX (XCounterCntl v)      = return v
    unX (XCounterCntlUnknown) = fail "Wire CounterCntl"
    wireType _  	      = V 2

    toRep   		      = enum_toRep counterCntls
    fromRep		      = enum_fromRep counterCntls
    showRep _ (XCounterCntl c)  = show c
    showRep _ _		      = "?"

counterCntls = [IncCounter,DecCounter,RstCounter,MaxCounter]

-- This can be moved
enum_toRep :: forall a . (Eq a, Rep a) => [a] -> X a -> RepValue
enum_toRep alls x_a = case unX x_a of
			Just a -> case lookup a env of
			   Just v -> v
			   Nothing -> unknownRepValue (witness :: a)
			Nothing -> unknownRepValue (witness :: a)
	where
		env = alls `zip` (allReps (witness :: a))


enum_fromRep :: forall a . (Rep a) => [a] -> RepValue -> X a
enum_fromRep alls repVal = optX $ lookup repVal env
	where
		env = (allReps (witness :: a)) `zip` alls 

counter :: forall a. (Size a, Num a, Rep a) => Env () -> Seq (Enabled CounterCntl) -> Seq a
counter env cntr = out0
  where
	out0 :: Seq a
	out0 = cASE 
		[(cntr .==. pureS (Just IncCounter),out1 + 1)
		,(cntr .==. pureS (Just DecCounter),out1 - 1)
		,(cntr .==. pureS (Just RstCounter),0)
		,(cntr .==. pureS (Just MaxCounter),fromIntegral ((size (witness :: a)) - 1))
		] out1

	out1 :: Seq a
	out1 = register env 0 out0
