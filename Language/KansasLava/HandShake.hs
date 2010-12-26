{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.HandShake where

import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Types
import Language.KansasLava.Signal

-- import Language.KansasLava.Entity
import Language.KansasLava.Deep
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
import Language.KansasLava.StdLogicVector
import Foreign.LambdaBridge.Service

import Language.KansasLava.Utils
import Control.Applicative
import Control.Concurrent

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


--liftHandShake :: (a -> b) -> Handshake a -> Handshake b
--liftHandShake = undefined

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

-- create a lambda bridge from a FIFO to a FIFO.
-- (Could be generalize to Matrix of FIFO  to Matrix of FIFO)
handShakeLambdaBridge :: (HandShake (Seq (Enabled Byte)) -> HandShake (Seq (Enabled Byte))) -> IO ()
handShakeLambdaBridge fn = bridge_service $ \ cmds [send] [recv] -> do
	sFIFO <- newShallowFIFO
	rFIFO <- newShallowFIFO

	forkIO $ hGetToFIFO send sFIFO
	hPutFromFIFO recv rFIFO

	sHS <- shallowFifoToHandShake sFIFO
	let rHS = fn sHS
	handShakeToShallowFifo rFIFO rHS
	return ()



incGroup :: (Rep x, Num x, Bounded x) => Comb x -> Comb x
incGroup x = mux2 (x .==. maxBound) (0,x + 1)

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
      => Witness ix
      -> Seq Bool
      -> (HandShake (Seq (Enabled a)),Seq counter)
	 -- ^ HS, and Seq trigger of how much to decrement the counter
      -> Seq (Enabled (ix,a))
	 -- ^ inc_counter * backedge for HandShake.
fifoFE Witness rst (HandShake hs,dec_by) = wr
  where
	resetable x = mux2 rst (0,x)

	inp = hs inp_ready

--	mem :: Seq ix -> Seq a
--	mem = pipeToMemory env env wr

	inp_done0 :: Seq Bool
	inp_done0 = inp_ready `and2` isEnabled inp

	wr :: Seq (Enabled (ix,a))
	wr = packEnabled (inp_done0)
			 (pack (wr_addr,enabledVal inp))

	wr_addr :: Seq ix
	wr_addr = resetable
		$ register 0
		$ mux2 inp_done0 (liftS1 incGroup wr_addr,wr_addr)

	in_counter0 :: Seq counter
	in_counter0 = resetable
		    $ in_counter1 
			+ mux2 inp_done0 (1,0)
		   	- dec_by

	in_counter1 :: Seq counter
	in_counter1 = register 0 in_counter0
	
--	out :: Seq (Enabled a)
--	out = packEnabled (out_counter1 .>. 0) (mem rd_addr0)

	inp_ready :: Seq Bool
	inp_ready = (in_counter1 .<. fromIntegral (size (error "witness" :: ix)))
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
      => Witness ix
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
fifoBE Witness rst (inc_by,mem_rd) = HandShake $ \ out_ready ->
    let
	resetable x = mux2 rst (0,x)

	rd_addr0 :: Seq ix
	rd_addr0 = resetable 
		 $ mux2 out_done0 (liftS1 incGroup rd_addr1,rd_addr1)

	rd_addr1 = register 0 
		 $ rd_addr0

	out_done0 :: Seq Bool
	out_done0 = out_ready `and2` (isEnabled out)

	out :: Seq (Enabled a)
	out = packEnabled (out_counter1 .>. 0 `and2` bitNot rst `and2` isEnabled mem_rd) (enabledVal mem_rd)

	out_counter0 :: Seq counter
	out_counter0 = resetable
		     $ out_counter1
			+ inc_by
			- mux2 out_done0 (1,0)

	out_counter1 = register 0 out_counter0
    in
        ((rd_addr0, out_done0) , out)

fifoCounter :: forall counter . (Num counter, Rep counter) => Seq Bool -> Seq Bool -> Seq Bool -> Seq counter
fifoCounter rst inc dec = counter1
    where
	resetable x = mux2 rst (0,x)

	counter0 :: Seq counter
	counter0 = resetable
		 $ counter1
			+ mux2 inc (1,0)
			- mux2 dec (1,0)

	counter1 = register 0 counter0

fifoCounter' :: forall counter . (Num counter, Rep counter) => Seq Bool -> Seq counter -> Seq counter -> Seq counter
fifoCounter' rst inc dec = counter1
    where
	resetable x = mux2 rst (0,x)

	counter0 :: Seq counter
	counter0 = resetable
		 $ counter1
			+ inc -- mux2 inc (1,0)
			- dec -- mux2 dec (1,0)

	counter1 = register 0 counter0



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
      => Witness ix
      -> Seq Bool
      -> HandShake (Seq (Enabled a))
      -> HandShake (Seq (Enabled a))
fifo w_ix@Witness rst hs = HandShake $ \ out_ready ->
    let
	resetable x = mux2 rst (low,x)

	wr :: Seq (Maybe (ix, a))
	wr = fifoFE w_ix rst (hs,dec_by)

	inp_done2 :: Seq Bool
	inp_done2 = resetable $ register false $ resetable $ register false $ resetable $ isEnabled wr

	mem :: Seq ix -> Seq (Enabled a)
	mem = enabledS . pipeToMemory wr

	((rd_addr0,out_done0),out) = fifoBE w_ix rst (inc_by,mem rd_addr0) <~~ out_ready


	dec_by = liftS1 (\ b -> mux2 b (1,0)) out_done0
	inc_by = liftS1 (\ b -> mux2 b (1,0)) inp_done2
    in
	out

fifoToMatrix :: forall a counter counter2 ix iy iz . 
         (Size counter
	, Size ix
	, Size counter2, Rep counter2, Num counter2
	, counter ~ ADD ix X1
	, counter2 ~ ADD iy X1
	, Rep a
	, Rep counter
	, Rep ix
	, Num counter
	, Num ix
	, Size iy
	, Rep iy, StdLogic ix, StdLogic iy, StdLogic a,
	WIDTH ix ~ ADD (WIDTH iz) (WIDTH iy),
	StdLogic counter, StdLogic counter2,
	StdLogic iz, Size iz, Rep iz, Num iy
	, WIDTH counter ~ ADD (WIDTH iz) (WIDTH counter2)
	, Num iz
--	ADD (WIDTH iz) (WIDTH counter2) ~ WIDTH counter
--	, Integral (Seq counter)

	) 
      => Witness ix
      -> Witness iy
      -> Seq Bool
      -> HandShake (Seq (Enabled a))
      -> HandShake (Seq (Enabled (M.Matrix iz a)))
fifoToMatrix w_ix@Witness w_iy@Witness rst hs = HandShake $ \ out_ready ->
    let
	resetable x = mux2 rst (low,x)

	wr :: Seq (Maybe (ix, a))
	wr = fifoFE w_ix rst (hs,dec_by)

	inp_done2 :: Seq Bool
	inp_done2 = resetable $ register false $ resetable $ register false $ resetable $ isEnabled wr

	mem :: Seq (Enabled (M.Matrix iz a))
	mem = enabledS 
	 	$ pack
	 	$ fmap (\ f -> f rd_addr0)
	 	$ fmap pipeToMemory
	 	$ splitWrite
	 	$ mapEnabled (mapPacked $ \ (a,d) -> (factor a,d))
		$ wr

	((rd_addr0,out_done0),out) = fifoBE w_iy rst (inc_by,mem) <~~ out_ready

	dec_by = mulBy (Witness :: Witness iz) out_done0
	inc_by = divBy (Witness :: Witness iz) rst inp_done2
    in
	out

-- Move into a Commute module? 
splitWrite :: forall a a1 a2 d . (Rep a1, Rep a2, Rep d, Size a1) => Seq (Pipe (a1,a2) d) -> M.Matrix a1 (Seq (Pipe a2 d))
splitWrite inp = M.forAll $ \ i -> let (g,v)   = unpackEnabled inp
			               (a,d)   = unpack v
			               (a1,a2) = unpack a
			            in packEnabled (g .&&. (a1 .==. pureS i))
				   	           (pack (a2,d))


mulBy :: forall x sz sig . (Size sz, Num sz, Num x, Rep x) => Witness sz -> Seq Bool -> Seq x
mulBy Witness trig = mux2 trig (pureS $ fromIntegral $ size (error "witness" :: sz),pureS 0)

divBy :: forall x sz sig . (Size sz, Num sz, Rep sz, Num x, Rep x) => Witness sz -> Seq Bool -> Seq Bool -> Seq x
divBy Witness rst trig = mux2 issue (1,0)
	where 
		issue = trig .&&. (counter1 .==. (pureS $ fromIntegral (size (error "witness" :: sz) - 1)))

		counter0 :: Seq sz
		counter0 = cASE [ (rst,0)
				, (trig,counter1 + 1)
				] counter1
		counter1 :: Seq sz
		counter1 = register 0 
			 $ mux2 issue (0,counter0)




