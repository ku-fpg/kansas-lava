{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp,
    RankNTypes  #-}

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
import Language.KansasLava.Shallow
import Language.KansasLava.Seq
import Language.KansasLava.Protocols
import Language.KansasLava.Shallow.FIFO 
import Language.KansasLava.StdLogicVector

import Language.KansasLava.Utils
import Control.Applicative
import Control.Concurrent

-----------------------------------------------------------------------------------------------
--
-- In 



-----------------------------------------------------------------------------------------------
-- type Handshake a = HandShaken (Seq (Enabled a))

{- | 

A Handshaken value is a value that has a has been accepted backedge.

> DIAGRAM

There is nothing wrong with trying to accept a value that is not sent.

-}

data HandShaken c a = HandShaken { unHandShaken :: CSeq c Bool -> a }


infix 4 <~~

(<~~) :: HandShaken c a -> CSeq c Bool -> a
(HandShaken h) <~~ a = h a

instance Functor (HandShaken c) where
	fmap f (HandShaken g) = HandShaken (f . g)

instance Applicative (HandShaken c) where
	pure a = HandShaken $ const a					-- K
	(HandShaken f) <*> (HandShaken g) = HandShaken (\ s -> f s (g s))	-- S

instance Monad (HandShaken c) where
	return a = HandShaken $ const a					-- K
	(HandShaken f) >>= k = HandShaken $ \ s -> unHandShaken (k (f s)) s


{-
instance Signal Handshake where
  liftS0 comb = Handshake $ \ _ -> enabledS $ liftS0 comb

  liftS1 f (Handshake shake) = Handshake $ \ rd -> liftS1 (mapEnabled f) (shake rd)

  liftS2 f (Handshake s1) (Handshake s2) = undefined
	-- This is where the magic will happen, giving a token passing implementation for free

  deepS _ = error "not possible to extract a Handshake without backedge"
-}


--liftHandShaken :: (a -> b) -> Handshake a -> Handshake b
--liftHandShaken = undefined

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

toHandShaken' :: (Rep a) => [Int] -> [Maybe a] -> HandShaken c (CSeq c (Enabled a))
toHandShaken' stutter xs = HandShaken $ \ ready -> toHandShaken stutter xs ready

toHandShaken :: (Rep a) => [Int] -> [Maybe a] -> (CSeq c Bool -> CSeq c (Enabled a))
toHandShaken stutter xs ready = toSeq (fn stutter xs (fromSeq ready))
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



fromHandShaken' :: forall a c . (Clock c, Rep a) => [Int] -> HandShaken c (CSeq c (Enabled a)) -> [Maybe a]
fromHandShaken' stutter (HandShaken sink) = res
    where (back, res) = fromHandShaken  stutter (sink back)


fromHandShaken :: forall a c . (Clock c, Rep a) => [Int] -> CSeq c (Enabled a) -> (CSeq c Bool, [Maybe a])
fromHandShaken stutter inp = (toSeq (map fst internal), map snd internal)
   where
	internal :: [(Bool,Maybe a)]
	internal = fn stutter (fromSeq inp)
	
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

shallowFifoToHandShaken :: (Clock c, Show a, Rep a) => ShallowFIFO a -> IO (HandShaken c (CSeq c (Enabled a)))
shallowFifoToHandShaken fifo = do
	xs <- getFIFOContents fifo
	return (toHandShaken' (repeat 0) (xs ++ repeat Nothing))

handShakeToShallowFifo :: (Clock c, Show a, Rep a) => ShallowFIFO a -> HandShaken c (CSeq c (Enabled a)) -> IO ()
handShakeToShallowFifo fifo sink = do
	putFIFOContents fifo (fromHandShaken' (repeat 0) sink)
	return ()

{- TODO: move into another location
-- create a lambda bridge from a FIFO to a FIFO.
-- (Could be generalize to Matrix of FIFO  to Matrix of FIFO)
handShakeLambdaBridge :: (Clock c) => (HandShaken c (CSeq c (Enabled Byte)) -> HandShaken c (CSeq c (Enabled Byte))) -> IO ()
handShakeLambdaBridge fn = bridge_service $ \ cmds [send] [recv] -> do
	sFIFO <- newShallowFIFO
	rFIFO <- newShallowFIFO

	forkIO $ hGetToFIFO send sFIFO
	hPutFromFIFO recv rFIFO

	sHS <- shallowFifoToHandShaken sFIFO
	let rHS = fn sHS
	handShakeToShallowFifo rFIFO rHS
	return ()
-}

incGroup :: (Rep x, Num x, Bounded x) => Comb x -> Comb x
incGroup x = mux2 (x .==. maxBound) (0,x + 1)

fifoFE :: forall c a counter ix .
         (Size counter
	, Size ix
	, counter ~ ADD ix X1
	, Rep a
	, Rep counter
	, Rep ix
	, Num counter
	, Num ix
	, Clock c
	) 
      => Witness ix
      -> CSeq c Bool
      -> (CSeq c (Enabled a), CSeq c counter)
	 -- ^ HS, and Seq trigger of how much to decrement the counter
      -> (CSeq c Bool, CSeq c (Enabled (ix,a)))
	 -- ^ inc_counter * backedge for HandShaken.
fifoFE Witness rst (inp,dec_by) = (inp_ready,wr)
  where

        resetable :: forall b. (Rep b, Num b) => CSeq c b -> CSeq c b
	resetable x = mux2 rst (0,x)

--	mem :: Seq ix -> Seq a
--	mem = pipeToMemory env env wr

	inp_done0 :: CSeq c Bool
	inp_done0 = inp_ready `and2` isEnabled inp

	wr :: CSeq c (Enabled (ix,a))
	wr = packEnabled (inp_done0)
			 (pack (wr_addr,enabledVal inp))

	wr_addr :: CSeq c ix
	wr_addr = resetable
		$ register 0
		$ mux2 inp_done0 (liftS1 incGroup wr_addr,wr_addr)

	in_counter0 :: CSeq c counter
	in_counter0 = resetable
		    $ in_counter1 
			+ mux2 inp_done0 (1,0)
		   	- dec_by

	in_counter1 :: CSeq c counter
	in_counter1 = register 0 in_counter0
	
--	out :: Seq (Enabled a)
--	out = packEnabled (out_counter1 .>. 0) (mem rd_addr0)

	inp_ready :: CSeq c Bool
	inp_ready = (in_counter1 .<. fromIntegral (size (error "witness" :: ix)))
			`and2`
		    (bitNot rst)

fifoBE :: forall a c counter ix .
         (Size counter
	, Size ix
 	, counter ~ ADD ix X1
	, Rep a
	, Rep counter
	, Rep ix
	, Num counter
	, Num ix
	, Clock c
	) 
      => Witness ix
      -> CSeq c Bool	-- ^ reset
--      -> (Comb Bool -> Comb counter -> Comb counter)
--      -> Seq (counter -> counter)
      -> (CSeq c counter,CSeq c (Enabled a))
	-- inc from FE
	-- input from Memory read
      -> CSeq c Bool
      -> ((CSeq c ix, CSeq c Bool), CSeq c (Enabled a))
	-- address for Memory read
	-- dec to FE
	-- output for HandShaken
fifoBE Witness rst (inc_by,mem_rd) out_ready = 
    let
        resetable :: forall b. (Rep b, Num b) => CSeq c b -> CSeq c b
	resetable x = mux2 rst (0,x)

	rd_addr0 :: CSeq c ix
	rd_addr0 = resetable 
		 $ mux2 out_done0 (liftS1 incGroup rd_addr1,rd_addr1)

	rd_addr1 = register 0 
		 $ rd_addr0

	out_done0 :: CSeq c Bool
	out_done0 = out_ready `and2` (isEnabled out)

	out :: CSeq c (Enabled a)
	out = packEnabled (out_counter1 .>. 0 `and2` bitNot rst `and2` isEnabled mem_rd) (enabledVal mem_rd)

	out_counter0 :: CSeq c counter
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



fifo :: forall a c counter ix . 
         (Size counter
	, Size ix
	, counter ~ ADD ix X1
	, Rep a
	, Rep counter
	, Rep ix
	, Num counter
	, Num ix
	, Clock c
	) 
      => Witness ix
      -> CSeq c Bool
      -> (CSeq c (Enabled a), CSeq c Bool)
      -> (CSeq c Bool, CSeq c (Enabled a))
fifo w_ix rst (inp,out_ready) = 
    let
	resetable x = mux2 rst (low,x)

	wr :: CSeq c (Maybe (ix, a))
	inp_ready :: CSeq c Bool
	(inp_ready, wr) = fifoFE w_ix rst (inp,dec_by)

	inp_done2 :: CSeq c Bool
	inp_done2 = resetable $ register False $ resetable $ register False $ resetable $ isEnabled wr

	mem :: CSeq c ix -> CSeq c (Enabled a)
	mem = enabledS . pipeToMemory wr

	((rd_addr0,out_done0),out) = fifoBE w_ix rst (inc_by,mem rd_addr0) out_ready

	dec_by = liftS1 (\ b -> mux2 b (1,0)) out_done0
	inc_by = liftS1 (\ b -> mux2 b (1,0)) inp_done2
    in
	(inp_ready, out)
{-
fifoToMatrix :: forall a counter counter2 ix iy iz c . 
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
	, Clock c
	) 
      => Witness ix
      -> Witness iy
      -> CSeq c Bool
      -> HandShaken c (CSeq c (Enabled a))
      -> HandShaken c (CSeq c (Enabled (M.Matrix iz a)))
fifoToMatrix w_ix@Witness w_iy@Witness rst hs = HandShaken $ \ out_ready ->
    let
	resetable x = mux2 rst (low,x)

	wr :: CSeq c (Maybe (ix, a))
	wr = fifoFE w_ix rst (hs,dec_by)

	inp_done2 :: CSeq c Bool
	inp_done2 = resetable $ register False $ resetable $ register False $ resetable $ isEnabled wr

	mem :: CSeq c (Enabled (M.Matrix iz a))
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
-- classical find the implementation problem.
splitWrite :: forall a a1 a2 d c . (Rep a1, Rep a2, Rep d, Size a1) => CSeq c (Pipe (a1,a2) d) -> M.Matrix a1 (CSeq c (Pipe a2 d))
splitWrite inp = M.forAll $ \ i -> let (g,v)   = unpackEnabled inp
			               (a,d)   = unpack v
			               (a1,a2) = unpack a
			            in packEnabled (g .&&. (a1 .==. pureS i))
				   	           (pack (a2,d))


mulBy :: forall x sz sig c . (Clock c, Size sz, Num sz, Num x, Rep x) => Witness sz -> CSeq c Bool -> CSeq c x
mulBy Witness trig = mux2 trig (pureS $ fromIntegral $ size (error "witness" :: sz),pureS 0)

divBy :: forall x sz sig c . (Clock c, Size sz, Num sz, Rep sz, Num x, Rep x) => Witness sz -> CSeq c Bool -> CSeq c Bool -> CSeq c x
divBy Witness rst trig = mux2 issue (1,0)
	where 
		issue = trig .&&. (counter1 .==. (pureS $ fromIntegral (size (error "witness" :: sz) - 1)))

		counter0 :: CSeq c sz
		counter0 = cASE [ (rst,0)
				, (trig,counter1 + 1)
				] counter1
		counter1 :: CSeq c sz
		counter1 = register 0 
			 $ mux2 issue (0,counter0)


-- sub-domain for the inner clock.
liftHandShaken :: (Clock c1) 
	=> (forall c0 . (Clock c0) => Clocked c0 a -> Clocked c0 b)
	-> HandShaken c1 (Enabled a) 
	-> HandShaken c1 (Enabled b)
liftHandShaken f = undefined


-}