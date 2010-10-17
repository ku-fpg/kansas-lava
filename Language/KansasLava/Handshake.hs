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

-----------------------------------------------------------------------------------------------

-- Need to add concept of clock

data Handshake a = Handshake { unHandshake :: Seq Bool -> Seq (Enabled a) }

instance Signal Handshake where
  liftS0 comb = Handshake $ \ _ -> enabledS $ liftS0 comb

  liftS1 f (Handshake shake) = Handshake $ \ rd -> liftS1 (mapEnabled f) (shake rd)

  liftS2 f (Handshake s1) (Handshake s2) = undefined
	-- This is where the magic will happen, giving a token passing implementation for free

  deepS _ = error "not possible to extract a Handshake without backedge"

----------------------------------------------------------------------------------------------------

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
			(Just True:rs,Just {}) -> fn ps xs rs         -- has been written
			(_:rs,_)               -> fn (0:ps) (x:xs) rs -- not written yet

	   fn (p:ps) xs c
		    = Nothing : case c of
			(Nothing:rs)         -> error "toVariableHandshake: bad protocol state (2)"
			(Just _:rs) 	     -> fn (pred p:ps) xs rs -- nothing read

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

----------------------------------------------------------------------------------------------------

-- | This function takes a ShallowFIFO object, and gives back a Handshake.
-- ShallowFIFO is typically connected to a data generator or source, like a file.

shallowFifoToHandshake :: (Show a, Rep a) => ShallowFIFO a -> IO (Handshake a)
shallowFifoToHandshake fifo = do
	xs <- getFIFOContents fifo
	return (toHandshake' (repeat 0) (xs ++ repeat Nothing))

handshakeToShallowFifo :: (Show a, Rep a) => ShallowFIFO a -> Handshake a -> IO ()
handshakeToShallowFifo fifo sink = do
	putFIFOContents fifo (fromHandshake' (repeat 0) sink)
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


