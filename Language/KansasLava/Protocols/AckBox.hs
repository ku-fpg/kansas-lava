{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies,
  TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes,
  UndecidableInstances #-}


-- | This module implements an Ack protocol. In this producer/consumer model,
-- the producer drives the data input of the consumer, using an enable to
-- indicate that data is present. The producer will then keep the data value
-- steady until it receives an Ack from the consumer, at which point it's free
-- to drive the data input with a different value. This assumes that consumer
-- latches the input, as it may change.
module Language.KansasLava.Protocols.AckBox where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Types
import Language.KansasLava.Protocols.Patch
import Language.KansasLava.Utils
import Language.KansasLava.Probes

import Data.Maybe  as Maybe

import qualified Prelude
import Prelude hiding (tail, lookup)


------------------------------------------------------------------------------------


{- The convention with handshaken signals is
  ...
 -> (lhs_inp, rhs_inp)
 -> (lhs_out, rhs_out)

OR

 -> (lhs_inp, control_in, rhs_inp)
 -> (lhs_out, control_out, rhs_out)

-}


-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toAckBox :: (Rep a, Clock c, sig ~ Signal c)
         =>  Patch [Maybe a]  			(sig (Enabled a))
	           ()				(sig Ack)

toAckBox = toAckBox' []

-- | An AckBox producer that will go through a series of wait states after each
-- time it drives the data output.
toAckBox' :: (Rep a, Clock c, sig ~ Signal c)
             => [Int]		    -- ^ list wait states after every succesful post
             -> Patch [Maybe a] 		(sig (Enabled a))
		      ()			(sig Ack)

toAckBox' pauses ~(ys,ack) = ((),toS (fn ys (fromS ack) pauses))
        where
--           fn xs cs | trace (show ("fn",take  5 cs,take 5 cs)) False = undefined
	   -- send the value *before* checking the Ack

           fn xs ys' [] = fn xs ys' (repeat 0)
           fn (x:xs) ys' (0:ps) = x :
                case (x,ys') of
                 (_,Nothing:_)          -> error "toAckBox: bad protocol state (1)"
                 (Just _,Just (Ack True) :rs) -> fn xs rs ps          -- has been written
                 (Just _,Just (Ack False):rs) -> fn (x:xs) rs (0:ps)  -- not written yet
                 (Nothing,Just _:rs)    -> fn xs rs ps    	      -- nothing to write (choose to use pause, though)
                 (_,[])                 -> error "toAckBox: can't handle empty list of values to receive"
           fn (x:xs) rs (p:ps) = Nothing :
		case x of
				-- Allow extra Nothings to be consumed in the gaps
		   Nothing -> fn xs (Prelude.tail rs) (pred p:ps)
		   Just {} -> fn (x:xs) (Prelude.tail rs) (pred p:ps)
           fn [] ys' ps = fn (Prelude.repeat Nothing) ys' ps


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I'm sure this space-leaks.
fromAckBox :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
           => Patch (sig (Enabled a))		[Maybe a]
		    (sig Ack)			()
fromAckBox = fromAckBox' []

-- | An ackBox that goes through a series of intermediate states each time
-- consumes a value from the input stream and then issues an Ack.
fromAckBox' :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
           => [Int]
           -> Patch (sig (Enabled a))		[Maybe a]
		    (sig Ack)			()
fromAckBox' pauses ~(inp,_) = (toS (map fst internal), map snd internal)
   where
        internal = fn (fromS inp) pauses

	-- pretty simple API
	fn :: [Maybe (Enabled a)] -> [Int] -> [(Ack,Maybe a)]
	fn xs                 []     = fn xs (repeat 0)
        fn (Nothing:_)        _      = error "found an unknown value in AckBox input"
        fn (Just Nothing:xs)  ps     = (Ack False,Nothing) : fn xs ps
	fn (Just (Just v):xs) (0:ps) = (Ack True,Just v)   : fn xs ps
	fn (_:xs)             (p:ps) = (Ack False,Nothing) : fn xs (pred p:ps)
	fn []                 _      = error "fromAckBox: ack sequences should never end"

---------------------------------------------------------------------------
-- | 'enableToAckBox' turns an Enabled signal into a (1-sided) Patch.
enabledToAckBox :: (Rep a, Clock c, sig ~ Signal c)
	       => Patch (sig (Enabled a))    (sig (Enabled a))
		        ()  		     (sig Ack)
enabledToAckBox ~(inp,ack) = ((),res)
	where
		res = register Nothing
		    $ cASE [ (isEnabled inp,inp)
			   , (fromAck ack, disabledS)
			   ] res

-- | 'ackBoxToEnabled' turns the AckBox protocol into the Enabled protocol.
-- The assumptions is the circuit on the right is fast enough to handle the
-- streamed data.
ackBoxToEnabled :: (Rep a, Clock c, sig ~ Signal c)
	       => Patch (sig (Enabled a))    (sig (Enabled a))
		        (sig Ack) 	     ()
ackBoxToEnabled ~(inp,_) = (toAck ack,out)
   where
	out = inp
	ack = isEnabled inp



-- | This introduces protocol-compliant delays (in the shallow embedding)
shallowAckBoxBridge :: forall sig c a . (Rep a, Clock c, sig ~ Signal c, Show a)
                       => ([Int],[Int])
                       -> Patch (sig (Enabled a))		(sig (Enabled a))
				(sig Ack)		 	(sig Ack)
shallowAckBoxBridge (lhsF,rhsF) = patch
  where
	patch = fromAckBox' lhsF $$ toAckBox' rhsF


-- | 'probeAckBoxPatch' creates a patch with a named probe, probing the data and ack
-- signals in an Ack interface.

probeAckBoxP :: forall sig a c . (Rep a, Clock c, sig ~ Signal c)
    => String
    -> Patch (sig (Enabled a))   (sig (Enabled a))
             (sig Ack)           (sig Ack)
probeAckBoxP probeName ~(inp, ack_in) = (ack_out, out)
  where
      out          = inp
      (_, ack_out) = unpack probed

      probed :: sig (Enabled a, Ack)
      probed = probeS probeName $ pack (inp, ack_in)


-- A simple way of running a patch
runAckBoxP :: forall sig c a b . (Clock c, sig ~ Signal c, c ~ (), Rep a, Rep b)
	=> Patch (sig (Enabled a)) 	(sig (Enabled b))
		 (sig Ack)		(sig Ack)
	-> [a] -> [b]
runAckBoxP p as = [ b | Just b <- bs' ]
  where
	as' = map Just as
	bs' = runP (outputP as' $$ toAckBox $$ unitClockP $$ p $$ fromAckBox)


-- | A sink patch throws away its data input (generating a () data
-- output). 'sinkReadyP' uses an enabled/ack protocol.
sinkAckP :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
    => Patch    (sig (Enabled a))           ()
                (sig Ack)                   ()
sinkAckP ~(inp, ()) = (toAck ack, ())
  where
        (ack,_) = unpack inp

-------------------------------------------------------------------------------
-- Source Patches - generate a stream of constant values
-------------------------------------------------------------------------------


-- | A source patch takes no input and generates a stream of values. It
-- corresponds to a top-level input port. 'sourceReadyP' uses the enabled/ack
-- protocol.

alwaysAckP :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
    => a
    -> Patch    ()           (sig (Enabled a))
                ()           (sig Ack)
alwaysAckP baseVal ~((), _) = ((), out)
  where
        out = packEnabled high (pureS baseVal)

------------------------------------------------

-- | stub, no data ever sent.
neverAckP :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
    => Patch    ()           (sig (Enabled a))
                ()           (sig Ack)
neverAckP (_,_) = ((),disabledS)

