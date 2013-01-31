{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies,
  TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes,
  UndecidableInstances #-}

-- | This module implements an Ready protocol. In this producer/consumer model,
-- the consumer issues a Ready signal to the producer, at which time the
-- producer can drive the data input to the consumer, signaling data valid with
-- an Enable. The producer will hold the consumer data input steady until it
-- receives another Ready.
module Language.KansasLava.Protocols.ReadyBox where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Types
import Language.KansasLava.Protocols.Patch
import Language.KansasLava.Probes
import Language.KansasLava.Utils

------------------------------------------------------------------------------------

{-
{- The convention with ReadyBoxn signals is
  ...
 -> (lhs_inp, rhs_inp)
 -> (lhs_out, rhs_out)

OR

 -> (lhs_inp, control_in, rhs_inp)
 -> (lhs_out, control_out, rhs_out)

-}


-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toReadyBox :: (Rep a, Clock c, sig ~ Signal c)
         =>  Patch [Maybe a]  			(sig (Enabled a))
	           ()				(sig Ready)
toReadyBox = toReadyBox' []

-- | A readybox that goes through a sequence of intermediate states after
-- issuing each enable, and before it looks for the next Ready.
toReadyBox' :: (Rep a, Clock c, sig ~ Signal c)
             => [Int]		    -- ^ list wait states after every succesful post
             -> Patch [Maybe a]  			(sig (Enabled a))
		      ()				(sig Ready)
toReadyBox' pauses ~(ys,full) = ((),toS (fn ys (fromS full) pauses))
        where
--           fn xs cs ps | trace (show ("fn",take 5 ps)) False = undefined
	   -- send the value *before* checking the Ready
           fn xs fs ps =
                case fs of
                 (Nothing:_)              -> error "toReadyBox: bad protocol state (1)"
                 (Just (Ready True) : fs') ->
			case (xs,ps) of
			   (x:xs',0:ps')       -> x : fn xs' fs' ps'     -- write it (it may be Nothing)
			   (Nothing:xs',p:ps') -> Nothing : fn xs' fs' (pred p : ps')
			   (_:_,p:ps')         -> Nothing : fn xs fs' (pred p : ps')
			   (_:_,[])            -> fn xs fs (repeat 0)
			   (_,_)               -> Nothing : fn xs fs' ps  -- nothing to write
                 (Just (Ready False) : fs')    -> Nothing : fn xs fs' ps -- not ready yet
		 [] 			       -> error "toReadyBox: Ready seq should never end"


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I'm sure this space-leaks.
fromReadyBox :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
           => Patch (sig (Enabled a))		[Maybe a]
		    (sig Ready)			()
fromReadyBox = fromReadyBox' (repeat 0)

-- | Like fromReadyBox, but which goes through a series of intermediate states
-- after receiving an enable before issuing another Ready.
fromReadyBox' :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
           => [Int]
           -> Patch (sig (Enabled a))		[Maybe a]
		    (sig Ready)			()
fromReadyBox' ps ~(inp,_) = (toS (map fst internal), map snd internal)
   where
        internal = fn (fromS inp) ps

	-- pretty simple API
	fn :: [Maybe (Enabled a)] -> [Int] -> [(Ready,Maybe a)]
        fn xs (0:ps') = (Ready True,v) : rest
         where
	    (v,rest) = case xs of
			(Nothing:_)          -> error "found an unknown value in ReadyBox input"
        		(Just Nothing:xs')   -> (Nothing,fn xs' (0:ps'))	-- nothing read yet
			(Just v':xs')        -> (v',fn xs' ps')
			[]                   -> error "fromReadyBox: Ready sequences should never end"
        fn xs (p:ps') = (Ready False,Nothing) : fn (Prelude.tail xs) (pred p:ps')
	fn xs []      = fn xs (repeat 0)

-- | Introduces protocol-compliant delays (in the shallow embedding)
shallowReadyBoxBridge :: forall sig c a . (Rep a, Clock c, sig ~ Signal c, Show a)
                       => ([Int],[Int])
                       -> Patch (sig (Enabled a))		(sig (Enabled a))
				(sig Ready)		 	(sig Ready)
shallowReadyBoxBridge (lhsF,rhsF) = patch
  where
	patch = fromReadyBox' lhsF $$ toReadyBox' rhsF

-- | 'probeReadyBoxPatch' creates a patch with a named probe, probing the data and ready
-- signals in a Ready interface.
probeReadyBoxP :: forall sig a c . ( Rep a, Clock c, sig ~ Signal c)
    => String
    -> Patch (sig (Enabled a))   (sig (Enabled a))
             (sig Ready)         (sig Ready)
probeReadyBoxP probeName ~(inp, ready_in) = (ready_out, out)
    where
        (out, _)  = unpack probed
        ready_out = ready_in

        probed :: sig (Enabled a, Ready)
        probed = probeS probeName $ pack (inp, ready_in)

-- A simple way of running a patch
runReadyBoxP :: forall sig c a b . (c ~ CLK, sig ~ Signal c, Rep a, Rep b)
	=> Patch (sig (Enabled a)) 	(sig (Enabled b))
		 (sig Ready)		(sig Ready)
	-> [a] -> [b]
runReadyBoxP p as = [ b | Just b <- bs' ]
  where
	as' = map Just as
	bs' = runP (outputP as' $$ toReadyBox $$ globalClockP $$ p $$ fromReadyBox)

-- | A sink patch throws away its data input (generating a () data
-- output). 'sinkReadyP' uses an enabled/ready protocol.
sinkReadyP :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
    => Patch    (sig (Enabled a))           ()
                (sig Ready)                 ()
sinkReadyP ~(_, ()) = (toReady ready, ())
  where
        ready = high

-- | A source patch takes no input and generates a stream of values. It
-- corresponds to a top-level input port. 'alwaysReadyP' uses the
-- ready/enabled protocol.
alwaysReadyP :: forall a c sig . ( Rep a, Clock c, sig ~ Signal c)
    => a
    -> Patch    ()           (sig (Enabled a))
                ()           (sig Ready)
alwaysReadyP baseVal ~((), ready_in) = ((), out)
  where
        out = packEnabled (fromReady ready_in) (pureS baseVal)

-- | stub, no data ever sent.
neverReadyP :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
    => Patch    ()           (sig (Enabled a))
                ()           (sig Ready)
neverReadyP (_,_) = ((),disabledS)
-}