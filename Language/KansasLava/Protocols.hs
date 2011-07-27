{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, TypeOperators #-}
module Language.KansasLava.Protocols (
	module Language.KansasLava.Protocols.Enabled,
	module Language.KansasLava.Protocols.Memory,
	module Language.KansasLava.Protocols.HandShake,
	module Language.KansasLava.Protocols.MailBox,
	module Language.KansasLava.Protocols.Types,
	nullPatch,
	bridge,
	shallowFIFO,
	bus,
	(>==>),
	(>~~>),
	(>~=>)
	) where

import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Protocols.HandShake
import Language.KansasLava.Protocols.MailBox
import Language.KansasLava.Signal
import Language.KansasLava.Protocols.Types

import Language.KansasLava.Rep
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Seq

---------------------------------------------------------------------------

-- | A 'bridge' is way of connecting a HandShake to a MailBox.
-- This is efficent; internally an 'and' and  a 'not' gate,
-- and represents a good 'bridging' interface, because
-- both side can play master, and try initiate the transfer at the same time, 
-- improving possible clock speeds.

nullPatch :: Patch a 
		   b () a
		 	b 
nullPatch ~(a,b) = (b,(),a)


bridge :: (Rep a, Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled a)) 
		 (sig Ack) () (sig (Enabled a)) 
				(sig Ready) 
bridge (inp,ready) = (toAck ack,(),out)
   where
	ack = isEnabled inp `and2` fromReady ready
	out = packEnabled ack (enabledVal inp)

-- | A (shallow only) infinite FIFO, for connecting
-- MailBox's on the left to HandShake's on the right.
-- If you need a synthesizable FIFO, you can find one in the kansas-lava-cores package.

shallowFIFO :: (Rep a, Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled a)) 
		 (sig Ready) () (sig (Enabled a)) 
				(sig Ack) 
shallowFIFO (inp,ack) = (full,(),toHandShake (Nothing:vals) ack)
   where
	(full,vals) = fromMailBox inp 

------------------------------------------------------------------------

infixr 5 `bus`
bus :: (Signal sig) 
      => Patch li1 lo1  s1 o (sig i) 
                  -> Patch o (sig i) s2  ro2 ri2 
      -> Patch li1 lo1 (s1 :>        s2) ro2 ri2
(p1 `bus` p2) inp = (lhs_out1,(bot_out1 :> bot_out2),rhs_out2)
   where
	(lhs_in,rhs_in) 	     = inp
	(lhs_out1,bot_out1,rhs_out1) = p1 (lhs_in,lhs_out2)
	(lhs_out2,bot_out2,rhs_out2) = p2 (rhs_out1,rhs_in)

-- Mailbox based signal with Ready
infixr 5 >==>
(>==>) :: (Clock c, sig ~ CSeq c, back ~ Ready, o ~ Enabled a, Rep a)
      => Patch li1 lo1  s1 (sig o) (sig back) 
                  -> Patch (sig o) (sig back) s2  ro2 ri2 
      -> Patch li1 lo1 (s1 :>            s2) ro2 ri2
(>==>) = bus

-- HandShake-based signal with Ack
infixr 5 >~~>
(>~~>) :: (Clock c, sig ~ CSeq c, back ~ Ack, o ~ Enabled a, Rep a)
      => Patch li1 lo1  s1 (sig o) (sig back) 
                  -> Patch (sig o) (sig back) s2  ro2 ri2 
      -> Patch li1 lo1 (s1 :>            s2) ro2 ri2
(>~~>) = bus

-- HandShake-based left hand side, Mailbox based right hand side.
infixr 5 >~=>
(>~=>) :: (Clock c, sig ~ CSeq c, o ~ Enabled a, Rep a)
      => Patch li1 lo1  s1 (sig o) (sig Ack) 
                  -> Patch (sig o) (sig Ready) s2  ro2 ri2 
      -> Patch li1 lo1 (s1 :>            s2) ro2 ri2
p1 >~=> p2 = mapPatch (\ (a :> () :> b) -> (a :> b)) patch 
   where patch = p1 `bus` bridge `bus` p2

--------------------------------------------------------------------------------


mapPatch :: (a -> b) -> Patch lhs_in lhs_out a rhs_out rhs_in
 		     -> Patch lhs_in lhs_out b rhs_out rhs_in
mapPatch f p inp = let (lhs_out,bot_out,rhs_out) = p inp
		   in (lhs_out,f bot_out,rhs_out)


