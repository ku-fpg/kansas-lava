{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances #-}
module Language.KansasLava.Protocols (
	module Language.KansasLava.Protocols.Enabled,
	module Language.KansasLava.Protocols.Memory,
	module Language.KansasLava.Protocols.HandShake,
	module Language.KansasLava.Protocols.MailBox,
	bridge,
	shallowFIFO,
	) where

import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Protocols.HandShake
import Language.KansasLava.Protocols.MailBox

import Language.KansasLava.Rep
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Seq


---------------------------------------------------------------------------

-- | A 'bridge' is way of connecting a HandShake to a MailBox.
-- This is efficent; internally an 'and' and  a 'not' gate.

bridge :: (Rep a, Clock c, sig ~ CSeq c)
	=> (sig (Enabled a),sig Full) 
        -> (sig Ack, sig (Enabled a))
bridge (inp,full) = (toAck ack,out)
   where
	ack = isEnabled inp `and2` bitNot (fromFull full)
	out = packEnabled ack (enabledVal inp)

-- | A (shallow only) infinite FIFO, for connecting
-- MailBox's on the left to HandShake's on the right.
-- If you need a synthesizable FIFO, you can find one in the kansas-lava-cores package.

shallowFIFO :: (Rep a, Clock c, sig ~ CSeq c)
	=> (sig (Enabled a),sig Ack) 
        -> (sig Full, sig (Enabled a))
shallowFIFO (inp,ack) = (full,toHandShake (Nothing:vals) ack)
   where
	(full,vals) = fromMailBox inp 

