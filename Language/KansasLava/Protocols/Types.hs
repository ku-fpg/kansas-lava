{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies,
  TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes,
  UndecidableInstances, TypeOperators #-}

-- | This module provides types and functions for creating and manipulating
-- control signals (ready and ack) associated with protocols. The 'Ack' signal
-- indicates that a protocol element has received data from an upstream source,
-- and the 'Ready' signal indicates that the component is prepared to accept
-- data from an upstream source.
module Language.KansasLava.Protocols.Types where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils

import Control.Monad

-- It is preferable to be sending a message that expects an Ack,
-- but to recieve a message based on your Ready signal.

------------------------------------------------------------------------------------
-- | An Ack is always in response to an incoming packet or message.
newtype Ack = Ack { unAck :: Bool }
	deriving (Eq,Ord)

instance Show Ack where
	show (Ack True)  = "A"
	show (Ack False) = "~"


-- TODO: use $(repSynonym ''Ack ''Bool)

instance Rep Ack where
  data X Ack = XAckRep { unXAckRep :: X Bool }
  type W Ack = W Bool
  -- The template for using representations
  unX             = liftM Ack   . unX  . unXAckRep
  optX            = XAckRep     . optX . liftM unAck
  toRep           = toRep       . unXAckRep
  fromRep         = XAckRep     . fromRep
  repType Witness = repType (Witness :: Witness Bool)
  showRep         = showRepDefault

-- | Convert a 'Bool' signal to an 'Ack' signal.
toAck :: (sig ~ Signal clk) => sig Bool -> sig Ack
toAck = coerce Ack

-- | Convert an 'Ack' to a 'Bool' signal.
fromAck :: (sig ~ Signal clk) => sig Ack -> sig Bool
fromAck = coerce unAck

------------------------------------------------------------------------------------
-- | An Ready is always in response to an incoming packet or message
newtype Ready = Ready { unReady :: Bool }
	deriving (Eq,Ord)

instance Show Ready where
	show (Ready True)  = "R"
	show (Ready False) = "~"

instance Rep Ready where
  data X Ready = XReadyRep { unXReadyRep :: X Bool }
  type W Ready = W Bool
  -- The template for using representations
  unX             = liftM Ready   . unX  . unXReadyRep
  optX            = XReadyRep     . optX . liftM unReady
  toRep           = toRep         . unXReadyRep
  fromRep         = XReadyRep     . fromRep
  repType Witness = repType (Witness :: Witness Bool)
  showRep         = showRepDefault

-- | Convert a Bool signal to a 'Ready' signal.
toReady :: (sig ~ Signal clk) => sig Bool -> sig Ready
toReady = coerce Ready

-- | Convert a 'Ready' signal to a Bool signal.
fromReady :: (sig ~ Signal clk) => sig Ready -> sig Bool
fromReady = coerce unReady

------------------------------------------------------------------------------------------------------------------------------------------------

