{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, TypeOperators #-}
module Language.KansasLava.Protocols.Types where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils

import Control.Monad

-- It is preferable to be sending a message that expects an Ack,
-- but to recieve a message based on your Ready signal.

------------------------------------------------------------------------------------
-- An Ack is always in response to an incoming packet or message.
newtype Ack = Ack { unAck :: Bool }
	deriving (Eq,Ord)
	
instance Show Ack where
	show (Ack True)  = "A"
	show (Ack False) = "~"

instance Rep Ack where
  data X Ack = XAckRep { unXAckRep :: (X Bool) }
  type W Ack = W Bool
  -- The template for using representations
  unX             = liftM Ack   . unX  . unXAckRep
  optX            = XAckRep     . optX . liftM unAck 
  toRep           = toRep       . unXAckRep
  fromRep         = XAckRep     . fromRep
  repType Witness = repType (Witness :: Witness Bool)
  showRep         = showRepDefault

toAck :: (Signal sig) => sig Bool -> sig Ack
toAck = coerce Ack

fromAck :: (Signal sig) => sig Ack -> sig Bool
fromAck = coerce unAck

------------------------------------------------------------------------------------
-- An Ready is always in response to an incoming packet or message
newtype Ready = Ready { unReady :: Bool }
	deriving (Eq,Ord)
	
instance Show Ready where
	show (Ready True)  = "R"
	show (Ready False) = "~"

instance Rep Ready where
  data X Ready = XReadyRep { unXReadyRep :: (X Bool) }
  type W Ready = W Bool
  -- The template for using representations
  unX             = liftM Ready   . unX  . unXReadyRep
  optX            = XReadyRep     . optX . liftM unReady 
  toRep           = toRep         . unXReadyRep
  fromRep         = XReadyRep     . fromRep
  repType Witness = repType (Witness :: Witness Bool)
  showRep         = showRepDefault

toReady :: (Signal sig) => sig Bool -> sig Ready
toReady = coerce Ready

fromReady :: (Signal sig) => sig Ready -> sig Bool
fromReady = coerce unReady

-------------------------------------------------------------------------

infixr 5 :>

data a :> b = a :> b

-------------------------------------------------------------------------

type Patch lhs_in 			-- the lhs input
	   lhs_out bot_out rhs_out 	-- the outputs, from l to r
			   rhs_in	-- the rhs input
	= (lhs_in,rhs_in) -> (lhs_out,bot_out,rhs_out)
