-- Stuff for the FED / LHC work

{-# LANGUAGE ScopedTypeVariables, RecursiveDo, KindSignatures, RankNTypes, GADTs, RecordWildCards, FlexibleInstances #-}
module FED where

import Language.KansasLava
import Language.KansasLava.Protocols
import Language.KansasLava.Utils
import Language.KansasLava.Fabric (ioFabric, observeFabric)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad
import System.IO
import Data.Monoid

import Data.Word
import Data.Sized.Ix
import Data.Sized.Unsigned
import Control.Concurrent.STM
import Control.Concurrent
import System.Environment

import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Monad.Trans.Class
import System.Random
import Control.Monad
import Control.Monad.Trans.Trace

--------------------------------------------------------------------------

data SendDatum a
        = SendUnknown
        | SendNoDatum
        | SendDatum a

sendDatum :: (Rep a)
          => IO (X Bool)
          -> (X a -> IO ())
          -> (X Bool -> IO ())
          -> SendDatum a
          -> IO Bool

sendDatum ready dat en cmd =
    case cmd of
      SendUnknown -> do
              _ <- ready        -- ignore the ready signal
              dat unknownX      -- send unknown
              en unknownX       -- send unknown enable
              return True
      SendNoDatum -> do
              _ <- ready        -- ignore the ready signal
              dat unknownX      -- send unknown
              en (pureX False)  -- send *no* enable
              return True
      SendDatum a -> do
              r <- ready        -- ignore the ready signal
              case unX r of
                 Nothing -> error "ready signal unknown in sendDatum"
                 Just False -> do
                         dat unknownX      -- send unknown
                         en (pureX False)  -- send *no* enable
                         return False
                 Just True -> do
                         dat (pureX a)     -- send value
                         en (pureX True)  -- send an enable
                         return True


data RecvDatum
        = RecvUnknown
        | RecvNoDatum
        | RecvDatum


recvDatum :: (Rep a)
          => (X Bool -> IO ())
          -> IO (X a)
          -> IO (X Bool)
          -> RecvDatum
          -> IO (Maybe a)

recvDatum ready dat en cmd =
   case cmd of
      RecvUnknown -> do
              ready unknownX
              _ <- dat
              _ <- en
              return Nothing
      RecvNoDatum -> do
              ready (pureX False)
              _ <- dat
              _ <- en
              return Nothing
      RecvDatum -> do
              ready (pureX True)
              d <- dat
              e <- en
              return $ case unX e of
                Nothing -> error "enabled undefined in recvDatum"
                Just True -> case unX d of
                  Nothing -> error "enabled set, undefined value in recvDatum"
                  Just a -> Just a
                Just False -> Nothing


--------------------------------------------------------------------------

data Reply a = Reply (a -> IO ())
data Ret  a = Ret a
        deriving Show

dut_interp :: (f Reply -> IO (f Ret)) -> MVar (Maybe (f Reply)) -> IO [f Ret]
dut_interp callout cmd_var = loop
  where
     loop = do
        opt_cmd <- takeMVar cmd_var
        case opt_cmd of
          Nothing -> return []
          Just cmd -> do ret <- callout cmd
                         rest <- unsafeInterleaveIO loop
                         return (ret : rest)
