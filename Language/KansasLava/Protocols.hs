{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, TypeOperators, DoRec #-}
module Language.KansasLava.Protocols (
	module Language.KansasLava.Protocols.Enabled,
	module Language.KansasLava.Protocols.Memory,
	module Language.KansasLava.Protocols.AckBox,
	module Language.KansasLava.Protocols.ReadyBox,
	module Language.KansasLava.Protocols.Types,
	module Language.KansasLava.Protocols.Patch,
	Response(..)
	) where

import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Protocols.AckBox
import Language.KansasLava.Protocols.ReadyBox
import Language.KansasLava.Protocols.Types
import Language.KansasLava.Protocols.Patch

----
import Language.KansasLava.Types
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import  qualified Language.KansasLava.Stream as S
import Language.KansasLava.Stream (Stream(..), cons)
import Language.KansasLava.Utils
import Data.Sized.Unsigned

import Control.Monad.ST
import Control.Concurrent
import Control.Concurrent.STM
import Data.STRef


--takeMVarS :: MVar (X a) -> IO (Signal i a)
--takeMVarS var = do
--        readIOS $ \ x -> takeMVar

--        :: (Clock i) => IO (X a) -> IO (Signal i a)
{-
data ProtocolVar r a = ProtocolVar (MVar a) (MVar ())

newProtocolVar :: (Response r) => IO (ProtocolVar r a)
newProtocolVar = do
        var <- newEmptyMVar
        rdy <- newEmptyMVar
        return $ ProtocolVar var rdy
  putProtocolVar  :: ProtocolVar r a -> a -> IO ()
  takeProtocolVar :: ProtocolVar r a -> IO a
-}

class Rep r => Response r where

  txProtocolS :: (Rep a, Clock c) => MVar a -> Signal c r           -> IO (Signal c (Enabled a))
  rxProtocolS :: (Rep a, Clock c) => MVar a -> Signal c (Enabled a) -> IO (Signal c r)

instance Response Ready where

  txProtocolS var readyS = do
          ready <- newEmptyMVar
          writeIOS readyS $ putMVar ready

          out <- newEmptyMVar
          r <- readIOS $ takeMVar out

          let loop = do
                  r <- takeMVar ready  -- read the ready signal *first*
                  case unpureX r of
                    Ready True -> do
                           o <- tryTakeMVar var
                           putMVar out (pureX o)
                           loop
                    Ready False -> do
                           putMVar out (pureX Nothing)
                           loop
          forkIO $ loop

          return r

  rxProtocolS var rxS = do
          ready <- newEmptyMVar
          r <- readIOS $ takeMVar ready

          rx <- newEmptyMVar
          writeIOS rxS $ putMVar rx

          let loop = do
                  okay <- isEmptyMVar var
                  putMVar ready (pureX (Ready okay))
                  case okay of
                    True -> do
                      o <- takeMVar rx
                      case unpureX o of
                        Nothing -> return ()
                        Just v  -> putMVar var v
                      loop
                    False -> do
                      _ <- takeMVar rx
                      loop

          forkIO $ loop

          return r


--  takeWithResponse :: Stream (Enabled a) -> Stream r -> Stream (Enabled a)
--  putWithResponse  :: Stream (Enabled a) -> Stream (r,Enabled a)

--  putTMVar  :: (Clock c, Rep a) => TMVar (X a) -> Signal c (Enabled a) -> IO (Signal c r)

{-
takeTMVarS :: forall r c a . (Response r, Clock c, Rep a) => TMVar a -> Signal c r           -> IO (Signal c (Enabled a))
takeTMVarS var sig = do
        res <- readIOS $ do
                v <- atomically $ tryTakeTMVar var
                case v of
                  Nothing -> return (optX (return Nothing))
                  Just x  -> return (optX (return (Just x)))
        return $ f res sig
    where
         f :: Signal c (Enabled a) -> Signal c r -> Signal c (Enabled a)
         f a b = mkShallowS $ fmap pureX $ takeWithResponse (fmap unpureX (shallowS a)) (fmap unpureX (shallowS b))

putTMVarS :: forall r c a . (Response r, Clock c, Rep a) => TMVar a -> Signal c (Enabled a) -> IO (Signal c r)
putTMVarS var sig = do
        let (r,val) = f sig
        writeIOS val $ \ x ->
                case unpureX x of
                  Just v -> atomically $ putTMVar var v
                  Nothing -> return ()
        return r

    where
	 f :: (Clock clk, Response r) => Signal clk (Enabled a) -> (Signal clk r,Signal clk (Enabled a))
         f a = (mkShallowS $ fmap pureX r, mkShallowS $ fmap pureX $ val)
           where
                (r,val) = S.unzip (putWithResponse $ fmap unpureX (shallowS a))

instance Response Ack where
  takeWithResponse xs0 ys0 =
          case S.uncons xs0 of
             (x0,xs1) -> cons x0 $ case (x0,S.uncons ys0) of
                                     (Just _,(Ack True,ys1))  -> takeWithResponse xs1 ys1
                                     (Just _,(Ack False,ys1)) -> takeWithResponse xs0 ys1
                                     (Nothing,(_,ys1))        -> takeWithResponse xs1 ys1      -- TODO, consider xs0

instance Response Ready where
  takeWithResponse xs0 ys0 =
          case S.uncons ys0 of
            (Ready True,ys1)  -> case S.uncons xs0 of
                                   (x0,xs1) -> cons x0 (takeWithResponse xs1 ys1)
            (Ready False,ys1) -> cons Nothing          (takeWithResponse xs0 ys1)
-}