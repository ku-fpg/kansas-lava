{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, TypeOperators, RecursiveDo #-}
module Language.KansasLava.Protocols (
	module Language.KansasLava.Protocols.Enabled,
	module Language.KansasLava.Protocols.Memory,
	module Language.KansasLava.Protocols.Types,
	Response(..)
	) where

import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Protocols.Types

----
import Language.KansasLava.Types
import Language.KansasLava.Rep
import Language.KansasLava.Signal

import Control.Concurrent
import Control.Concurrent.STM


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

  txProtocolS :: (Rep a, Clock c) => TMVar a -> Signal c Bool -> Signal c r           -> IO (Signal c (Enabled a))
  rxProtocolS :: (Rep a, Clock c) => TMVar a -> Signal c Bool -> Signal c (Enabled a) -> IO (Signal c r)

instance Response Ready where

  txProtocolS var enB readyS = do
          ready <- newEmptyMVar
          writeIOS readyS $ putMVar ready

          out <- newEmptyMVar
          r <- readIOS $ takeMVar out

          let loop (en:ens) = do
                  r <- takeMVar ready  -- read the ready signal *first*
                        -- This depends of unpureX not being called for non-enabled iterations
                  o <- case (en, unpureX r) of
                    (Just True, Ready True) -> atomically $ tryTakeTMVar var
                    _                       -> return Nothing
                  putMVar out (pureX o)
                  loop ens
          _ <- ($) forkIO $ loop $ fromS enB

          return r

  rxProtocolS var enB rxS = do
          ready <- newEmptyMVar
          r <- readIOS $ takeMVar ready

          rx <- newEmptyMVar
          writeIOS rxS $ putMVar rx

          let loop (en:ens) = do
                  okay <- case en of
                            Just True -> atomically $ isEmptyTMVar var
                            _         -> return False
                  putMVar ready (pureX (Ready okay))
                  o <- takeMVar rx
                  case okay of
                    True -> do
                      case unpureX o of
                        Nothing -> return ()
                                -- Should never block
                        Just v  -> atomically $ putTMVar var v
                    False -> return ()
                  loop ens

          _ <- ($) forkIO $ loop $ fromS enB

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
         f a b = mkShallowXS $ fmap pureX $ takeWithResponse (fmap unpureX (shallowXS a)) (fmap unpureX (shallowXS b))

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
         f a = (mkShallowXS $ fmap pureX r, mkShallowXS $ fmap pureX $ val)
           where
                (r,val) = S.unzip (putWithResponse $ fmap unpureX (shallowXS a))

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
