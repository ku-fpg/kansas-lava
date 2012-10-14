-- Stuff for the FED / LHC work

{-# LANGUAGE ScopedTypeVariables, BangPatterns, RecursiveDo, UndecidableInstances, FlexibleContexts, KindSignatures, RankNTypes, GADTs, RecordWildCards, FlexibleInstances #-}
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
import Data.Maybe

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


-- All this is now inside dut-check.

{-
--------------------------------------------------------------------------
-- The Monad

data FifoM :: ((* -> *) -> *) -> * -> * where
        FifoM :: (Env f -> IO a) -> FifoM f a

instance Monad (FifoM f) where
        return a = FifoM $ \ env -> return a
        (FifoM m) >>= k = FifoM $ \ env -> do
                r <- m env
                case k r of
                  FifoM m -> m env

instance MonadIO (FifoM f) where
        liftIO m = FifoM $ \ _ -> m

runFifoM :: String -> (f Reply -> IO (f Ret)) -> FifoM f () -> IO ()
runFifoM seed action prog = do
        cmd_var <- newEmptyMVar
        let std0 :: StdGen = read seed
        var <- newMVar std0
        env <- return $ Env
          { env_rand = do
                std <- takeMVar var
                let (n,std') = random std
                putMVar var std'
                return n
          , env_randR = \ (a,b) -> do
                std <- takeMVar var
                let (n,std') = randomR (a,b) std
                putMVar var std'
                return n
          , the_cmds = cmd_var
          }

        forkIO $ case prog of
           FifoM m -> do m env

        dut_interp action cmd_var

--        if n `mod`  == 0 then do { putChar '.' ; hFlush stdout } else return ()

dut_interp :: (f Reply -> IO (f Ret)) -> MVar (StepCmd f) -> IO ()
dut_interp callout cmd_var = loop 0 []
  where
     loop n checked = do
        if n `mod` 10000 == 0 then do { putChar '.' ; hFlush stdout } else return ()
        -- get step commands
        (props,opt_cmd) <- takeStepCmd cmd_var
        case opt_cmd of
          Nothing -> return ()
                         -- do step
          Just cmd -> do ret <- callout cmd
                         props' <- sequence [ mkProp prop | prop <- props ]
                         loop2 n ret (props' ++ checked)

     loop2 n ret checked = do
            answers <- sequence [ f ret | f <- checked ]
            let result = Prelude.and answers -- better all be true
            if result then do loop (n+1) checked
                      else do -- failed! Give error messsage
                              print ("failed at #",n)
                              return ()

mkProp :: Prop f -> IO (f Ret -> IO Bool)
mkProp (Prop nm f) = do
        in_var <- newEmptyMVar
        out_var <- newEmptyMVar
        forkIO $ do
                let in_loop = do
                        state <- takeMVar in_var
                        states <- unsafeInterleaveIO in_loop
                        return $ state : states
                    -- You need the ! pattern here to make sure
                    -- the unsafeInterleaveIO is consumed by
                    -- *this* thread.
                    loop ((!o):os) = do
--                        print o
                        putMVar out_var o
                        loop os
                    loop [] = return ()
                ins <- in_loop
                loop (f ins)

        return $ \ state -> do
                putMVar in_var state
                takeMVar out_var


--------------------------------------------------------------------------

data Reply a = Reply (a -> IO ())
data Ret  a = Ret a
        deriving Show

--------------------------------------------------------------------------
-- A prop is something that should always be true for a specific test.
data Prop f = Prop String ([f Ret] -> [Bool])

data StepCmd f
        = StepDone               -- 1 cycle, no more commands
        | StepCmd (f Reply)      -- 1 cycle
        | RegProp (Prop f)       -- no cycles


takeStepCmd :: MVar (StepCmd f) -> IO ([Prop f],Maybe (f Reply))
takeStepCmd var = loop []
  where
        loop regs = do
--                print "take step"
                v <- takeMVar var
--                print "taken step"
                case v of
                   StepDone -> return (regs,Nothing)
                   StepCmd cmd -> return (regs,Just cmd)
                   RegProp prop -> loop (regs ++ [prop])


data Env f = Env
        { env_rand  :: forall r . (Random r) => IO r -- a random number generator
        , env_randR :: forall r . (Random r) => (r,r) -> IO r
        , the_cmds  :: MVar (StepCmd f)  -- where to send the commands
        }

parFifoM :: (Monoid (f Reply)) => [ FifoM f () ] -> FifoM f ()
parFifoM ms = FifoM $ \ env -> do
        vs <- sequence
                  [ do v <- newEmptyMVar
                       forkIO $ do f (env { the_cmds = v })
                                   forever $ putMVar v StepDone
                       return v
                  | FifoM f <- ms
                  ]

        -- returns when all the commands
        let loop = do
                (regs,vals) <- liftM unzip $ sequence [ takeStepCmd v | v <- vs ]
                case (concat regs,mconcat vals) of
                  ([],Nothing) -> return ()
                  (props,opt_cmd) -> do
                          sequence_ [ putMVar (the_cmds env) (RegProp prop)
                                    | prop <- props
                                    ]
                          putMVar (the_cmds env) $ case opt_cmd of
                                        Nothing -> StepDone
                                        Just cmd -> StepCmd cmd
                          loop


        -- call loop until all the sub-threads "die" (start issuing Nothing)
        loop

h i m = do print ("starting",i)
           r <- m
           print ("done",i)
           return r

---------------------------------------------------------
-- Monad Commands

wait :: Monoid (f Reply) => Int -> FifoM f ()
wait 0 = return ()
wait n = do
        FifoM $ \ env -> putMVar (the_cmds env) (StepCmd $ mempty)
        wait (n - 1)

-- You need to use the reply argument (otherwise the takeMVar v hangs)
putCmd :: Monoid (f Reply) => (Reply b -> f Reply) -> FifoM f b
putCmd cmd = FifoM $ \ env -> do
        v <- newEmptyMVar
        putMVar (the_cmds env) (StepCmd $ cmd (Reply $ putMVar v))
        takeMVar v

rand :: (Random r) => FifoM f r
rand = FifoM $ \ env -> env_rand env

randR :: (Random r) => (r,r) -> FifoM f r
randR (a,b) = FifoM $ \ env -> env_randR env (a,b)

property :: Prop f -> FifoM f ()
property prop = FifoM $ \ env -> do
        putMVar (the_cmds env) (RegProp prop)
-}