{-# LANGUAGE ScopedTypeVariables, RecursiveDo, KindSignatures, RankNTypes, GADTs, RecordWildCards, FlexibleInstances #-}
module Main where

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

import FED


main :: IO ()
main = do
        args <- getArgs
        main2 args

main2 :: [String] -> IO ()
main2 ["dut"] = do
        hState <- openFile "DUT_STATE" ReadWriteMode
        hIn    <- openFile "DUT_IN"    ReadWriteMode
        hOut   <- openFile "DUT_OUT"   ReadWriteMode

        let stmt :: STMT ()
            stmt = do
                    inp :: ReadReadyBox U8  <- connectReadReadyBox  "i0" "i_valid" "i_ready"
                    out :: WriteReadyBox U8 <- connectWriteReadyBox "o0" "o_valid" "o_ready"
                    VAR reg :: VAR U8 <- SIGNAL $ var 0

                    SPARK $ \ loop -> do
                            takeReadyBox inp $ \ v -> reg := v
                            putReadyBox out reg
                            GOTO loop

        let dut :: Fabric ()
            dut = compileToFabric stmt

        let dut_wrapper :: SuperFabric IO ()
            dut_wrapper = do
                hWriterFabric hState
                        [ IN (inStdLogic         "i_ready" :: SuperFabric IO (Seq Bool))
                        ]
                hReaderFabric hIn
                        [ OUT (outStdLogicVector "i0"      :: Seq U8   -> SuperFabric IO ())
                        , OUT (outStdLogic       "i_valid" :: Seq Bool -> SuperFabric IO ())
                        , OUT (outStdLogic       "o_ready" :: Seq Bool -> SuperFabric IO ())
                        ]
                hWriterFabric hOut
                        [ IN (inStdLogicVector   "o0"      :: SuperFabric IO (Seq U8))
                        , IN (inStdLogic         "o_valid" :: SuperFabric IO (Seq Bool))
                        ]

        runFabricWithDriver (ioFabric dut) dut_wrapper

        -- wait for 1000 seconds
        threadDelay $ 1000 * 1000 * 1000

        return ()

main2 ["driver"] = do
        hState <- openFile "DUT_STATE" ReadWriteMode
        hIn    <- openFile "DUT_IN"    ReadWriteMode
        hOut   <- openFile "DUT_OUT"   ReadWriteMode

        var_i_ready <- newEmptyMVar
        var_i0 <- newEmptyMVar

        hReaderFabric hState
                [ OUT (flip writeIOS $ putMVar var_i_ready :: Seq Bool -> IO ())
                ]
        var_i0 <- newEmptyMVar
        var_i_valid <- newEmptyMVar
        var_o_ready <- newEmptyMVar

        hWriterFabric hIn
                [ IN (readIOS (takeMVar var_i0)      :: IO (Seq U8))
                , IN (readIOS (takeMVar var_i_valid) :: IO (Seq Bool))
                , IN (readIOS (takeMVar var_o_ready) :: IO (Seq Bool))
                ]

        var_o0 <- newEmptyMVar
        var_o_valid <- newEmptyMVar

        hReaderFabric hOut
                [ OUT (flip writeIOS $ putMVar var_o0      :: Seq U8 -> IO ())
                , OUT (flip writeIOS $ putMVar var_o_valid :: Seq Bool -> IO ())
                ]

        let dut = DUT { dut_i_ready          = takeMVar var_i_ready

                      , dut_i0               = putMVar var_i0
                      , dut_i_valid          = putMVar var_i_valid
                      , dut_o_ready          = putMVar var_o_ready

                      -- DUT out
                      , dut_o0               = takeMVar var_o0
                      , dut_o_valid          = takeMVar var_o_valid
                      }

        hl_dut <- dutToHLdut dut

        let sender =
                let loop n = do
                        send n 10
                        loop (n+1)
                 in loop 0

        let recvr = forever $ do
                 d <- recv 10
                 liftIO $ print ("recved",d)

        let prog =
                parFifoM
                        [ sender
                        , recvr
                        ]

        cmd_var <- newEmptyMVar
        forkIO $ runFifoM cmd_var prog

        -- Show the events, please

        events <- dut_interp (callout hl_dut) cmd_var

        -- This runs in the main thread
--        events <- sequence [ takeMVar event_var | _ <- [1..100]]

        print $ length (take 100 events)

        print $ prop_fifo (take 100 events)

        return ()

------------------------------------------------------------------

runFifoM cmd_var prog = do
        std0 <- getStdGen
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


        case prog of
           FifoM m -> m env

        return ()



-- This is the low level API into the DUT, reflecting the
-- VHDL almost directly. Note the tick, which is an extra
-- field that 'ticks'  every

data DUT = DUT
        -- DUT state
        { dut_i_ready           :: IO (X Bool)
--        , dut_proxy_tick        :: IO (X ())

        -- DUT in
        , dut_i0                :: X U8   -> IO ()
        , dut_i_valid           :: X Bool -> IO ()
        , dut_o_ready           :: X Bool -> IO ()

        -- DUT out
        , dut_o0                :: IO (X U8)
        , dut_o_valid           :: IO (X Bool)
        }

-- The high level API into the DUT.
-- Note you need to call these functions from top to bottom.

data HL_DUT = HL_DUT
        { proxy_counter         :: IO Integer

        -- inputs
        , i0                    :: SendDatum U8 -> IO Bool

        -- outputs
        , o0                    :: RecvDatum   -> IO (Maybe U8)
        }


dutToHLdut :: DUT -> IO HL_DUT
dutToHLdut (DUT { .. }) = do
    var_proxy_counter <- newEmptyMVar
    forkIO $ let loop n = do { putMVar var_proxy_counter n ; loop (n+1) }
              in loop 0

    return $ HL_DUT
        { proxy_counter = takeMVar var_proxy_counter
        , i0            = sendDatum dut_i_ready
                                    dut_i0
                                    dut_i_valid
        , o0            = recvDatum dut_o_ready
                                    dut_o0
                                    dut_o_valid
        }


------------------------------------------------------------------
{-
data FifoE :: * where
        SendEvent :: Maybe (U8,Integer) -> FifoE    -- val x cycle-sent
        WaitEvent :: Int -> FifoE
        ResetEvent :: FifoE

instance Show FifoE where
        show (SendEvent v) = "send(" ++ show v ++ ")"
        show (WaitEvent n)  = "pause(" ++ show n ++ ")"
        show (ResetEvent)  = "reset"

data FifoM :: * -> * where
        FifoM :: (Env -> IO a)          -> FifoM a

instance Monad FifoM where
        return a = FifoM $ \ env -> return a
        (FifoM m) >>= k = FifoM $ \ env -> do
                r <- m env
                case k r of
                  FifoM m -> m env

instance MonadIO FifoM where
        liftIO m = FifoM $ \ _ -> m

data Env = Env
        { env_rand  :: forall r . (Random r) => IO r -- a random number generator
        , env_randR :: forall r . (Random r) => (r,r) -> IO r
        , in_val    :: TMVar U8
        , the_clk   :: TVar Integer
        }


send :: Int -> TraceT FifoE FifoM Bool
send n = event $ FifoM $ \ env -> do
        let dat = fromIntegral n :: U8
        -- waits until sent
        tm <- atomically $ do
                putTMVar (in_val env) dat
                readTVar (the_clk env)
        return (SendEvent (Just (dat,tm)), True)

wait :: Int -> TraceT FifoE FifoM ()
wait n = event $ FifoM $ \ env -> do { return (WaitEvent n,()) }

reset :: TraceT FifoE FifoM ()
reset = event $ FifoM $ \ env -> do { return (ResetEvent,()) }

rand :: (Random r) => TraceT FifoE FifoM r
rand = lift $ FifoM $ \ env -> do { r <- env_rand env ; return r }

randR :: (Random r) => (r,r) -> TraceT FifoE FifoM r
randR (a,b) = lift $ FifoM $ \ env -> do { r <- env_randR env (a,b) ; return r }
-}

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

data Env f = Env
        { env_rand  :: forall r . (Random r) => IO r -- a random number generator
        , env_randR :: forall r . (Random r) => (r,r) -> IO r
        , the_cmds  :: MVar (Maybe (f Reply))  -- where to send the commands
                -- Nothing is this "thread" is done
                -- Just cmd is try execute the command, please
        }

parFifoM :: (Monoid (f Reply)) => [ FifoM f () ] -> FifoM f ()
parFifoM ms = FifoM $ \ env -> do
        vs <- sequence
                  [ do v <- newEmptyMVar
                       forkIO $ f (env { the_cmds = v })
                       return v
                  | FifoM f <- ms
                  ]

        -- returns when all the commands
        let loop = do
                vals <- sequence [ takeMVar v | v <- vs ]
                case mconcat vals of
                  Nothing -> return ()  -- done
                  Just cmd -> do
                          putMVar (the_cmds env) (Just cmd)
                          loop

        -- call loop until all the sub-threads "die" (start issuing Nothing)
        loop

send :: U8 -> Int -> FifoM FifoCmd Bool
send d 0 = return False
send d n = do
        r <- putCmd $ \ reply -> mempty { send1 = Just (d,reply) }
        case r of
          True  -> return True
          False -> send d (n-1)

recv :: Int -> FifoM FifoCmd (Maybe U8)
recv 0 = return Nothing
recv n = do
        r <- putCmd $ \ reply -> mempty { recv1 = Just reply }
        case r of
          Nothing -> recv (n-1)
          Just r -> return (Just r)

wait :: Int -> FifoM FifoCmd ()
wait 0 = return ()
wait n = do
        putCmd $ \ reply -> mempty
        wait (n - 1)

putCmd :: (Reply b -> FifoCmd Reply) -> FifoM FifoCmd b
putCmd cmd = FifoM $ \ env -> do
        v <- newEmptyMVar
        putMVar (the_cmds env) (Just $ cmd (Reply $ putMVar v))
        takeMVar v

------------------------------------------------------------------

data FifoCmd resp = FifoCmd
        { send1 :: Maybe (U8,resp Bool)
        , recv1 :: Maybe (resp (Maybe U8))
        }

instance Show (FifoCmd Ret) where
        show (FifoCmd { .. }) =
                "FifoCmd { send1 = " ++ show send1 ++
                        ", recv1 = " ++ show recv1 ++
                        "}"

instance Monoid (FifoCmd a) where
        mempty = FifoCmd
         { send1 = Nothing
         , recv1 = Nothing
         }
        mappend f1 f2 = FifoCmd
         { send1 = send1 f1 `join` send1 f2
         , recv1 = recv1 f1 `join` recv1 f2
         } where join Nothing   Nothing  = Nothing
                 join (Just a)  Nothing  = Just a
                 join Nothing   (Just b) = Just b
                 join _         _        = error "FifoCmd attempting to request Cmd twice"


-- All the logic that modifies the DUT is inside here.
-- This is called once per cycle.
callout :: HL_DUT -> FifoCmd Reply -> IO (FifoCmd Ret)
callout  (HL_DUT { .. }) (FifoCmd { .. }) = do
        send1' <- case send1 of
                   Nothing -> do
                           _ <- i0 SendNoDatum
                           return Nothing
                   Just (u8,Reply resp) -> do
                           r <- i0 (SendDatum u8)
                           resp r
                           return $ Just (u8,Ret r)

        recv1' <- case recv1 of
                   Nothing -> do
                           r <- o0 RecvNoDatum
                           return Nothing
                   Just (Reply resp) -> do
                           r <- o0 RecvDatum
                           resp r
                           return $ Just (Ret r)

        return $ FifoCmd
                { send1 = send1'
                , recv1 = recv1'
                }


-- This is the key concept, correctnes of the FIFO.

prop_fifo :: [FifoCmd Ret] -> Bool
prop_fifo cmds = and $ zipWith (==) xs ys
  where
          xs = [ u | FifoCmd { send1 = Just (u,Ret True) } <- cmds ]
          ys = [ u | FifoCmd { recv1 = Just (Ret (Just u)) } <- cmds ]
