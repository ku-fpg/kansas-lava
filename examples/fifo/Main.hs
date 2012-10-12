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

        let dut_proxy :: SuperFabric IO ()
            dut_proxy = do
                hReaderFabric hState
                        [ OUT (outStdLogic       "i_ready" :: Seq Bool -> SuperFabric IO ())
                        , OUT (outStdLogicVector "proxy_clk" :: Seq () -> SuperFabric IO ())
                        ]
                hWriterFabric hIn
                        [ IN (inStdLogicVector   "i0"      :: SuperFabric IO (Seq U8))
                        , IN (inStdLogic         "i_valid" :: SuperFabric IO (Seq Bool))
                        , IN (inStdLogic         "o_ready" :: SuperFabric IO (Seq Bool))
                        ]
                hReaderFabric hOut
                        [ OUT (outStdLogicVector "o0"      :: Seq U8   -> SuperFabric IO ())
                        , OUT (outStdLogic       "o_valid" :: Seq Bool -> SuperFabric IO ())
                        ]




        v0 <- atomically $ newEmptyTMVar
        v1 <- atomically $ newEmptyTMVar
        vClk <- atomically $ newEmptyTMVar
        vCount <- atomically $ newTVar 0

        let en :: Seq Bool
            en = toS (cycle (take 10 (repeat False) ++ take 10 (repeat True)))

        let dut_driver :: IO DUT
            dut_driver = do

                var_i_ready <- newEmptyMVar
--                var_proxy_tick <- newEmptyMVar
                var_i0 <- newEmptyMVar

                hReaderFabric hState
                        [ OUT (flip writeIOS $ putMVar var_i_ready :: Seq Bool -> IO ())
--                        , OUT (flip writeIOS $ putMVar var_proxy_tick :: Seq () -> IO ())
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

--                var_proxy_counter <- newEmptyMVar
--                forkIO $ let loop n = do { takeMVar var_proxy_clk ; putMVar var_proxy_counter n ; loop (n+1) }
--                         in loop 1

                return $
                  DUT { dut_i_ready          = takeMVar var_i_ready
--                      , dut_proxy_tick       = takeMVar var_proxy_tick

                      , dut_i0               = putMVar var_i0
                      , dut_i_valid          = putMVar var_i_valid
                      , dut_o_ready          = putMVar var_o_ready

                      -- DUT out
                      , dut_o0               = takeMVar var_o0
                      , dut_o_valid          = takeMVar var_o_valid
                      }

        dut <- dut_driver
        hl_dut <- dutToHLdut dut

        cmd_var <- newEmptyMVar
        event_var <- newEmptyMVar

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

        std0 <- getStdGen
        var <- newMVar std0
        cmd_var <- newEmptyMVar
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


        forkIO $ do case prog of
                       FifoM m -> m env
                    print "done"

        -- Show the events, please


        forkIO $ interp hl_dut cmd_var event_var

        -- This runs in the main thread
        events <- sequence [ takeMVar event_var | _ <- [1..100]]

        print $ length events

        print $ prop_fifo events

        return ()

{-
        let loop0 :: Integer -> TraceT FifoE FifoM ()
            loop0 n = do
                liftIO $ print ("loop0",n)
                randR (0::Int,10) >>= wait
                randR (0::Int,255) >>= send
       --                atomically $ putTMVar v0 (fromIntegral (n :: Integer) :: U8)
                if n > 10 then return () else loop0 (n+1)

        let loop1 = do
                v :: U8 <- atomically $ takeTMVar v1
--                threadDelay (100 * 1000)
--                threadDelay (1000)
                print ("loop1",v)
                loop1

        let interleave ~(FifoM f) = FifoM $ unsafeInterleaveIO . f

        forkIO $ do
                let FifoM f = interp1 interleave (loop0 0) :: FifoM (Trace FifoE ())
                a <- f env
                print a

        loop1
-}

------------------------------------------------------------------

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

data Recv'dDatum


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
        , the_cmds  :: MVar (Maybe (FifoCmd Reply))  -- where to send the commands
                -- Nothing is this "thread" is done
                -- Just cmd is try execute the command, please

        }

parFifoM :: [ FifoM () ] -> FifoM ()
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

send :: U8 -> Int -> FifoM Bool
send d 0 = return False
send d n = do
        r <- putCmd $ \ reply -> mempty { send1 = Just (d,reply) }
        case r of
          True  -> return True
          False -> send d (n-1)

recv :: Int -> FifoM (Maybe U8)
recv 0 = return Nothing
recv n = do
        r <- putCmd $ \ reply -> mempty { recv1 = Just reply }
        case r of
          Nothing -> recv (n-1)
          Just r -> return (Just r)

wait :: Int -> FifoM ()
wait 0 = return ()
wait n = do
        putCmd $ \ reply -> mempty
        wait (n - 1)

putCmd :: (Reply b -> FifoCmd Reply) -> FifoM b
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

data Reply a = Reply (a -> IO ())
data Ret  a = Ret a
        deriving Show

-- data Cmd a b = Cmd a (b -> IO ())

interp :: HL_DUT -> MVar (Maybe (FifoCmd Reply)) -> MVar (FifoCmd Ret) -> IO ()
interp (HL_DUT { .. }) cmd_var event_var = loop 0
  where
     loop n = do
        opt_cmd <- takeMVar cmd_var
        case opt_cmd of
          Nothing -> return ()
          Just cmd -> loop' n cmd

     loop' n (FifoCmd { .. }) = do

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

        putMVar event_var $ FifoCmd
                { send1 = send1'
                , recv1 = recv1'
                }

        loop (n+1)


data FifoState = FifoState
        { fifo_contents :: [U8]
        }

-- This is the key concept, correctnes of the FIFO.

prop_fifo :: [FifoCmd Ret] -> Bool
prop_fifo cmds = and $ zipWith (==) xs ys
  where
          xs = [ u | FifoCmd { send1 = Just (u,Ret True) } <- cmds ]
          ys = [ u | FifoCmd { recv1 = Just (Ret (Just u)) } <- cmds ]
{-
correctness (FifoCmd { send1 = Nothing,              recv1 = Nothing               }) state = state
correctness (FifoCmd { send1 = Just (u,Reply False), recv1 = Nothing               }) state = state
correctness (FifoCmd { send1 = Just (u,Reply True),  recv1 = Nothing               }) state = state
correctness (FifoCmd { send1 = Nothing,              recv1 = Just (Reply Nothing)  }) state = state
correctness (FifoCmd { send1 = Just (u,Reply False), recv1 = Just (Reply Nothing)  }) state = state
correctness (FifoCmd { send1 = Just (u,Reply True),  recv1 = Just (Reply Nothing)  }) state = state
correctness (FifoCmd { send1 = Nothing,              recv1 = Just (Reply (Just w)) }) state = state
correctness (FifoCmd { send1 = Just (u,Reply False), recv1 = Just (Reply (Just w)) }) state = state
correctness (FifoCmd { send1 = Just (u,Reply True),  recv1 = Just (Reply (Just w)) }) state = state
-}

