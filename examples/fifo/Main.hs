{-# LANGUAGE ScopedTypeVariables, RecursiveDo, KindSignatures, RankNTypes, GADTs #-}
module Main where

import Language.KansasLava
import Language.KansasLava.Protocols
import Language.KansasLava.Utils
import Language.KansasLava.Fabric (ioFabric, observeFabric)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad
import System.IO

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

        let proto_driver :: SuperFabric IO ()
            proto_driver = do

               clk :: Seq () <- inStdLogicVector "proxy_clk"
               liftIO $ writeIOS clk (const $ atomically $ putTMVar vClk ())

               rec i_ready :: Seq Ready <- inStdLogic "i_ready"
                   val_i :: Seq (Enabled U8) <- liftIO $ txProtocolS v0 en i_ready
                   outStdLogicVector "i0"      (enabledVal val_i :: Seq U8)
                   outStdLogic       "i_valid" (isEnabled val_i :: Seq Bool)

               rec o_ready :: Seq Ready <- liftIO $ rxProtocolS v1 en val_o
                   outStdLogicVector "o_ready" (o_ready :: Seq Ready)
                   o0      :: Seq U8   <- inStdLogicVector "o0"
                   o_valid :: Seq Bool <- inStdLogic "o_valid"
                   let val_o   :: Seq (Enabled U8) = packEnabled o_valid o0

               return ()

        -- New stuff

        forkIO $ let loop n = do
                        atomically $ do
                                () <- takeTMVar vClk
                                writeTVar vCount n
                        loop (n+1)
                 in loop 1



        v <- newMVar ()
        setProbesAsTrace $ \ str -> do
                takeMVar v
                appendFile "DUT_PROXY_FABRIC" str
                putMVar v ()

        runFabricWithDriver (observeFabric probeS dut_proxy) proto_driver

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
          , in_val = v0
          , the_clk = vCount
          }

{-        let stuff = do
                i <- randR (0::Int,10)
                if i == 0 then reset
                          else loop

            loop = do
                w <- randR (0::Int,100)
                wait w
                d <- randR (0::Int,255)
                b <- send d
                if b then stuff
                     else loop

        let interleave ~(FifoM f) = FifoM $ unsafeInterleaveIO . f
        let FifoM f = interp1 interleave stuff
        a <- f env
--        print a
        print a
        return ()
-}



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

------------------------------------------------------------------

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

------------------------------------------------------------------



