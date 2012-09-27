{-# LANGUAGE ScopedTypeVariables, DoRec #-}
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

        let en :: Seq Bool
            en = toS (cycle (take 10 (repeat False) ++ take 10 (repeat True)))

        let proto_driver :: SuperFabric IO ()
            proto_driver = do
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

        v <- newMVar ()
        setProbesAsTrace $ \ str -> do
                takeMVar v
                appendFile "DUT_PROXY_FABRIC" str
                putMVar v ()

        runFabricWithDriver (observeFabric probeS dut_proxy) proto_driver

        let loop0 n = do
                print ("loop0",n)
                atomically $ putTMVar v0 (fromIntegral (n :: Integer) :: U8)
                if n > 100000 then return () else loop0 (n+1)

        let loop1 = do
                v :: U8 <- atomically $ takeTMVar v1
--                threadDelay (100 * 1000)
--                threadDelay (1000)
                print ("loop1",v)
                loop1

        forkIO $ loop0 0

        loop1



