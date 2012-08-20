{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Language.KansasLava
import Control.Monad.Fix
import Control.Monad
import System.IO

import Data.Word
import Data.Sized.Ix
import Data.Sized.Unsigned
import Control.Concurrent.STM
import Control.Concurrent


dut :: (MonadFix m) => SuperFabric m ()
dut = do
        i0 :: Seq U4 <- inStdLogicVector "i0"
        i1 :: Seq U4 <- inStdLogicVector "i1"
        let o0 = delay $ i0 + i1 :: Seq U4
        outStdLogicVector "o0" o0

-- This is basically the same as what we export for block 4,
-- but generated from Haskell.
exportableDUT :: IO ()
exportableDUT = do
        hIn <- openFile "DUT_IN" ReadWriteMode
        hOut <- openFile "DUT_OUT" ReadWriteMode
        setProbesAsTrace $ putStr
        let wrapper :: SuperFabric IO ()
            wrapper = do
                hReaderFabric hIn
                        [ OUT (outStdLogicVector "i0" . probeS "i0" :: Seq U4 -> SuperFabric IO ())
                        , OUT (outStdLogicVector "i1" . probeS "i1" :: Seq U4 -> SuperFabric IO ())
                        ]
                hWriterFabric hOut
                        [ IN (liftM (probeS "o0") $ inStdLogicVector "o0" :: SuperFabric IO (Seq U4))
                        ]

        runFabricWithDriver dut wrapper
        threadDelay $ 1000 * 1000 * 1000
        return ()

-- The external binary that acts like "BLOCK4" (but is an adder)
--main = exportableDUT
--main = tester

tester :: IO ()
tester = do
        -- The opposite of the one above
        hIn <- openFile "DUT_IN" ReadWriteMode
        hOut <- openFile "DUT_OUT" ReadWriteMode
        let tester_wrapper :: SuperFabric IO ()
            tester_wrapper = do
                hReaderFabric hOut
                        [ OUT (outStdLogicVector "o0" :: Seq U4 -> SuperFabric IO ())
                        ]
                hWriterFabric hIn
                        [ IN (inStdLogicVector "i0" :: SuperFabric IO (Seq U4))
                        , IN (inStdLogicVector "i1" :: SuperFabric IO (Seq U4))
                        ]


        let
--            dat :: [U16]
--            dat = [0..]
            dat :: [(U4,U4)]
            dat = [(a,b) | a <- [0..15], b <- [0..15]]

        let tester_driver :: SuperFabric IO (Seq U4)
            tester_driver = do
                    let xs = toS dat :: Seq (U4,U4)
                    let (x0,x1) :: (Seq U4,Seq U4) = unpack xs

--                    let x0 = toS dat :: Seq U16
--                    outStdLogicVector "i0" (0 :: Seq U16)
                    outStdLogicVector "i0" (x0 :: Seq U4)
                    outStdLogicVector "i1" (x1 :: Seq U4)

                    inStdLogicVector "o0" :: SuperFabric IO (Seq U4)

        r <- runFabricWithDriver tester_wrapper tester_driver
        print r

        threadDelay $ 1000 * 1000 * 1000



