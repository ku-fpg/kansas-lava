{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
module Main where

import Language.KansasLava
import Language.KansasLava.Protocols
import Language.KansasLava.Utils
import Control.Monad.Fix
import Control.Monad
import System.IO

import Data.Word
import Data.Sized.Ix
import Data.Sized.Unsigned
import Control.Concurrent.STM
import Control.Concurrent

main = do
        v0 <- newEmptyMVar
        v1 <- newEmptyMVar

        rec val       :: Seq (Enabled U32) <- txProtocolS v0 ready
            let val2  :: Seq (Enabled U32) = packEnabled (isEnabled val) val3
            let val3  :: Seq U32           = iterateS (+1) 0
            ready     :: Seq Ready        <- rxProtocolS v1 val2

        let loop0 n = do
--                print ("loop0",n)
                putMVar v0 n
                if n > 1000 then return () else loop0 (n+1)

        let loop1 = do
                v <- takeMVar v1
                threadDelay (100 * 1000)
                print ("loop1",v)
                loop1

        forkIO $ loop0 0

        loop1
