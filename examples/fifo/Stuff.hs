{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Monad.Trans.Class
import System.Random
import Control.Monad
import Control.Monad.Trans.Trace

------------------------------------------------------------------

data Event :: * where
        SendEvent :: Int -> Bool -> Event
        WaitEvent :: Int -> Event
        ResetEvent :: Event

instance Show Event where
        show (SendEvent n b) = "send(" ++ show n ++ ")"
        show (WaitEvent n)  = "pause(" ++ show n ++ ")"
        show (ResetEvent)  = "reset"

data Env = Env (IO Int)         -- a random number generator

mkEnv :: IO Env
mkEnv = do
        std0 <- getStdGen
        var <- newMVar std0
        return $ Env $ do
                std <- takeMVar var
                let (n,std') = random std
                putMVar var std'
                return n

send :: Int -> TraceT Event IO Bool
send n = Event $ do { print ("send",n) ; return $ EventTrace (SendEvent n True) True }

wait :: Int -> TraceT Event IO ()
wait n = Event $ do { print ("wait",n) ; return $ EventTrace (WaitEvent n) () }

reset :: TraceT Event IO ()
reset = Event $ do { return $ EventTrace (ResetEvent) () }

rand :: Env -> TraceT Event IO Int
rand (Env m) = NonEvent $ m

randR :: Env -> Int -> TraceT Event IO Int
randR (Env m) mx = NonEvent $ liftM (`mod` mx) m

------------------------------------------------------------------


main = do
        env <- mkEnv
        a <- interp1 unsafeInterleaveIO $ do
                do { send 99  ; send 1 }
                wait 9
                i <- randR env 99
                do { send 100 ; send i }
                reset
        print a
        print a
