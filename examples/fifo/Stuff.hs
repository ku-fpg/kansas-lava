{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}

import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Monad.Trans.Class
import System.Random
import Control.Monad
import Control.Monad.Trans.Trace

------------------------------------------------------------------

data FifoE :: * where
        SendEvent :: Int -> Bool -> FifoE
        WaitEvent :: Int -> FifoE
        ResetEvent :: FifoE

instance Show FifoE where
        show (SendEvent n b) = "send(" ++ show n ++ ")"
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

data Env = Env
        { env_rand  :: forall r . (Random r) => IO r -- a random number generator
        , env_randR :: forall r . (Random r) => (r,r) -> IO r
        }

mkEnv :: IO Env
mkEnv = do
        std0 <- getStdGen
        var <- newMVar std0
        return $ Env
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
          }

send :: Int -> TraceT FifoE FifoM Bool
send n = event $ FifoM $ \ env -> do { print ("send",n) ; return (SendEvent n True,True) }

wait :: Int -> TraceT FifoE FifoM ()
wait n = event $ FifoM $ \ env -> do { print ("wait",n) ; return (WaitEvent n,()) }

reset :: TraceT FifoE FifoM ()
reset = event $ FifoM $ \ env -> do { return (ResetEvent,()) }

rand :: (Random r) => TraceT FifoE FifoM r
rand = lift $ FifoM $ \ env -> do { r <- env_rand env ; return r }

randR :: (Random r) => (r,r) -> TraceT FifoE FifoM r
randR (a,b) = lift $ FifoM $ \ env -> do { r <- env_randR env (a,b) ; return r }

------------------------------------------------------------------

main = do
        env <- mkEnv
        let interleave ~(FifoM f) = FifoM $ unsafeInterleaveIO . f
        let FifoM f = interp1 interleave $ do
                do { send 99  ; send 1 }
                wait 9
                i <- randR (10,100)
                do { send 100 ; send i }
                reset
        a <- f env
        print a
        print a
