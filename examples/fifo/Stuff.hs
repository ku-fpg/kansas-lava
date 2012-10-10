{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Monad.Trans.Class

-- Monad transformer
data M :: * -> (* -> *) -> * -> * where
  Event :: m (Trace e a) -> M e m a
  Bind :: M e m a -> (a -> M e m b) -> M e m b
  Return :: a  -> M e m a

instance Monad m => Monad (M e m) where
        return = Return
        (>>=) = Bind

-- This is a trace
data Trace :: * -> * -> * where
        EventTrace :: (Show a) => e -> a -> Trace e a
        BindTrace :: Trace e a -> Trace e b -> Trace e b
        ReturnTrace :: a                -> Trace e a

instance Show e => Show (Trace e a) where
        show (EventTrace event a) = show a ++ "<-" ++ show event
        show (BindTrace m n) = show m ++ ";" ++ show n
        show (ReturnTrace _) = show "nop"

result :: Trace e a -> a
result (EventTrace _ a) = a
result (BindTrace _ b) = result b
result (ReturnTrace a) = a

data Event :: * where
        SendEvent :: Int -> Bool -> Event

instance Show Event where
        show (SendEvent n b) = "send(" ++ show n ++ ")"

send :: Int -> M Event IO Bool
send n = Event $ do { print ("send",n) ; return $ EventTrace (SendEvent n True) True }

interp1 :: Interleave m => M e m a -> m (Trace e a)
interp1 (Event ev) = ev
interp1 (Return a) = return (ReturnTrace a)
interp1 (Bind m k) = do
        a <- interleave $ interp1 m
        b <- interleave $ interp1 (k (result a))
        return $ BindTrace a b

class Monad m => Interleave m where
        interleave :: m a -> m a

instance Interleave IO where
        interleave = unsafeInterleaveIO

main = do
        a <- interp1 $ do
                do { send 99  ; send 1 }
                do { send 100 ; send 2 }
        print a
        print a
