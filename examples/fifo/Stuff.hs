{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Concurrent.MVar
import System.IO.Unsafe

data M :: * -> * where
  Send  :: Int -> M Bool
  Pause :: Int -> M ()
  Rand  ::        M Int
  Bind :: M a -> (a -> M b) -> M b
  Return :: a  -> M a

instance Monad M where
        return = Return
        (>>=) = Bind


-- This is a trace
data Trace :: * -> * where
        EventTrace :: Event a -> Trace a
        BindTrace :: Trace a -> Trace b -> Trace b
        ReturnTrace :: a -> Trace a

instance Show (Trace a) where
        show (EventTrace event) = show event
        show (BindTrace m n) = show m ++ ";" ++ show n
        show (ReturnTrace _) = show "nop"

result :: Trace a -> a
result (EventTrace e) = eventResult e
result (BindTrace _ b) = result b
result (ReturnTrace a) = a

data Event :: * -> * where
        SendEvent :: Int -> Bool -> Event Bool

instance Show (Event a) where
        show (SendEvent n b) = show b ++ " <- send(" ++ show n ++ ")"

eventResult :: Event a -> a
eventResult (SendEvent _ a) = a

interp1 :: M a -> IO (Trace a)
interp1 (Send n) = do
        print ("send",n)
        return (EventTrace (SendEvent n True))
--interp1 (Rand) = do
--        rand
interp1 (Return a) = return (ReturnTrace a)
interp1 (Bind m k) = do
        a <- unsafeInterleaveIO $ interp1 m
        b <- unsafeInterleaveIO $ interp1 (k (result a))
        return $ BindTrace a b

main = do
        a <- interp1 $ do
                do { Send 99  ; Send 1 }
                do { Send 100 ; Send 2 }
        print a
        print a
