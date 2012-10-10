{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
module Control.Monad.Trans.Trace where

import Control.Monad.Trans.Class
import Control.Monad

-- Monad transformer

data TraceT :: * -> (* -> *) -> * -> * where
  Event    :: m (Trace e a)                       -> TraceT e m a
  NonEvent :: m a                                 -> TraceT e m a
  Bind     :: TraceT e m a -> (a -> TraceT e m b) -> TraceT e m b
  Return   :: a                                   -> TraceT e m a

instance Monad m => Monad (TraceT e m) where
        return = Return
        (>>=) = Bind

instance MonadTrans (TraceT e) where
        lift = NonEvent

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

interp1 :: (Monad m) => (forall b . m b -> m b) -> TraceT e m a -> m (Trace e a)
interp1 _  (Event ev) = ev
interp1 _  (NonEvent m) = liftM ReturnTrace m
interp1 _  (Return a) = return (ReturnTrace a)
interp1 il (Bind m k) = do
        a <- il $ interp1 il m
        b <- il $ interp1 il (k (result a))
        return $ BindTrace a b
