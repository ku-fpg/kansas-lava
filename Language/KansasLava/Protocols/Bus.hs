{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, RecursiveDo, TypeFamilies,
             FlexibleInstances, FlexibleContexts, DataKinds #-}

module Language.KansasLava.Protocols.Bus where

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Spark

import Data.Boolean

import Data.Sized.Unsigned

import Control.Monad.Fix

import GHC.TypeLits

import Debug.Trace

{-
-- BusOutput
data Bus c a = Bus (Signal c (Enabled a)) (REG c ())

-- BusInput
data Writer c a = Writer (REG c a) (Signal c (Enabled ()))

data BUS c a = BUS (Bus c a) (Writer c a)

bus :: forall a m c . (LocalM m, c ~ LocalClock m, Rep a, SingI (W (Enabled a))) => m (BUS c a)
bus = do PORT wtr rdr     :: PORT c a <- port
         PORT wtr' rdr'   :: PORT c () <- port
         return (BUS (Bus rdr wtr') (Writer wtr rdr'))

-- Assumes signal input does not change when waiting for ack.--
putBus :: (Rep a, SingI (W (Enabled a))) => Writer c a -> Signal c a -> STMT c b -> STMT c b
putBus (Writer reg ack) sig k = do
        startLoc <- LABEL
        -- send data
        reg := sig
        -- wait for ack
        notB (isEnabled ack) :? GOTO startLoc
        k
takeBus :: (Rep a, SingI (W (Enabled a))) => Bus c a -> REG c a -> STMT c b -> STMT c b
takeBus (Bus inp ack) reg k = do
        startLoc <- LABEL
        (isEnabled inp) :? do
                reg := enabledVal inp
                ack := pureS ()
        (notB (isEnabled inp)) :? do
                GOTO startLoc
        k


fifo :: forall a c m . (Rep a, LocalM m, c ~ LocalClock m, SingI (W (Enabled a))) => Bus c a -> m (Bus c a)
fifo lhs_bus = do
    BUS rhs_bus rhs_bus_writer :: BUS c a <- bus
    VAR reg                    :: VAR c a <- uninitialized

    _pc <- spark $ do
            lab <- LABEL
            _ <- ($) takeBus lhs_bus reg   $ STEP
            putBus rhs_bus_writer reg      $ GOTO lab
            return ()

--    outStdLogicVector "fifo_pc" pc

    return $ rhs_bus


latchBus :: forall a m c
          . (LocalM m, c ~ LocalClock m, Rep a, SingI (W (Enabled a))) => Signal c (Enabled a) -> m (Bus c a)
latchBus inp = do
        BUS rhs_bus rhs_bus_writer :: BUS c a <- bus
        VAR reg                    :: VAR c a <- uninitialized
        _ <- ($) spark $ do
                         lab <- STEP
                         (isEnabled inp) :?              reg := enabledVal inp
                         (notB (isEnabled inp)) :?       GOTO lab
                         _ <- STEP
                         putBus rhs_bus_writer reg $ GOTO lab
        return rhs_bus
-}