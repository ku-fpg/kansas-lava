{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, RecursiveDo, TypeFamilies,
             FlexibleInstances, FlexibleContexts, DataKinds #-}

module Language.KansasLava.Spark where

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled

import Data.Boolean

import Data.Sized.Unsigned

import Control.Monad.Fix

import GHC.TypeLits

import Debug.Trace

infixr 1 :=
infixr 1 :?

-------------------------------------------------------------------------------

newtype LABEL = L U8      deriving (Eq,Ord)

instance Show LABEL where
        show (L n) = "L" ++ show n

-- We always start at address zero.
start :: LABEL
start = L 0
-------------------------------------------------------------------------------


data REG c a where
    R :: (SingI (W (Enabled a))) => SignalVar c (Enabled a) -> REG c a

assign :: (c ~ LocalClock m, LocalM m, Rep a) => REG c a -> Signal c a -> m ()
assign (R v) e = writeSignalVar v (enabledS e)

-------------------------------------------------------------------------------

-- TODO: call assignable
class Variable var where  -- something that can be both read and written to
        toVAR :: (REG c a,Signal c a) -> var c a


instance Variable REG where
        toVAR (r,_) = r

instance Variable Signal where
        toVAR (_,r) = r


data VAR c a = VAR (forall (var :: * -> * -> *) . (Variable var) => var c a)

type instance (1 + 64) = 65
type instance (1 + 8)  = 9
type instance (1 + 0)  = 1

initially :: forall a m c . (LocalM m, c ~ LocalClock m, Rep a, SingI (W (Enabled a))) => a -> m (VAR c a)
--, SingI (W (Enabled a))) => a -> m (VAR c a)
initially a = do
        var <- newSignalVar
        let f x rest = mux (isEnabled x) (rest,enabledVal x)
        sig <- readSignalVar var $ \ xs ->
                let r = register a $ Prelude.foldr f r xs
                in r
        return (VAR (toVAR (R var,sig :: Signal c a)))

uninitialized :: forall a m c . (LocalM m, c ~ LocalClock m, Rep a, SingI (W (Enabled a))) => m (VAR c a)
uninitialized = do
        var <- newSignalVar
        let f a rest = mux (isEnabled a) (rest,enabledVal a)
        sig <- readSignalVar var $ \ xs ->
                let r = delay $ Prelude.foldr f r xs
                in r
        return (VAR (toVAR (R var,sig :: Signal c a)))


--------------------------------------------------------------------------
-- CHAN rdr wtr :: CHAN c Int <- channel

data CHAN c a = CHAN (Signal c (Enabled a)) (REG c a)

channel :: forall a c m . (LocalM m, c ~ LocalClock m, Rep a, SingI (W (Enabled a))) => m (CHAN c a)
channel = do
        var :: SignalVar c (Enabled a) <- newSignalVar
        let f a rest = mux (isEnabled a) (rest,a)
        sig <- readSignalVar var $ \ xs -> Prelude.foldr f disabledS xs
        return $ CHAN sig (R var)

--------------------------------------------------------------------------------------

data STMT :: * -> * -> * where
        -- Assignment
        (:=)   :: (Rep a) => REG c a -> Signal c a          -> STMT c ()

        -- Predicate
        (:?)   :: Signal c Bool  -> STMT c ()                   -> STMT c ()


        -- control flow
        STEP   :: STMT c LABEL    -- wait a cycle, give me a label
        LABEL  :: STMT c LABEL    -- give a intra-clock cycle label

        GOTO   :: LABEL                                         -> STMT c ()    --  GOTO the label *on the next cycle*

        NOP   ::                                                   STMT c ()

        -- Monad stuff
        RETURN :: a -> STMT c a
        BIND   :: STMT c a -> (a -> STMT c b) -> STMT c b
        MFIX   :: (a -> STMT c a) -> STMT c a

instance Monad (STMT c) where
        return = RETURN
        (>>=) = BIND
instance MonadFix (STMT c) where
        mfix = MFIX

data SMState m = SMState
        { pc_reg :: REG (LocalClock m) U8
        , pc_sig :: Signal (LocalClock m) U8
        , pc_val :: U8
        , st_fab :: (LocalM m) => m ()
        , st_pred :: Pred (LocalClock m)
        }

data SparkMonad m a = SparkMonad { runSparkMonad :: SMState m -> (a,SMState m) }

instance Monad (SparkMonad m) where
        return a = SparkMonad $ \ st -> (a,st)
        (SparkMonad m) >>= k = SparkMonad $ \ st -> case m st of
                                                       (a,st') -> runSparkMonad (k a) st'

instance MonadFix (SparkMonad m) where
        mfix k = SparkMonad $ \ st -> let (a,st') = runSparkMonad (k a) st
                                      in (a,st')

issueFab :: (Signal (LocalClock m) Bool -> m ()) -> SparkMonad m ()
issueFab fab = SparkMonad $ \ st ->
        ((),st { st_fab = st_fab st >> fab (compilePred (pc_sig st) (st_pred st)) })

regPC :: SparkMonad m (REG (LocalClock m) U8)
regPC = SparkMonad $ \ st -> (pc_reg st,st)

-- takes a function that takes the old predicate, and gives the new one
incPC :: (c ~ LocalClock m) => (U8 -> Pred c -> Pred c) -> SparkMonad m U8
incPC f = SparkMonad $ \ st -> let pc1 = pc_val st + 1
                             in (pc1,st { pc_val = pc1, st_pred = f pc1 (st_pred st) })

falsifyPred :: SparkMonad m ()
falsifyPred = SparkMonad $ \ st -> ((),st { st_pred = PredFalse })

withPred :: Signal (LocalClock m) Bool -> SparkMonad m () -> SparkMonad m ()
withPred p (SparkMonad m) = SparkMonad $ \ st ->
        case m st { st_pred = st_pred st `PredAnd` (PredCond p) } of
          (a,st') -> (a,st' { st_pred = (st_pred st `PredAnd` (PredNot (PredCond p))) `PredOr` st_pred st' })

data Pred c = PredCond (Signal c Bool)
            | PredOr (Pred c) (Pred c)
            | PredAnd (Pred c) (Pred c)
            | PredNot (Pred c)
            | PredFalse
            | PredPC U8

instance Show (Pred c) where
        show _ = "Show Pred"

compilePred :: Signal c U8 -> Pred c -> Signal c Bool
compilePred _pc (PredCond p)     = p
compilePred pc (PredOr p1 p2)    = compilePred pc p1 .||. compilePred pc p2
compilePred pc (PredAnd p1 p2)   = compilePred pc p1 .&&. compilePred pc p2
compilePred pc (PredNot p1)      = bitNot (compilePred pc p1)
compilePred _pc (PredFalse)      = low
compilePred pc (PredPC pc')      = pc .==. pureS pc'

spark :: forall m c . (LocalM m, c ~ LocalClock m) => STMT c () -> m (Signal c U8)
spark stmt = do
--        () <- trace (show ("spark")) $ return ()
        VAR pc :: VAR c U8 <- initially 0
        let st0 = SMState
                { pc_reg = pc
                , pc_sig = pc
                , pc_val = 0
                , st_fab = return ()
                , st_pred = PredPC 0
                }
        let stmt' = do stmt
                       lab <- STEP
                       GOTO lab
        let ((),st1) = runSparkMonad (compile stmt') st0
--        () <- trace (show ("pred", st_pred st1)) $ return ()
        -- do the actions
        st_fab st1
--        liftFabric (st_fab st1)

        -- If nothing else triggers the PC, then increment it by 1
--        assign pc (pc + 1)
--        () <- trace (show ("done")) $ return ()


        return pc

compile :: (LocalM m, c ~ LocalClock m) => STMT (LocalClock m) a -> SparkMonad m a
compile ((R v) := expr) = issueFab $ \ predArg ->
        writeSignalVar v (packEnabled predArg expr)
compile (p :? stmt) = withPred p (compile stmt)
compile STEP = do
    rec issueFab $ \ predArg -> writeSignalVar pc_reg (packEnabled predArg (pureS pc))
        pc <- incPC $ \ pcArg _ -> PredPC pcArg
        (R pc_reg) <- regPC
    return (L pc)
compile LABEL = do
        pc <- incPC $ \ pc old_pred -> (PredPC pc) `PredOr` old_pred
        return (L pc)
compile (GOTO ~(L n)) = do      -- the ~ allows for forward jumping
        (R pc) <- regPC
        issueFab $ \ predArg -> writeSignalVar pc (packEnabled predArg (pureS n))
        falsifyPred
compile NOP = return ()                 -- is this needed?

compile (RETURN a) = return a
compile (BIND m k) = do
        a <- compile m
        compile (k a)
compile (MFIX k) = mfix (compile . k)

--------------------------------------------------
