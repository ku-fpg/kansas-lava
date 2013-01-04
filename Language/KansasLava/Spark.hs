{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, RecursiveDo, DoRec, TypeFamilies,
             FlexibleInstances, FlexibleContexts, DataKinds #-}

module Language.KansasLava.Spark where

import Language.KansasLava.Signal
import Language.KansasLava.Probes
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Protocols.Types
import Language.KansasLava.Types
import Language.KansasLava.Universal

import Data.Boolean

import Data.Monoid

import Data.Sized.Sized
import Data.Sized.Unsigned
import Data.Sized.Matrix as M

import Control.Monad.Fix
import Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader

import GHC.TypeLits

import Debug.Trace
import System.IO.Unsafe

import qualified Data.Set as Set
import Data.Set (Set)

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

data REG a where
    R :: SignalVar CLK (Enabled a) -> REG a

assign :: (SparkM m, Rep a, SingI (W (Enabled a))) => REG a -> Seq a -> m ()
assign (R v) e = writeSignalVar v (enabledS e)

-------------------------------------------------------------------------------

-- TODO: call assignable
class Variable var where  -- something that can be both read and written to
        toVAR :: (REG a,Signal CLK a) -> var a

instance Variable REG where
        toVAR (r,_) = r
instance Variable (Signal CLK) where
        toVAR (_,r) = r

data VAR a = VAR (forall (var :: * -> *) . (Variable var) => var a)

initially :: forall a var m . (SparkM m, Rep a, SingI (W (Enabled a))) => a -> m (VAR a)
initially a = do
        var <- newSignalVar
        let f a rest = mux (isEnabled a) (rest,enabledVal a)
        sig <- readSignalVar var $ \ xs ->
                let r = register a $ Prelude.foldr f r xs
                in r
        return (VAR (toVAR (R var,sig :: Signal CLK a)))

uninitialized :: forall a var m . (SparkM m, Rep a, SingI (W (Enabled a))) => m (VAR a)
uninitialized = do
        var <- newSignalVar
        let f a rest = mux (isEnabled a) (rest,enabledVal a)
        sig <- readSignalVar var $ \ xs ->
                let r = delay $ Prelude.foldr f r xs
                in r
        return (VAR (toVAR (R var,sig :: Signal CLK a)))

-- TODO: call assignable
--class Variable var where  -- something that can be both read and written to
--        toVAR :: (REG a,Signal CLK a) -> var a

-- TODO: swap order here  - we should go from left to right
data CHAN a = CHAN (Signal CLK (Enabled a)) (REG a)

-- CHAN rdr wtr :: CHAN Int <- channel

channel :: forall a var m . (SparkM m, Rep a, SingI (W (Enabled a))) => m (CHAN a)
channel = do
        var :: SignalVar CLK (Enabled a) <- newSignalVar
        let f a rest = mux (isEnabled a) (rest,a)
        sig <- readSignalVar var $ \ xs -> Prelude.foldr f disabledS xs
        return $ CHAN sig (R var)

--- Acknowledged ??
data Bus a = Bus (Signal CLK (Enabled a)) (REG ())

data Writer a = Writer (REG a) (Signal CLK (Enabled ()))

data BUS a = BUS (Bus a) (Writer a)

bus :: forall a var m . (SparkM m, Rep a, SingI (W (Enabled a)), SingI (W (Enabled ()))) => m (BUS a)
bus = do CHAN rdr wtr :: CHAN a <- channel
         CHAN rdr' wtr' :: CHAN () <- channel
         return (BUS (Bus rdr wtr') (Writer wtr rdr'))

-- Assumes signal input does not change when waiting for ack.
--
putBus :: (Rep a, SingI (W (Enabled a))) => Writer a -> Signal CLK a -> STMT b -> STMT b
putBus (Writer reg ack) sig k = do
        start <- LABEL
        -- send data
        reg := sig
        -- wait for ack
        notB (isEnabled ack) :? GOTO start
        k

takeBus :: (Rep a, SingI (W (Enabled a)), SingI (W (Enabled ()))) => Bus a -> REG a -> STMT b -> STMT b
takeBus (Bus inp ack) reg k = do
        start <- LABEL
        (isEnabled inp) :? do
                reg := enabledVal inp
                ack := pureS ()
        (notB (isEnabled inp)) :? do
                GOTO start
        k

fifo :: forall a . (Rep a, SingI (W (Enabled a)), SingI (W (Enabled ())), SingI (W (Enabled U8)))=> Bus a -> Fabric (Bus a)
fifo lhs_bus = do
    BUS rhs_bus rhs_bus_writer :: BUS a <- bus
    VAR reg :: VAR a <- uninitialized

    pc <- spark $ do
            lab <- LABEL
            takeBus lhs_bus reg         $ STEP
            putBus rhs_bus_writer reg   $ GOTO lab
            return ()

    outStdLogicVector "fifo_pc" pc

    return rhs_bus


latchBus :: forall a m . (Rep a, SingI (W (Enabled a)), SingI (W (Enabled ())), SingI (W (Enabled U8)), SparkM m) => Signal CLK (Enabled a) -> m (Bus a)
latchBus inp = do
        BUS rhs_bus rhs_bus_writer :: BUS a <- bus
        VAR reg                    :: VAR a <- uninitialized
        spark $ do
                lab <- STEP
                (isEnabled inp) :?              reg := enabledVal inp
                (notB (isEnabled inp)) :?       GOTO lab
                STEP
                putBus rhs_bus_writer reg
                                              $ GOTO lab
        return rhs_bus

data MEM a d = MEM (Signal CLK d) (REG a) (REG (a,d))

-- Not compilent with protocol
memory :: forall a d .  (SingI a, Rep d,SingI (W (Enabled ((Sized a),d))), SingI (W (Enabled (Sized a)))) => Fabric (MEM (Sized a) d)
memory = do
        CHAN addr_out addr_in :: CHAN (Sized a)     <- channel
        CHAN wt_out   wt_in   :: CHAN (Sized a,d) <- channel

        let mem :: Signal CLK ((Sized a) -> d)
            mem = writeMemory wt_out

            addr_out' :: Signal CLK (Sized a)
            addr_out' = mux (isEnabled addr_out) (delay addr_out',enabledVal addr_out)

        return $ MEM (syncRead mem addr_out') addr_in wt_in

-- readMemory :: Signal CLK (a -> d) -> S

--------------------------------------------------------------------------------------

data STMT :: * -> * where
        -- Assignment
        (:=)   :: (Rep a, SingI (W (Enabled a))) => REG a -> Signal CLK a
                                                                -> STMT ()

        -- Predicate
        (:?)   :: Signal CLK Bool  -> STMT ()                   -> STMT ()

        -- control flow
        STEP   :: STMT LABEL    -- wait a cycle, give me a label
        LABEL  :: STMT LABEL    -- give a intra-clock cycle label

        GOTO   :: LABEL                                         -> STMT ()    --  GOTO the label *on the next cycle*

        NOP   ::                                                   STMT ()

        -- Monad stuff
        RETURN :: a -> STMT a
        BIND   :: STMT a -> (a -> STMT b) -> STMT b
        MFIX   :: (a -> STMT a) -> STMT a

instance Monad STMT where
        return = RETURN
        (>>=) = BIND
instance MonadFix STMT where
        mfix = MFIX

data SMState m = SMState
        { pc_reg :: REG U8
        , pc_sig :: Seq U8
        , pc_val :: U8
        , st_fab :: (SparkM m) => m ()
        , st_pred :: Pred
        }

{-
(&&&) :: STMT a -> STMT () -> STMT a
(&&&) (RETURN a) _ = error "no STEP found"
(&&&) (STEP) stmts = do { stmts ; STEP }
(&&&) (BIND (BIND m k1) k2) rest = m
-}

data SparkMonad m a = SparkMonad { runSparkMonad :: SMState m -> (a,SMState m) }

instance Monad (SparkMonad m) where
        return a = SparkMonad $ \ st -> (a,st)
        (SparkMonad m) >>= k = SparkMonad $ \ st -> case m st of
                                                       (a,st') -> runSparkMonad (k a) st'

instance MonadFix (SparkMonad m) where
        mfix k = SparkMonad $ \ st -> let (a,st') = runSparkMonad (k a) st
                                      in (a,st')

issueFab :: (Seq Bool -> m ()) -> SparkMonad m ()
issueFab fab = SparkMonad $ \ st ->
        ((),st { st_fab = st_fab st >> fab (compilePred (pc_sig st) (st_pred st)) })


regPC :: SparkMonad m (REG U8)
regPC = SparkMonad $ \ st -> (pc_reg st,st)

-- takes a function that takes the old predicate, and gives the new one
incPC :: (U8 -> Pred -> Pred) -> SparkMonad m U8
incPC f = SparkMonad $ \ st -> let pc1 = pc_val st + 1
                             in (pc1,st { pc_val = pc1, st_pred = f pc1 (st_pred st) })


falsifyPred :: SparkMonad m ()
falsifyPred = SparkMonad $ \ st -> ((),st { st_pred = PredFalse })


withPred :: Seq Bool -> SparkMonad m () -> SparkMonad m ()
withPred p (SparkMonad m) = SparkMonad $ \ st ->
        case m st { st_pred = st_pred st `PredAnd` (PredCond p) } of
          (a,st') -> (a,st' { st_pred = (st_pred st `PredAnd` (PredNot (PredCond p))) `PredOr` st_pred st' })

data Pred = PredCond (Seq Bool)
          | PredOr Pred Pred
          | PredAnd Pred Pred
          | PredNot Pred
          | PredFalse
          | PredPC U8

-- FIXME
instance Show Pred where
        show = undefined

compilePred :: Seq U8 -> Pred -> Seq Bool
compilePred pc (PredCond p)      = p
compilePred pc (PredOr p1 p2)    = compilePred pc p1 .||. compilePred pc p2
compilePred pc (PredAnd p1 p2)   = compilePred pc p1 .&&. compilePred pc p2
compilePred pc (PredNot p1)      = bitNot (compilePred pc p1)
compilePred pc (PredFalse)       = low
compilePred pc (PredPC pc')      = pc .==. pureS pc'

spark :: forall m . (SparkM m, SingI (W (Enabled U8))) => STMT () -> m (Seq U8)
spark stmt = do
--        () <- trace (show ("spark")) $ return ()
        VAR pc :: VAR U8 <- initially 0
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
        () <- trace (show ("pred", st_pred st1)) $ return ()
        -- do the actions
        st_fab st1
--        liftFabric (st_fab st1)

        -- If nothing else triggers the PC, then increment it by 1
--        assign pc (pc + 1)
        () <- trace (show ("done")) $ return ()


        return pc
  where
        compile :: STMT a -> SparkMonad m a
        compile ((R v) := expr) = issueFab $ \ pred ->
                writeSignalVar v (packEnabled pred expr)
        compile (p :? stmt) = withPred p (compile stmt)
        compile STEP = do
            rec issueFab $ \ pred -> writeSignalVar pc_reg (packEnabled pred (pureS pc))
                pc <- incPC $ \ pc _ -> PredPC pc
                (R pc_reg) <- regPC
            return (L pc)
        compile LABEL = do
                pc <- incPC $ \ pc old_pred -> (PredPC pc) `PredOr` old_pred
                return (L pc)
        compile (GOTO ~(L n)) = do      -- the ~ allows for forward jumping
                (R pc) <- regPC
                issueFab $ \ pred -> writeSignalVar pc (packEnabled pred (pureS n))
                falsifyPred
        compile NOP = return ()                 -- is this needed?

        compile (RETURN a) = return a
        compile (BIND m k) = do
                a <- compile m
                compile (k a)
        compile (MFIX k) = mfix (compile . k)

--------------------------------------------------

instance Boolean (STMT (Seq Bool)) where
        true = return true
        false = return false
        notB = liftM notB
        (&&*) = liftM2 (&&*)
        (||*) = liftM2 (||*)

type instance BooleanOf (STMT s) = STMT (Seq Bool)

instance IfB (STMT ()) where
        ifB i t f = do
            rec b <- i
                b :? GOTO t_lab
                _ <- STEP
                f
                GOTO end_lab
                t_lab <- STEP
                t
                end_lab <- STEP
                return ()
            return ()


