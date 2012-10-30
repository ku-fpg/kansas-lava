{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, RecursiveDo, DoRec, TypeFamilies,
             FlexibleInstances, FlexibleContexts #-}

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

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix as M

import Control.Monad.Fix
import Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader

import Debug.Trace
import System.IO.Unsafe

import qualified Data.Set as Set
import Data.Set (Set)

infixr 1 :=
infixr 1 :?


data REG a where
    R :: SignalVar CLK (Enabled a) -> REG a

data LABEL = L U8      deriving (Eq,Ord)

instance Show LABEL where
        show (L n) = "L" ++ show n

-- TODO: call assignable
class Variable var where  -- something that can be both read and written to
        toVAR :: (REG a,Signal CLK a) -> var a

instance Variable REG where
        toVAR (r,_) = r
instance Variable (Signal CLK) where
        toVAR (_,r) = r

data VAR a = VAR (forall (var :: * -> *) . (Variable var) => var a)

initially :: forall a var m . (MonadFix m, Rep a, Size (W (Enabled a))) => a -> SuperFabric m (VAR a)
initially a = do
        var <- newSignalVar
        let f a rest = mux (isEnabled a) (rest,enabledVal a)
        sig <- readSignalVar var $ \ xs ->
                let r = register a $ Prelude.foldr f r xs
                in r
        return (VAR (toVAR (R var,sig :: Signal CLK a)))

uninitialized :: forall a var m . (MonadFix m, Rep a, Size (W (Enabled a))) => SuperFabric m (VAR a)
uninitialized = do
        var <- newSignalVar
        let f a rest = mux (isEnabled a) (rest,enabledVal a)
        sig <- readSignalVar var $ \ xs ->
                let r = delay $ Prelude.foldr f r xs
                in r
        return (VAR (toVAR (R var,sig :: Signal CLK a)))


assign :: (MonadFix m, Rep a, Size (W (Enabled a))) => REG a -> Seq a -> SuperFabric m ()
assign (R v) e = writeSignalVar v (enabledS e)

data STMT :: * -> * where
        -- Assignment
        (:=)   :: (Rep a, Size (W (Enabled a))) => REG a -> Signal CLK a              -> STMT ()

        -- Predicate
        (:?)   :: Signal CLK Bool  -> STMT ()                   -> STMT ()

        -- control flow
        WAIT   :: STMT LABEL
        GOTO   :: LABEL                                         -> STMT ()

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

data SMState = SMState
        { pc_reg :: REG U8
        , pc_sig :: Seq U8
        , pc_val :: U8
        , st_fab :: Fabric ()
        , st_pred :: Pred
        }

data SparkMonad a = SparkMonad { runSparkMonad :: SMState -> (a,SMState) }

instance Monad SparkMonad where
        return a = SparkMonad $ \ st -> (a,st)
        (SparkMonad m) >>= k = SparkMonad $ \ st -> case m st of
                                                       (a,st') -> runSparkMonad (k a) st'

instance MonadFix SparkMonad where
        mfix k = SparkMonad $ \ st -> let (a,st') = runSparkMonad (k a) st
                                      in (a,st')

issueFab :: (Seq Bool -> Fabric ()) -> SparkMonad ()
issueFab fab = SparkMonad $ \ st ->
        ((),st { st_fab = st_fab st >> fab (compilePred (pc_sig st) (st_pred st)) })


regPC :: SparkMonad (REG U8)
regPC = SparkMonad $ \ st -> (pc_reg st,st)

incPC :: SparkMonad U8
incPC = SparkMonad $ \ st -> let pc1 = pc_val st + 1
                             in (pc1,st { pc_val = pc1, st_pred = PredPC pc1 })


withPred :: Seq Bool -> SparkMonad () -> SparkMonad ()
withPred p (SparkMonad m) = SparkMonad $ \ st ->
        case m st { st_pred = st_pred st `PredAnd` (PredCond p) } of
          (a,st') -> (a,st' { st_pred = (st_pred st `PredAnd` (PredNot (PredCond p))) `PredOr` st_pred st' })

data Pred = PredCond (Seq Bool)
          | PredOr Pred Pred
          | PredAnd Pred Pred
          | PredNot Pred
          | PredFalse
          | PredPC U8
  deriving Show

compilePred :: Seq U8 -> Pred -> Seq Bool
compilePred pc (PredCond p)      = p
compilePred pc (PredOr p1 p2)    = compilePred pc p1 .||. compilePred pc p2
compilePred pc (PredAnd p1 p2)   = compilePred pc p1 .&&. compilePred pc p2
compilePred pc (PredNot p1)      = bitNot (compilePred pc p1)
compilePred pc (PredFalse)       = low
compilePred pc (PredPC pc')      = pc .==. pureS pc'

spark :: forall m . (MonadFix m) => STMT () -> SuperFabric m (Seq U8)
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
                       lab <- WAIT
                       GOTO lab
        let ((),st1) = runSparkMonad (compile stmt') st0
        () <- trace (show ("pred", st_pred st1)) $ return ()
        -- do the actions
        liftFabric (st_fab st1)

        -- If nothing else triggers the PC, then increment it by 1
        assign pc (pc + 1)
        () <- trace (show ("done")) $ return ()


        return pc
  where
        compile :: STMT a -> SparkMonad a
        compile ((R v) := expr) = issueFab $ \ pred ->
                writeSignalVar v (packEnabled pred expr)
        compile (p :? stmt) = withPred p (compile stmt)
        compile WAIT = do
                pc <- incPC
                return (L pc)
        compile (GOTO ~(L n)) = do      -- the ~ allows for forward jumping
                (R pc) <- regPC
                issueFab $ \ pred -> writeSignalVar pc (packEnabled pred (pureS n))
        compile NOP = return ()                 -- is this needed?

        compile (RETURN a) = return a
        compile (BIND m k) = do
                a <- compile m
                compile (k a)
        compile (MFIX k) = mfix (compile . k)

--------------------------------------------------

instance Boolean (STMT (Seq Bool)) where

type instance BooleanOf (STMT s) = STMT (Seq Bool)

instance IfB (STMT ()) where
        ifB i t f = do
            rec b <- i
                b :? GOTO t_lab
                _ <- WAIT
                f
                GOTO end_lab
                t_lab <- WAIT
                t
                end_lab <- WAIT
                return ()
            return ()


