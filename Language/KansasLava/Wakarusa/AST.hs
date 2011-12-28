{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa.AST where

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Patch


import Data.Sized.Ix
import Data.Sized.Matrix (Matrix)
import Control.Monad.Fix
 
infixr 1 :=
infixr 1 :?
infixl 0 |||

-----------------------------------------------------------------------------------------

data STMT :: * -> * where
        -- syntax

        -- Assignment
        (:=)   :: (Rep a) => REG a -> EXPR a     -> STMT ()
        -- Predicate
        (:?)   :: EXPR Bool           -> STMT () -> STMT ()

        -- functionality

        CONNECT  :: SIGNAL a    -> EXPR a -> STMT ()

        -- primitive channel builder.
        CHANNEL  :: (Rep a, Rep b) => (Seq (Maybe a) -> Fabric (Seq b)) -> STMT Int

        -- COMBINATIONS
        OUTPUT   :: (Rep a) =>  (Seq (Maybe a) -> Fabric ())    -> STMT (REG a)
        INPUT    :: (Rep a) =>  Fabric (Seq a)                  -> STMT (EXPR a)
        SIGNAL   :: (Rep a) =>  (Seq (Maybe a) -> Seq a)        -> STMT (VAR a)
        PATCH    :: (Rep a, Rep b, Rep c, Rep d) 
                 => (Patch (Seq (Enabled a)) (Seq (Enabled b))
                           (Seq (Enabled c)) (Seq (Enabled d))) -> STMT ( REG a
                                                                        , EXPR (Enabled b)
                                                                        , EXPR (Enabled c)
                                                                        , REG d )

                -- way of puting an arbirary lava-function into the REG/EXPR world.
                -- TODO: call FABRIC
        GENERIC :: (Fabric ())                                  -> STMT ([Int],[Int])

        -- control flow
        GOTO   :: LABEL         -> STMT ()
        LABEL  :: STMT LABEL
        PAR    :: [STMT ()]     -> STMT ()
        SPARK  :: (LABEL -> STMT ())                            -> STMT ()      
                -- Do sub-thread. TODO: Returns the PC / state value??

        -- real time:
        -- wait for a cycle, or an event, or using a sample. 
        -- Much to figure out here.
        STEP   ::                                                  STMT ()
 
        -- Debugging
        PRINT :: (Rep a) => String -> EXPR a                    -> STMT ()

        -- macros
        IF     :: EXPR Bool -> STMT () -> STMT () -> STMT ()
        WHILE  :: EXPR Bool -> STMT () -> STMT ()

        -- Monad stuff
        RETURN :: a -> STMT a
        BIND   :: STMT a -> (a -> STMT b) -> STMT b
        MFIX   :: (a -> STMT a) -> STMT a

a ||| b = PAR [a,b]


instance Show (STMT a) where
        show (r := e) = show r ++ " := " ++ show e
        show (p :? s) = show p ++ " :? " ++ show s
        show (OUTPUT {})      = "OUTPUT"
        show (INPUT {})       = "INPUT"
        show (SIGNAL {})      = "SIGNAL"
        show (GOTO lab)       = "GOTO " ++ show lab
        show (LABEL)          = "LABEL"
        show (SPARK {})       = "SPARK"
        show (GENERIC {})     = "GENERIC"
        show (PAR es)         = "PAR" ++ show es
        show (CONNECT {})     = "IF"
        show (IF {})          = "IF"
        show (PRINT msg _)    = "PRINT<" ++ show msg ++ ">"
        show _ = "..."

instance Monad STMT where
        return = RETURN
        (>>=) = BIND
instance MonadFix STMT where
        mfix = MFIX

-----------------------------------------------------------------------------------------

data EXPR :: * -> * where
        OP0 :: (Rep a) 
            => (forall u . Signal u a)                                  -> EXPR a        -- also used as a lit
        OP1 :: (Rep a, Rep b)
            => (forall u . Signal u a -> Signal u b)                         
            -> EXPR a                                                   -> EXPR b
        OP2 :: (Rep a, Rep b,Rep c) 
            => (forall u . Signal u a -> Signal u b -> Signal u c) 
            -> EXPR a -> EXPR b                                         -> EXPR c
        OP3 :: (Rep a, Rep b,Rep c, Rep d) 
            => (forall u . Signal u a -> Signal u b -> Signal u c -> Signal u d)
            -> EXPR a -> EXPR b -> EXPR c                               -> EXPR d
        REG :: REG a                                                    -> EXPR a          -- only needed internally
        READ :: (Rep ix, Rep a, Size ix)                                                   -- is this used?
             => MEM ix a -> EXPR ix                                     -> EXPR a

instance Eq (EXPR a) where {}
instance Show (EXPR a) where
        show (OP0 _)     = "OP0"
        show (OP1 _ a)   = "OP1 (" ++ show a ++ ")"
        show (OP2 _ a b) = "OP1 (" ++ show a ++ ") (" ++ show b ++ ")"
        show (REG r)     = "REG " ++ show r
instance (Rep a, Num a) => Num (EXPR a) where
        (+) = OP2 (+)
        (-) = OP2 (-)
        (*) = OP2 (*)
        abs = OP1 abs
        signum = OP1 signum
        fromInteger n = OP0 (fromInteger n :: Signal u a)

-----------------------------------------------------------------------------------------

var :: (Rep a) => a -> Seq (Enabled a) -> Seq a
var = registerEnabled
undefinedVar :: (Rep a) => Seq (Enabled a) -> Seq a
undefinedVar = delayEnabled

{-
data STORE :: * -> * where
  UNDEFINED ::           STORE a      -- undefined default
  DEFAULT   :: a      -> STORE a      -- given default
  BUS       ::           STORE a      -- no store, just passthrough
  ACK       ::           STORE Bool   -- 
-}

-----------------------------------------------------------------------------------------

data REG a where
    R :: Int -> REG a

instance Show (REG a) where
        show (R n) = "R" ++ show n

data LABEL = L Int      deriving (Eq,Ord)

instance Show LABEL where
        show (L n) = "L" ++ show n

-- a signal works over time, and is not (directly) connected to any specific state machine
-- R a = SIGNAL (Maybe a), where the enable is the state machine
data SIGNAL a where
   S :: Int -> SIGNAL a

instance Show (SIGNAL a) where
        show (S n) = "S" ++ show n

-- a signal works over time, and is not (directly) connected to any specific state machine
-- EVENT a ~= EXPR (Maybe a)
data EVENT a where
   Ev :: Int -> EVENT a

-- An event is a continous incomming signal, that should be continously monitored.
instance Show (EVENT a) where
        show (Ev n) = "E" ++ show n

-----------------------------------------------------------------------------------------

class Variable var where  -- something that can be both read and written to
        toVAR :: (REG a,EXPR a) -> var a

instance Variable REG where
        toVAR (r,_) = r
instance Variable EXPR where
        toVAR (_,r) = r

data VAR a = VAR (forall (var :: * -> *) . (Variable var) => var a)


twiddle :: (REG a, EXPR a) -> VAR a
twiddle = undefined

data MEM ix a = MEM (forall (var :: * -> *) . (Variable var) => EXPR ix -> var a)

-----------------------------------------------------------------------------------------

nop :: STMT ()
nop = RETURN ()

