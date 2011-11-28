{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa.AST where

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Protocols.Enabled

import Data.Sized.Ix
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

        -- primitive channel builder.
        CHANNEL  :: (Rep a,Rep b) => (Seq (Maybe a) -> Seq b) -> STMT (REG a, EXPR b)

        OUTPUT   :: (Rep a) =>  (Seq (Maybe a) -> Fabric ()) -> STMT (REG a)
        INPUT    :: (Rep a) =>  Fabric (Seq a)               -> STMT (EXPR a)
        SIGNAL   :: (Rep a) =>  (Seq (Maybe a) -> Seq a)     -> STMT (VAR a)
        MEMORY   :: (Rep ix, Rep a, Size ix)                 => STMT (MEM ix a)
        
        -- control flow
        GOTO   :: LABEL         -> STMT ()
        LABEL  :: STMT LABEL
        PAR    :: [STMT ()]     -> STMT ()

        -- memory
--        WRITE  :: MEM ix a -> EXPR ix -> EXPR a             -> STMT ()

        -- real time:
        -- wait for a cycle, or an event, or using a sample. 
        -- Much to figure out here.
        STEP   ::                                                STMT ()

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
        show (OUTPUT {})    = "OUTPUT"
        show (INPUT {})     = "INPUT"
        show (SIGNAL {})  = "SIGNAL"
        show (GOTO lab)     = "GOTO " ++ show lab
        show (LABEL)        = "LABEL"
        show (PAR es)       = "PAR" ++ show es
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
        REG :: REG a                                                    -> EXPR a          -- only needed internally
        READ :: (Rep ix, Rep a, Size ix)
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
    M :: (Rep ix, Size ix) => Int -> EXPR ix -> REG a

instance Show (REG a) where
        show (R n) = "R" ++ show n
        show (M n _) = "M" ++ show n

data LABEL = L Int      deriving (Eq,Ord)

instance Show LABEL where
        show (L n) = "L" ++ show n

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

