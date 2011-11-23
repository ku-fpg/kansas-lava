{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa.AST where

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Rep

import Control.Monad.Fix
 
infix 1 :=
infix 0 :?

-----------------------------------------------------------------------------------------

data STMT :: * -> * where
        -- syntax
        IF     :: EXPR Bool -> STMT () -> STMT () -> STMT ()
        WHILE  :: EXPR Bool -> STMT () -> STMT ()
        (:=)   :: (Rep a) => REG a -> EXPR a     -> STMT ()
        (:?)   :: EXPR Bool -> STMT () -> STMT ()

        -- functionality
        OUTPUT :: (Rep a) =>  (Seq (Maybe a) -> Fabric ()) -> STMT (REG a)
        INPUT  :: (Rep a) =>  Fabric (Seq a)               -> STMT (EXPR a)
        ALLOC  :: (Rep a) =>  a                            -> STMT (VAR' a)

        -- control flow
        GOTO   :: LABEL         -> STMT ()
        THREAD :: STMT ()       -> STMT LABEL -- needed to avoid Observable sharing
        PAR    :: [STMT ()]     -> STMT ()

        -- Monad stuff
        RETURN :: a -> STMT a
        BIND   :: STMT a -> (a -> STMT b) -> STMT b
        MFIX   :: (a -> STMT a) -> STMT a

-----------------------------------------------------------------------------------------

data EXPR :: * -> * where
        OP0 :: (forall u . Signal u a)                                                 -> EXPR a        -- also used as a lit
        OP1 :: (Rep a) => (forall u . Signal u a -> Signal u b)                         -> EXPR a -> EXPR b
        OP2 :: (Rep a, Rep b) => (forall u . Signal u a -> Signal u b -> Signal u c) -> EXPR a -> EXPR b -> EXPR c
        REG :: REG a                                                                   -> EXPR a          -- only needed internally


instance Monad STMT where
        return = RETURN
        (>>=) = BIND
instance MonadFix STMT where
        mfix = MFIX

instance Show (STMT a) where
        -- should really complete this

instance Eq a => Eq (EXPR a) where {}
instance Show a => Show (EXPR a) where {}
instance (Rep a, Num a) => Num (EXPR a) where
        (+) = OP2 (+)
        (-) = OP2 (-)
        (*) = OP2 (*)
        abs = OP1 abs
        signum = OP1 signum
        fromInteger n = OP0 (fromInteger n :: Signal u a)
-----------------------------------------------------------------------------------------

data REG a = R Int      deriving (Eq,Ord,Show)
data LABEL = L Int      deriving (Eq,Ord,Show)

-----------------------------------------------------------------------------------------

class VAR var where  -- something that can be both read and written to
        toVAR :: REG a -> var a

data VAR' a = VAR (forall (var :: * -> *) . (VAR var) => var a)

instance VAR REG where
        toVAR = id
instance VAR EXPR where
        toVAR = REG

-----------------------------------------------------------------------------------------

nop :: STMT ()
nop = RETURN ()

thread :: STMT () -> STMT LABEL
thread = THREAD
