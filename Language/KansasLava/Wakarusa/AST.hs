{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa.AST where

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Rep

import Control.Monad.Fix
 
infixr 0 :=
infixr 0 :?

-----------------------------------------------------------------------------------------

data STMT :: * -> * where
        -- syntax
        (:=)   :: (Rep a) => REG a -> EXPR a     -> STMT ()
        (:?)   :: EXPR Bool -> STMT () -> STMT ()

        -- functionality
        OUTPUT   :: (Rep a) =>  (Seq (Maybe a) -> Fabric ()) -> STMT (REG a)
        INPUT    :: (Rep a) =>  Fabric (Seq a)               -> STMT (EXPR a)
        REGISTER :: (Rep a) =>  a                            -> STMT (VAR a)

        -- control flow
        GOTO   :: LABEL         -> STMT ()
        LABEL  :: STMT LABEL
        PAR    :: [STMT ()]     -> STMT ()

        -- macros
        IF     :: EXPR Bool -> STMT () -> STMT () -> STMT ()
        WHILE  :: EXPR Bool -> STMT () -> STMT ()

        -- Monad stuff
        RETURN :: a -> STMT a
        BIND   :: STMT a -> (a -> STMT b) -> STMT b
        MFIX   :: (a -> STMT a) -> STMT a

instance Show (STMT a) where
        show (r := e) = show r ++ " := " ++ show e
        show (p :? s) = show p ++ " :? " ++ show s
        show (OUTPUT {})    = "OUTPUT"
        show (INPUT {})     = "INPUT"
        show (REGISTER {})  = "REGISTER"
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
        OP0 :: (forall u . Signal u a)                                                 -> EXPR a        -- also used as a lit
        OP1 :: (Rep a) => (forall u . Signal u a -> Signal u b)                         -> EXPR a -> EXPR b
        OP2 :: (Rep a, Rep b) => (forall u . Signal u a -> Signal u b -> Signal u c) -> EXPR a -> EXPR b -> EXPR c
        REG :: REG a                                                                   -> EXPR a          -- only needed internally


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

data REG a = R Int      deriving (Eq,Ord)

instance Show (REG a) where
        show (R n) = "R" ++ show n

data LABEL = L Int      deriving (Eq,Ord)

instance Show LABEL where
        show (L n) = "L" ++ show n

-----------------------------------------------------------------------------------------

class Variable var where  -- something that can be both read and written to
        toVAR :: REG a -> var a

data VAR a = VAR (forall (var :: * -> *) . (Variable var) => var a)

instance Variable REG where
        toVAR = id
instance Variable EXPR where
        toVAR = REG

-----------------------------------------------------------------------------------------

nop :: STMT ()
nop = RETURN ()

