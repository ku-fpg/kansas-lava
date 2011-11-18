{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}
import Language.KansasLava hiding (Reg, (:=), IF)
import Data.Sized.Unsigned

import Control.Monad.Fix
import Data.Set as Set
import Data.Map as Map

import Control.Monad.State

data REG a = R Int
data OUT a = O Int
data IN  a = I Int

{-
data BLOCK :: * -> * where
        BLOCK :: STMT () -> BLOCK LABEL

instance Monad BLOCK where {}
-}

data LABEL = L

infix 1 :=
infix 0 :?

data PROG :: * -> * where
        PROC :: STMT ()               -> PROG LABEL
        

data STMT :: * -> * where
        -- syntax
        IF     :: EXPR Bool -> STMT () -> STMT () -> STMT ()
        WHILE  :: EXPR Bool -> STMT () -> STMT ()
        (:=)   :: REG a -> EXPR a     -> STMT ()
        (:?)   :: EXPR Bool -> STMT () -> STMT ()

        -- functionality
        OUTPUT :: (Rep a) =>  (Seq (Maybe a) -> Fabric ()) -> STMT (REG a)
        INPUT  :: (Rep a) =>  Fabric a                     -> STMT (EXPR a)
        ALLOC  :: (Rep a) => Signal c a                    -> STMT (VAR' a)

        RETRY  :: EXPR Bool             -> STMT ()
        THREAD :: STMT ()               -> STMT LABEL -- needed to avoid Observable sharing
        GOTO   :: LABEL                 -> STMT ()

        START  :: EXPR a        -- wait for
               -> EXPR a        -- deadline
               -> LABEL
               -> STMT ()
        PAR    :: [STMT ()] -> STMT ()

        -- Monad stuff
        RETURN :: a -> STMT a
        BIND   :: STMT a -> (a -> STMT b) -> STMT b
        MFIX   :: (a -> STMT a) -> STMT a

        
--        ALWAYS :: 

class VAR var where  -- something that can be both read and written to
        toVAR :: REG a -> var a

data VAR' a = VAR (forall (var :: * -> *) . (VAR var) => var a)

instance VAR REG where
        toVAR = id
instance VAR EXPR where
        toVAR = REG

data EXPR :: * -> * where
        OP0 :: (forall u . Signal u a)                                                 -> EXPR a        -- also used as a lit
        OP1 :: (forall u . Signal u a -> Signal u b)                         -> EXPR a -> EXPR b
        OP2 :: (forall u . Signal u a -> Signal u b -> Signal u c) -> EXPR a -> EXPR b -> EXPR c
        REG :: REG a                                                                   -> EXPR a          -- only needed internally

o0 :: OUT U8
o0 = O 0

instance Monad STMT where
        return = RETURN
        (>>=) = BIND
instance MonadFix STMT where
        mfix = MFIX

instance Show (STMT a) where
--        show (ALLOC v) = "ALLOC " ++ show v

instance Show a => Show (OUT a) where

--showSTMT ::  (Show a) => STMT a -> String
--showSTMT (OUTPUT o e) = "OUTPUT" ++ showOUT o ++ showEXPR e

showOUT :: OUT a -> String
showOUT = undefined


showEXPR :: EXPR a -> String
showEXPR = undefined

instance Eq a => Eq (EXPR a) where {}
instance Show a => Show (EXPR a) where {}
instance (Rep a, Num a) => Num (EXPR a) where
        (+) = OP2 (+)
        (-) = OP2 (-)
        (*) = OP2 (*)
        fromInteger n = OP0 (fromInteger n :: Signal u a)

instance Show (REG a) where 
{-
example1 = do
        OUTPUT o0 (0 :: EXPR U8)
        OUTPUT o0 (OP1 (+1) (OP0 $ 0 :: EXPR U8))
--        OUTPUT o0 (OP2 (+) (0 :: EXPR U8) (0 :: EXPR U8))

example2 = do
        a <- ALLOC (0 :: Signal u U8)
        forever $ do
                o0 := REG a
                a := (REG a + 1)
-}
{-
forever m = do 
        rec lab <- BB $ do { m ; FORK (0 :: EXPR Int) 0 lab }
        FORK (0 :: EXPR Int) 0 lab

always m = do
        let fork = FORK (0 :: EXPR Int) 0
        rec lab <- BB $ PAR [m, fork lab]
        fork lab
-}


data StateOfWakarusa = StateOfWakarusa
        { program :: Map LABEL (STMT ())        -- static
        , ready   :: Set LABEL                 -- programs that *should* be run
        }



--interp :: StateOfWakarusa -> STMT () -> Fabric ()
--interp st (


{-        
-- example o0 <- output (outStdLogicVector "o0" . latch) 

output :: (Seq (Maybe a) -> Fabric ()) -> STMT (OUT a)
output = undefined

        -- example i1 <- input (inStdLogicVector "i1")
input :: Fabric a -> STMT (IN a)
input = undefined

register' :: (Seq (Maybe a) -> Seq a) -> STMT (REG a)
-}

prog1 :: STMT ()
prog1 = do
        o0 :: REG Int <- OUTPUT (outStdLogicVector "o0")
        p0 :: REG Bool <- OUTPUT (outStdLogicVector "o0")
        VAR v0        <- ALLOC (0 :: Seq Int)
        VAR p0        <- ALLOC (low :: Seq Bool)
        
        rec loop <- thread $ do
                v0 := v0 + 1
                o0 := v0
                GOTO loop
--        (p0) :? o0 := 1
--        forever loop
        return ()

nop :: STMT ()
nop = RETURN ()

thread = THREAD

wakarusa :: STMT LABEL -> Fabric ()
wakarusa _ = return ()

data WakarusaState = WakarusaState
        { ws_uniq :: Int
        }

type WakarusaComp a = State WakarusaState a

compWakarusa :: STMT a -> WakarusaComp a
compWakarusa (BIND (BIND m1 k1) k2) = compWakarusa (BIND m1 (\ a -> BIND (k1 a) k2))
compWakarusa (BIND (RETURN a) k2)   = compWakarusa (k2 a)
compWakarusa (MFIX _) = error "MFIX"
compWakarusa (BIND m1 k1) = do
        r1 <- compWakarusa m1 
        compWakarusa (k1 r1)
compWakarusa _ = error "_"