{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}
import Language.KansasLava hiding (Reg, (:=), IF)

import Language.KansasLava.Universal
import Data.Sized.Unsigned

import Control.Monad.Fix
import Data.Set as Set
import Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import System.IO.Unsafe

data REG a = R Int      deriving (Eq,Ord,Show)
--data OUT a = O Int      deriving Show
--data IN  a = I Int      deriving Show
data LABEL = L Int      deriving (Eq,Ord,Show)

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
        ALLOC  :: (Rep a) => (forall c . Signal c a)       -> STMT (VAR' a)

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

--o0 :: OUT U8
--o0 = O 0

instance Monad STMT where
        return = RETURN
        (>>=) = BIND
instance MonadFix STMT where
        mfix = MFIX

instance Show (STMT a) where
--        show (ALLOC v) = "ALLOC " ++ show v

-- instance Show a => Show (OUT a) where

--showSTMT ::  (Show a) => STMT a -> String
--showSTMT (OUTPUT o e) = "OUTPUT" ++ showOUT o ++ showEXPR e

--showOUT :: OUT a -> String
--showOUT = undefined


showEXPR :: EXPR a -> String
showEXPR = undefined

instance Eq a => Eq (EXPR a) where {}
instance Show a => Show (EXPR a) where {}
instance (Rep a, Num a) => Num (EXPR a) where
        (+) = OP2 (+)
        (-) = OP2 (-)
        (*) = OP2 (*)
        fromInteger n = OP0 (fromInteger n :: Signal u a)

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

prog1 :: STMT [LABEL]
prog1 = do
        o0 :: REG Int <- OUTPUT (outStdLogicVector "o0")
--        p0 :: REG Bool <- OUTPUT (outStdLogicVector "o0")
        VAR v0        <- ALLOC (0 :: Signal c Int)
--        VAR p0        <- ALLOC (low :: Signal c Bool)
        
        rec loop <- thread $ do
                v0 := v0 + 1
                o0 := v0
--                abc := OP2 (lava_fun) v0 p0
                GOTO loop
--        (p0) :? o0 := 1
--        forever loop
        return [loop]

nop :: STMT ()
nop = RETURN ()

thread = THREAD

wakarusa :: STMT LABEL -> Fabric ()
wakarusa _ = return ()

------------------------------------------------------------------------------

type Uniq = Int

data WakarusaState = WakarusaState
        { ws_uniq   :: Uniq
        , ws_fabric :: Fabric ()
        , ws_regs   :: Map Uniq [Pad] -- assignments
        , ws_pc     :: Maybe Uniq     -- ^ The PC 
        , ws_pcs    :: Map (LABEL,Uniq) (LABEL,Uniq)
                        -- ^ These are the defaults.
        }
--        deriving Show


instance Show WakarusaState where
        show (WakarusaState u f regs pc pcs) = 
                show u ++ "\n" ++
                show (unsafePerformIO (reifyFabric f)) ++ "\n" ++
                show regs ++ "\n" ++
                show pc ++ "\n" ++
                show pcs
                

data WakarusaEnv = WakarusaEnv
        { we_regs    :: Map Uniq Pad    -- assignments (2nd pass)
        , we_label   :: Maybe LABEL     --
        }
        deriving Show

------------------------------------------------------------------------------

type WakarusaComp = StateT WakarusaState (ReaderT WakarusaEnv Identity)

------------------------------------------------------------------------------

getUniq :: WakarusaComp Int
getUniq = do
        st <- get
        let u = ws_uniq st + 1
        put $ st { ws_uniq = u }
        return u

getRegWrite :: (Rep a) => Uniq -> WakarusaComp (Seq (Enabled a))
getRegWrite _ = do
        return $ undefinedS 


addOutput :: Fabric () -> WakarusaComp ()
addOutput f = do
        st <- get
        let fab = ws_fabric st >> f
        put $ st { ws_fabric = fab }
        return ()

--addLink :: (LABEL,Uniq) -> (LABEL,Uniq) -> 

addLabelContext :: Uniq -> WakarusaComp () -> WakarusaComp ()
addLabelContext uq m = local f m
  where
          f env = env { we_label = Just (L uq) }
                  
------------------------------------------------------------------------------

compWakarusa :: STMT a -> WakarusaComp a
compWakarusa (RETURN a) = return a
compWakarusa (BIND m1 k1) = do
        r1 <- compWakarusa m1
        compWakarusa (k1 r1)
compWakarusa (MFIX fn) = mfix (compWakarusa . fn)
compWakarusa (ALLOC  {}) = do
        uq <- getUniq
        -- TODO: compile build the alloc
        return (VAR $ toVAR $ R uq)

compWakarusa (OUTPUT connect) = do
        uq  <- getUniq   -- the uniq name of this output
        wt <- getRegWrite uq
        addOutput (connect wt)
        return $ R uq
compWakarusa (INPUT  {}) = error "INPUT"
compWakarusa (THREAD prog) = do
        -- get the number of the thread
        uq <- getUniq
        addLabelContext uq $ compWakarusa prog
        return $ L uq
compWakarusa (R n := e) = do
        uq <- getUniq        
        error (":= " ++ show uq)
compWakarusa (GOTO {}) = do
        error "compWakarusa GOTO"
compWakarusa _ = error "compWakarusa _"

{-
        IF     :: EXPR Bool -> STMT () -> STMT () -> STMT ()
        WHILE  :: EXPR Bool -> STMT () -> STMT ()
        (:=)   :: REG a -> EXPR a     -> STMT ()
        (:?)   :: EXPR Bool -> STMT () -> STMT ()

        RETRY  :: EXPR Bool             -> STMT ()
        THREAD :: STMT ()               -> STMT LABEL -- needed to avoid Observable sharing
        GOTO   :: LABEL                 -> STMT ()

        START  :: EXPR a        -- wait for
               -> EXPR a        -- deadline
               -> LABEL
               -> STMT ()
        PAR    :: [STMT ()] -> STMT ()

-}

test :: ([LABEL], WakarusaState)
test = runIdentity res3
  where
        res0 = runStateT (compWakarusa prog1) 
        res1 = res0 $ WakarusaState 
                    { ws_uniq = 0 
                    , ws_fabric = return ()
                    , ws_regs = Map.empty
                    , ws_pc = Nothing
                    , ws_pcs = Map.empty
                    }
        res2 = runReaderT res1
        res3 = res2 $ WakarusaEnv 
                    { we_regs = Map.empty
                    , we_label = Nothing
                    } 
