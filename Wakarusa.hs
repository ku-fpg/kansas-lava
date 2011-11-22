{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}
import Language.KansasLava hiding (Reg, (:=), IF)

import Language.KansasLava.Universal
import Language.KansasLava.Fabric
import Data.Sized.Unsigned
import Data.Sized.Ix

import Control.Monad.Fix
import Data.Set as Set
import Data.Map as Map
import Data.Maybe

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
        (:=)   :: (Rep a) => REG a -> EXPR a     -> STMT ()
        (:?)   :: EXPR Bool -> STMT () -> STMT ()

        -- functionality
        OUTPUT :: (Rep a) =>  (Seq (Maybe a) -> Fabric ()) -> STMT (REG a)
        INPUT  :: (Rep a) =>  Fabric a                     -> STMT (EXPR a)
        ALLOC  :: (Rep a) =>  a                            -> STMT (VAR' a)

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
        OP2 :: (Rep a, Rep b) => (forall u . Signal u a -> Signal u b -> Signal u c) -> EXPR a -> EXPR b -> EXPR c
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
        VAR v0        <- ALLOC (99 :: Int)

        rec loop <- thread $ do
                v0 := v0 + 1
--                o0 := v0
                o0 := v0
--                o0 := 234
--                o0 := 3
--                o0 := 4
--                o0 := 5
--                o0 := 6
--                o0 := 7
--                o0 := 3
--                PAUSE (....)
                
--                abc := OP2 (lava_fun) v0 p0
                GOTO loop
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
        , ws_regs        :: Map Uniq (Pad -> Pad)       -- add the register, please
        , ws_assignments :: Map Uniq Pad
          -- ^(untyped) assignments, chained into a single Seq (Enabled a).
        , ws_pc     :: PC               -- ^ The PC 
        , ws_fallthrough :: Bool        
                -- ^ Does the code fall through to the next statement?
                -- After GOTO this is False.
                -- Catches threads with non-terminated code fragments
        , ws_pcs    :: Map LABEL (Seq (Enabled (Enabled PC)))     
                        -- assign your way into a new thread
                        -- The outermost enabled is if there is a write command
                        -- The inner one allow the writing of a STOP command
        }
--        deriving Show


instance Show WakarusaState where
        show (WakarusaState u f regs assignments pc _ pcs) = 
                "uniq : " ++ show u ++ "\n" ++
                "reify : " ++ show (unsafePerformIO (reifyFabric f)) ++ "\n" ++
                "regs : " ++ show (fmap (const ()) regs) ++ "\n" ++
                "assignments : " ++ show (fmap (const ()) assignments) ++ "\n" ++
                "pc : " ++ show pc ++ "\n" ++
                "pcs : " ++ show (fmap (const ()) pcs)
                

type PC = X256

data WakarusaEnv = WakarusaEnv
        { we_label    :: Maybe LABEL            --

        -- These are 2nd pass things
        , we_reads    :: Map Uniq Pad           -- register output  (2nd pass)
        , we_writes   :: Map Uniq Pad           -- register input   (2nd pass)
        , we_labels  :: Map LABEL (Seq Bool)
        , we_pcs     :: Map LABEL (Seq (Enabled PC))
                -- ^ is this basic block being executed right now?
                -- ^ and each block has a PC (where maxBound == no execution happending)
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
        
getPC :: WakarusaComp PC
getPC = do
        st <- get
        return $ ws_pc st 
        
incPC :: WakarusaComp ()
incPC = do
        st <- get
        let pc = ws_pc st + 1
        put $ st { ws_pc = pc }
        return ()
        
recordJump :: LABEL -> WakarusaComp ()
recordJump lab =  do
        p <- getPred
        st <- get
        let m = insertWith chooseEnabled
                           lab
                           (commentS "recordJump" $ packEnabled p (enabledS 0))
                           (ws_pcs st)
        put (st { ws_pcs = m })
        return ()

registerAction :: forall a . (Rep a) => REG a -> Seq Bool -> Seq a -> WakarusaComp ()
registerAction (R r) en val = do
        st <- get
        let mix :: Maybe (Seq (Enabled a)) -> Maybe (Seq (Enabled a)) -> Seq (Enabled a)
            mix (Just s1) (Just s2) = chooseEnabled s1 s2
            mix _ _ = error "failure to mix register assignments"
        let assignments = Map.insertWith (\ u1 u2 -> toUni (mix (fromUni u1) (fromUni u2)))
                                  r 
                                  (toUni $ packEnabled en val)
                                  (ws_assignments st)
        put $ st { ws_assignments = assignments }
        return ()

addRegister :: forall a. (Rep a) => REG a -> a -> WakarusaComp ()
addRegister (R k) def = do
        st <- get
        let m = Map.insert k (toUni . f . fromUni) (ws_regs st)
        put $ st { ws_regs = m }
        return ()
  where
          f :: (Rep a) => Maybe (Seq (Enabled a)) -> Seq a
          f Nothing   = pureS def
          f (Just wt) = registerEnabled def wt

-- get the predicate for *this* instruction
getPred :: WakarusaComp (Seq Bool)
getPred = do
        lab <- getLabel
        case lab of
          Nothing -> error "actions are possible outside basic blocks"
          Just l  -> do
            pc <- getPC
            env <- ask
            return $ case Map.lookup l (we_pcs env) of
              Nothing     -> error $ "can not find the PC for " ++ show lab
              Just pc_sig -> isEnabled pc_sig .&&. (enabledVal pc_sig .==. pureS pc)

getLabel :: WakarusaComp (Maybe LABEL)
getLabel = do
        env <- ask
        return $ we_label env

getRegRead :: (Rep a) => Uniq -> WakarusaComp (Seq a)
getRegRead k = do
        env <- ask
        -- remember to return before checking error, to allow 2nd pass
        return $ case Map.lookup k (we_reads env) of
           Nothing -> error $ "getRegRead, can not find : " ++ show k
           Just p  -> case fromUni p of
                        Nothing -> error $ "getRegRead, coerce error in : " ++ show k
                        Just e -> e

getRegWrite :: forall a . (Rep a) => Uniq -> WakarusaComp (Seq (Enabled a))
getRegWrite k = do
        env <- ask
        -- remember to return before checking error, to allow 2nd pass
        return $ case Map.lookup k (we_writes env) of
           Nothing -> error $ "getRegWrite, can not find : " ++ show k
           Just p  -> case fromUni p of
                        Nothing -> error $ "getRegWrite, coerce error in : " ++ show k
                        Just e -> e

compThread :: LABEL -> WakarusaComp () -> WakarusaComp ()
compThread lab m = do
        st0 <- get
        put (st0 { ws_pc = 0, ws_fallthrough = True })
        local f m
        st1 <- get
        if ws_fallthrough st1 then error $ " label " ++ show lab ++ " falls through"
                              else put (st1 { ws_pc = ws_pc st0
--                                            , ws_pcs = Map.insertWith choose lab disabledS (ws_pcs st1)
                                            })
        return ()
  where
          f env = env { we_label = Just lab }
          
addOutput :: Fabric () -> WakarusaComp ()
addOutput f = do
        st <- get
        let fab = ws_fabric st >> f
        put $ st { ws_fabric = fab }
        return ()

noFallThough :: WakarusaComp ()
noFallThough = modify (\ st -> st { ws_fallthrough = False })

------------------------------------------------------------------------------

compWakarusa :: STMT a -> WakarusaComp a
compWakarusa (RETURN a) = return a
compWakarusa (BIND m1 k1) = do
        r1 <- compWakarusa m1
        compWakarusa (k1 r1)
compWakarusa (MFIX fn) = mfix (compWakarusa . fn)
compWakarusa (ALLOC def) = do
        uq <- getUniq
        let reg = R uq
        -- add the register to the table
        addRegister reg def
        return (VAR $ toVAR $ reg)

compWakarusa (OUTPUT connect) = do
        uq  <- getUniq   -- the uniq name of this output
        wt <- getRegWrite uq
        addOutput (connect wt)
        return $ R uq
compWakarusa (INPUT  {}) = error "INPUT"
compWakarusa (THREAD prog) = do
        -- get the number of the thread
        uq <- getUniq
        let lab = L uq
        compThread lab $ compWakarusa prog
        return $ lab
compWakarusa (R n := expr) = do
--        p <- getPred
        -- connect (lab,pc) (lab,pc + 1)
        exprCode <- compWakarusaExpr expr
        addAssignment (R n) exprCode
        incPC
compWakarusa (GOTO lab) = do
        recordJump lab
        noFallThough
        return ()
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


compWakarusaExpr :: (Rep a) => EXPR a -> WakarusaComp (Seq a)
compWakarusaExpr (REG (R r)) = getRegRead r
compWakarusaExpr (OP2 f e1 e2) = do
        c1 <- compWakarusaExpr e1
        c2 <- compWakarusaExpr e2
        return $ f c1 c2
compWakarusaExpr (OP0 lit) = do
        return $ lit
compWakarusaExpr _ = error "compWakarusaExpr"


-- add assignment in the context of the PC
addAssignment :: (Rep a) => REG a -> Seq a -> WakarusaComp ()
addAssignment reg expr = do
        p <- getPred
        registerAction reg p expr

test :: ([LABEL], WakarusaState)
test = (labels,st)
  where
        res0 = runStateT (compWakarusa prog1) 
        res1 = res0 $ WakarusaState 
                    { ws_uniq = 0 
                    , ws_fabric = return ()
                    , ws_regs = Map.empty
                    , ws_assignments = Map.empty
                    , ws_pc = 0
                    , ws_fallthrough = False
                    , ws_pcs = Map.empty
                    }
        res2 = runReaderT res1
        res3 = res2 $ WakarusaEnv 
                    { we_label = Nothing
                    , we_writes = ws_assignments st
                    , we_reads  = placeRegisters (ws_regs st) $ ws_assignments st
                    , we_pcs = Map.mapWithKey (placePC labels) $ ws_pcs st
                    , we_labels = Map.empty
                    } 
        (labels,st) = runIdentity res3

placePC :: [LABEL] -> LABEL -> Seq (Enabled (Enabled PC)) -> Seq (Enabled PC)
placePC starts lab inp = out
   where
           out = registerEnabled initial 
               $ cASE [ (isEnabled inp, inp)
                      , (isEnabled out, enabledS $ enabledS $ (enabledVal out + 1))
                      ] disabledS

           initial :: Enabled PC
           initial = if lab `elem` starts then Just 0 else Nothing


placeRegisters :: Map Uniq (Pad -> Pad) -> Map Uniq Pad -> Map Uniq Pad
placeRegisters regMap = Map.mapWithKey (\ k p -> 
        case Map.lookup k regMap of
          Nothing -> error $ "can not find register for " ++ show k
          Just f  -> f p)
 
 
 
fab = ws_fabric (snd test)
t = fromJust (head [ fromUni p |  ("o0",p) <- snd (runFabric fab []) ]) :: (Seq (Enabled Int))

          
          
          