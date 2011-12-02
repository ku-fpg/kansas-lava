{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa.Monad where

import Language.KansasLava.Wakarusa.AST

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Probes
import Language.KansasLava.Rep
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Protocols.Patch
import Language.KansasLava.Universal
import Language.KansasLava.Utils
import Language.KansasLava.Types

import Data.Sized.Ix

import Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

import Debug.Trace

data WakarusaState = WakarusaState
        { ws_uniq     :: Uniq

        ----------------------------------------------------------
        -- conserning the placement of instructions

        , ws_pred     :: Pred
                -- ^ The predicate for the next instruction
        , ws_filled   :: SlotStatus
                -- ^ Has an instruction (or more in parallel) been issued in this cycle?
                

        ----------------------------------------------------------
        -- All Predicates

        , ws_preds  :: Map Uniq (Seq Bool)

        ----------------------------------------------------------
        -- Registers

        , ws_regs     :: [WritePortInfo]

--        , ws_lookups  :: Map Uniq Pad
        , ws_assigns  :: Map Uniq Pad
          -- ^ The registers, with the way of mapping from
          --   Enabled assignment to result value (typically using registerEnabled)

--        , ws_patches :: Map Uniq PatchInfo
--          -- ^ (untyped) assignments, chained into a single Seq (Enabled a).

        ----------------------------------------------------------
        -- Memories

        , ws_mems     :: Map Uniq (Maybe Pad -> Maybe Pad -> Pad)
           --- ^ The memories, which provide the function :: sig (Enabled (ix,a)) -> sig ix -> sig a

        , ws_mem_writes :: Map Uniq Pad
          -- ^ (untyped) write assignments, chained into a single Seq (Enabled (ix,a)).

        , ws_mem_reads :: Map Uniq Pad 
          -- ^ (untyped) read assignments, chained into a single Seq (Enabled ix)


        ----------------------------------------------------------
        -- Control flow

        , ws_pc       :: PC             -- ^ The PC 
        , ws_labels   :: Map LABEL PC   -- where the labels are
        , ws_pcs    :: [(PC,Pred,LABEL)]
                        -- if conditional holds, then jump to the given label

        ----------------------------------------------------------
        -- Global Threads
        
        , ws_fork     :: [LABEL]

        ----------------------------------------------------------
        -- Debugging output

        , ws_pp       :: [String]               -- a reverse of the output

        }

data WritePortInfo
        = WritePortInfo
        { ri_regs        :: [Pad] -> Fabric [Pad]  -- :: sig [Enabled a|b|c] -> Fabric [sig a|b|c]
        , ri_read_ports  :: [Uniq]
        , ri_write_ports :: [Uniq]
        }
{-
        | PatchInfo
        { ri_patches :: (Pad,Pad) -> Fabric (Pad,Pad)
                                               -- :: Patch (sig (E a)) (sig (E b)) (sig (E c)) (sig (E d))
        , ri_patch_ass_1 :: Pad
        , ri_patch_ass_2 :: Pad
        }
-}
--        deriving Show

-- Placeholder
{-
instance Show WakarusaState where
        show (WakarusaState u regs assignments inputs pc _ pcs) = 
                "uniq : " ++ show u ++ "\n" ++
                "regs : " ++ show (fmap (const ()) regs) ++ "\n" ++
                "assignments : " ++ show (fmap (const ()) assignments) ++ "\n" ++
                "inputs : " ++ show (fmap (const ()) inputs) ++ "\n" ++                
                "pc : " ++ show pc ++ "\n" ++
                "pcs : " ++ show (fmap (const ()) pcs)
-}

data WakarusaEnv = WakarusaEnv
        { we_reads    :: Map Uniq Pad           -- register output  (2nd pass)

        , we_mem_reads :: Map Uniq Pad          -- 


        , we_preds  :: Map Uniq (Seq Bool)
                -- ^ symbol table for all user predicates
        , we_pcs      :: Map PC (Seq Bool)
                -- ^ is this instruction being executed right now?
        , we_pidtable :: Map PC Int     -- Map PC to Pid
                
        , we_pp       :: Pass        -- which version of the syntax should be printed?
        , we_pp_scope :: Int         -- depth when pretty printing
        }
        deriving Show

data Pass
        = Parsed
        | Threaded              -- adding the threadid numbers
  deriving (Show, Eq, Ord)

data PrettyScope = InPred | InPar
  deriving Show
        
------------------------------------------------------------------------------

type WakarusaComp = StateT WakarusaState (ReaderT WakarusaEnv Fabric)

------------------------------------------------------------------------------

type Uniq = Int
type PC = X256

------------------------------------------------------------------------------

data SlotStatus = SlotStatus
        { ss_full :: Bool       -- has the slot been filled
        , ss_par  :: Bool       -- are we in a parallel context?
        }
        deriving (Show)

------------------------------------------------------------------------------
-- Quick AST for Pred. Later will allow some sort of pretty printer

data Pred = LitPred Bool         
          | Pred Uniq
          | NotPred Pred
          | AndPred Pred Pred    --  i & p
          | OrPred  Pred Pred    -- not i | p
        deriving (Show,Eq)

truePred :: Pred
truePred = LitPred True

falsePred :: Pred
falsePred = LitPred False

singletonPred :: Uniq -> Pred
singletonPred = Pred

notPred :: Pred -> Pred
notPred (LitPred b) = LitPred (not b)
notPred p = NotPred p

andPred :: Pred -> Pred -> Pred
andPred (LitPred b1) (LitPred b2) = LitPred (b1 && b2)
andPred p1 p2 = AndPred p1 p2

-- attempt to simplify the or.
orPred :: Pred -> Pred -> Pred
orPred (LitPred b1) (LitPred b2) = LitPred (b1 || b2)
orPred (AndPred (NotPred p1) p2) (AndPred p3 p4) 
   | p1 == p3 = orPred p2 p4
orPred p1 p2 = trace (show ("ordPred",p1,p2)) $ OrPred p1 p2

fromPred :: Pred -> Map Uniq (Seq Bool) -> Seq Bool
fromPred (LitPred p)      _ = pureS p
fromPred (Pred u1)       mp = mp ! u1
fromPred (NotPred p)     mp = bitNot (fromPred p mp)
fromPred (AndPred p1 p2) mp = fromPred p1 mp .&&. fromPred p2 mp 
fromPred (OrPred p1 p2)  mp = fromPred p1 mp .||. fromPred p2 mp 

------------------------------------------------------------------------------
-- Uniq names; simple enought generator

getUniq :: WakarusaComp Int
getUniq = do
        st <- get
        let u = ws_uniq st + 1
        put $ st { ws_uniq = u }
        return u
        

------------------------------------------------------------------------------
-- Allocate predicates

newPred :: Seq Bool -> WakarusaComp Int
newPred p = do
        u <- getUniq
        modify $ \ st -> st { ws_preds = Map.insert u p (ws_preds st) }
        return u

------------------------------------------------------------------------------
-- Concerning the allocation of PC to instructions

getPC :: WakarusaComp PC
getPC = do
        st <- get
        return $ ws_pc st 
        
recordJump :: LABEL -> WakarusaComp ()
recordJump lab =  do
        pc <- getPC
        st <- get
        -- Does not use getPred here, because we 
        -- want to store the partial result; the PC stores the other part of the condition
        -- In this way, we might be able to analysis control flow  (AJG: unconvinsed)
        modify (\ st -> st { ws_pcs = (pc,ws_pred st,lab) : ws_pcs st 
                           , ws_pred = falsePred
                           })
        return ()

resetInstSlot :: WakarusaComp ()
resetInstSlot = return () -- modify $ \ st -> st { ws_filled = False }

markInstSlot :: WakarusaComp ()
markInstSlot = do
        st <- get
        put $ case ws_filled st of
          SlotStatus False p -> st { ws_filled = SlotStatus True p }
          SlotStatus True  p -> error "attempting to fill an already full instruction slot"

-- note this is idenpotent.
prepareInstSlot :: WakarusaComp ()
prepareInstSlot = do
        st <- get
        case ws_filled st of
          SlotStatus False False ->
                return ()
          SlotStatus False True  ->  
                return ()
          SlotStatus True  False -> do
                -- empty the slot by incrementing the PC
                put $ st { ws_pc = ws_pc st + 1
                         , ws_pred = truePred 
                         , ws_filled = SlotStatus False False
                         }
                prettyPrintPC
          SlotStatus True  True ->
                error "attempting to insert more than one consecutive instruction into a par"

prettyPrintPC = do
        st <- get
        env <- ask
        prettyPrint (\ o -> "{pc = " ++ show (ws_pc st) ++ 
                        if o >= Threaded 
                        then case Map.lookup (ws_pc st) (we_pidtable env) of
                                Nothing  -> ", pid = ??}"
                                Just pid -> ", pid = " ++ show pid ++ "}"
                        else "}")
        topLevelPrettyPrint "\n" 

parInstSlot :: WakarusaComp () -> WakarusaComp ()
parInstSlot m = do
        st0 <- get
        put $ st0 { ws_filled = SlotStatus False True }
        m
        st1 <- get
        put $ st1 { ws_filled = (ws_filled st1) { ss_par = ss_par (ws_filled st0) } }

newLabel :: LABEL -> WakarusaComp ()
newLabel lab = do
        modify (\ st0 -> st0 { ws_labels = insert lab (ws_pc st0) (ws_labels st0) 
                             , ws_pred = truePred  -- someone arrives here!
                              })

------------------------------------------------------------------------------

addChannel :: forall a b . (Rep a, Rep b) => (Seq (Enabled a) -> Fabric (Seq b)) -> WakarusaComp Uniq
addChannel fn = do
        k <- getUniq
        modify $ \ st -> st { ws_regs = regInfo k : ws_regs st
                            , ws_assigns = Map.insert k (toUni (disabledS :: Seq (Enabled a))) (ws_assigns st)
                            }
        return k
  where
          regInfo :: Uniq -> WritePortInfo 
          regInfo k = WritePortInfo 
                { ri_regs    = \ [p] -> mapMPad fn p >>= \ p' -> return [p']
                , ri_read_ports = [k]
                , ri_write_ports = [k]
                }

-- This is primitive because we can *not* join the outputs; 
-- because of the timing / direction issues.

addPatch :: forall a b c d 
          . (Rep a, Rep b, Rep c, Rep d) 
         => (Patch (Seq (Enabled a)) (Seq (Enabled b))
                   (Seq (Enabled c)) (Seq (Enabled d)))
         -> WakarusaComp (Uniq,Uniq,Uniq,Uniq)
addPatch p = do
        a <- getUniq
        b <- getUniq
        c <- getUniq
        d <- getUniq

        modify $ \ st -> st { ws_regs = regInfo a b c d : ws_regs st
                            , ws_assigns = 
                                  Map.insert a (toUni (disabledS :: Seq (Enabled a)))
                                $ Map.insert d (toUni (disabledS :: Seq (Enabled d)))
                                $ ws_assigns st
                            }
        return (a,b,c,d)
  where
          regInfo :: Uniq -> Uniq -> Uniq -> Uniq -> WritePortInfo 
          regInfo a b c d = WritePortInfo 
                { ri_regs    = \ [p1,p2] ->
                        let (p3,p4) = execP p (fromUni' p1,fromUni' p2)
                        in return [toUni p3,toUni p4]
                , ri_read_ports = [a,d]
                , ri_write_ports = [c,b]
                }



-- Should be reg
registerAction :: forall a . (Rep a) => REG a -> Seq Bool -> Seq a -> WakarusaComp ()
registerAction (R k) en val = do
        let updateAssign old = return (mapPad (chooseEnabled (packEnabled en val)) old)

        modify $ \ st -> st { ws_assigns = Map.update updateAssign k $ ws_assigns st }
        return ()


addMemory :: forall ix a. (Rep a, Rep ix, Size ix) => Uniq -> Witness (MEM ix a) -> WakarusaComp ()
addMemory k Witness = do
        st <- get
        let m = Map.insert k (\ ix a -> toUni (f (fmap fromUni ix) (fmap fromUni a))) (ws_mems st)
        put $ st { ws_mems = m }
        return ()
  where
          f :: Maybe (Maybe (Seq (Enabled (ix,a)))) -> Maybe (Maybe (Seq ix)) -> Seq a
          f Nothing _ = undefinedS
          f _ Nothing = undefinedS
          f (Just (Just wt)) (Just (Just rd)) = asyncRead (writeMemory wt) rd 
          f _ _ = error $ "internal type error with memory : " ++ show k
          
{-
addInput :: forall a. (Rep a) => REG a -> Seq a -> WakarusaComp ()
addInput (R k) inp = do
        st <- get
        let m = Map.insert k (toUni inp) (ws_inputs st)
        put $ st { ws_inputs = m }
        return ()
-}

-- get the predicate for *this* instruction
getPred :: WakarusaComp (Seq Bool)
getPred = do
            pc <- getPC
            st <- get
            env <- ask
            return $ case Map.lookup pc (we_pcs env) of
              Nothing     -> error $ "can not find the PC predicate for " ++ show pc ++ " (no control flow to this point?)"
              Just pc_pred -> pc_pred .&&. fromPred (ws_pred st) (we_preds env)

-- set a predicate inside a context.

setPred :: Seq Bool -> WakarusaComp a -> WakarusaComp a
setPred p m = do
        p_uq <- newPred p
        uq <- getUniq
        st0 <- get
        put (st0 { ws_pred = andPred (singletonPred p_uq) (ws_pred st0) })
--        () <- trace ("set pred"  ++ show (andPred p (ws_pred st0)))  $ return ()
        r <- m
        st1 <- get
        -- Two control flow branches; one predicated (and might terminate/jump),
        -- the other passing over.
        put (st1 { ws_pred = orPred (andPred (notPred (singletonPred p_uq)) (ws_pred st0)) (ws_pred st1) })
        return r




{-
  where
   f env = env { we_pred = case we_pred env of
                             Nothing -> Just p
                             Just p' -> Just (p' .&&. p)
               }
-}

--getLabel :: WakarusaComp (Maybe LABEL)
--getLabel = do
--        st <- get
--        return $ ws_label st

getRegRead :: (Rep a) => Uniq -> WakarusaComp (Seq a)
getRegRead k = do
        env <- ask
        -- remember to return before checking error, to allow 2nd pass
        return $ case Map.lookup k (we_reads env) of
           Nothing -> error $ "getRegRead, can not find : " ++ show k
           Just p  -> case fromUni p of
                        Nothing -> error $ "getRegRead, coerce error in : " ++ show k
                        Just e -> e


getMemRead :: forall a ix . (Rep a, Rep ix, Size ix) => Uniq -> Seq ix -> WakarusaComp (Seq a)
getMemRead k ix = do
        p <- getPred
        -- There are two parts, recording the *read* address as a type
        -- of write event, and the reading of the value.
        st <- get
        let f :: Seq a -> Seq a -> Seq a
            f a b = mux p (b,a)

        put $ st { ws_mem_reads = Map.insertWith (zipPad f) k (toUni ix) (ws_mem_reads st)
                 }

        env <- ask        -- remember to return before checking error, to allow 2nd pass
        return $ case Map.lookup k (we_mem_reads env) of
           Nothing -> error $ "getMemRead, can not find : " ++ show k
           Just p  -> case fromUni p of
                        Nothing -> error $ "getMemRead, coerce error in : " ++ show k
                        Just e -> e

prettyPrint :: (Pass -> String) -> WakarusaComp ()
prettyPrint fn = do
        env <- ask
        modify $ \ st -> st { ws_pp = addText (fn $ we_pp env) (ws_pp st) } 
  where
          addText :: String -> [String] -> [String]
          addText txt []      = [txt]
          addText txt ([]:xs) = txt : xs
          addText txt (x:xs) | last x == '\n' = txt : x : xs
                             | otherwise      = (x ++ txt) : xs
        
        
addPrettyScope :: Int -> WakarusaComp a -> WakarusaComp a
addPrettyScope ps m = do
        env <- ask
        if ps < we_pp_scope env then prettyPrint (const " (") else return ()
        r <- local (\ env -> env { we_pp_scope = ps }) m
        if ps < we_pp_scope env then prettyPrint (const ") ") else return ()
        return r

topLevelPrettyPrint :: String -> WakarusaComp ()
topLevelPrettyPrint str = do
        we <- ask
        case we_pp_scope we of
           0 -> prettyPrint (const str)
           _ -> return ()


addToFabric :: Fabric a -> WakarusaComp a
addToFabric f = lift (lift f)

-- No more execution statements, so we unset the label
--noFallThrough :: WakarusaComp ()
--noFallThrough = modify (\ st -> st { ws_label = Nothing })

------------------------------------------------------------------------------------

addFork :: LABEL -> WakarusaComp ()
addFork lab = modify $ \ st -> st { ws_fork = lab : ws_fork st }

------------------------------------------------------------------------------------

zipPad :: (Rep a, Rep b, Rep c) => (Seq a -> Seq b -> Seq c) -> Pad -> Pad -> Pad
zipPad f p1 p2 = g (fromUni p1) (fromUni p2)
  where
          g (Just a) (Just b) = toUni $ f a b
          g _        _        = error "zipPad, failed to coerce argument"

mapMPad :: (Monad m, Rep a, Rep b) => (Seq a -> m (Seq b)) -> Pad -> m Pad
mapMPad f p1 = g (fromUni p1)
  where
          g (Just a) = do r <- f a
                          return (toUni r)
          g _        = fail "mapMPad, failed to coerce argument"
          
mapPad :: (Rep a, Rep b) => (Seq a -> Seq b) -> Pad -> Pad
mapPad f p1 = g (fromUni p1)
  where 
          g (Just a) = toUni $ f a
          g _        = error "mapPad, failed to coerce argument"

