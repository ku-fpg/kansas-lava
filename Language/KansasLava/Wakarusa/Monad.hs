{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa.Monad where

import Language.KansasLava.Wakarusa.AST

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Probes
import Language.KansasLava.Rep
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Universal
import Language.KansasLava.Utils
import Language.KansasLava.Types

import Data.Sized.Ix

import Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

data WakarusaState = WakarusaState
        { ws_uniq     :: Uniq

        ----------------------------------------------------------
        -- conserning the placement of instructions

        , ws_pred     :: Pred
                -- ^ The predicate for the next instruction
        , ws_filled   :: SlotStatus
                -- ^ Has an instruction (or more in parallel) been issued in this cycle?
                
        ----------------------------------------------------------
        -- Registers

        , ws_regs     :: Map Uniq RegisterInfo
          -- ^ The registers, with the way of mapping from
          --   Enabled assignment to result value (typically using registerEnabled)

--        , ws_assignments :: Map Uniq Pad
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
        , ws_pcs    :: [(PC,Maybe (Seq Bool),LABEL)]
                        -- if conditional holds, then jump to the given label
        }

data RegisterInfo
        = RegisterInfo
        { ri_regs    :: Pad -> Fabric Pad      -- :: sig (Enabled a) -> Fabric (sig a)
        , ri_assigns :: Pad                    -- :: sig (Enabled a)
        }

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


        , we_pcs      :: Map PC (Seq Bool)
                -- ^ is this instruction being executed right now?
        }
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

data Pred = Pred (Seq Bool)     -- list of ands

truePred :: Pred
truePred = Pred high

falsePred :: Pred
falsePred = Pred low

notPred :: Pred -> Pred
notPred (Pred p) = Pred $ bitNot p

andPred :: Seq Bool -> Pred -> Pred
andPred p (Pred p') = Pred (p .&&. p')

orPred :: Seq Bool -> Pred -> Pred
orPred p (Pred p') = Pred (p .||. p')

fromPred :: Pred -> Seq Bool
fromPred (Pred p) = p

------------------------------------------------------------------------------
-- Uniq names; simple enought generator

getUniq :: WakarusaComp Int
getUniq = do
        st <- get
        let u = ws_uniq st + 1
        put $ st { ws_uniq = u }
        return u
        
------------------------------------------------------------------------------
-- Concerning the allocation of PC to instructions

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
        pc <- getPC
        st <- get
        -- Does not use getPred here, because we 
        -- want to store the partial result; the PC stores the other part of the condition
        -- In this way, we might be able to analysis control flow  (AJG: unconvinsed)
        modify (\ st -> st { ws_pcs = (pc,Just $ fromPred (ws_pred st),lab) : ws_pcs st 
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
          SlotStatus True  True ->
                error "attempting to insert more than one consecutive instruction into a par"

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
-- TODO: addSignal => addChannel, REG a => Uniq.

addSignal :: forall a b . (Rep a, Rep b) => REG a -> (Seq (Enabled a) -> Fabric (Seq b)) -> WakarusaComp ()
addSignal r@(R k) fn = do
        modify $ \ st -> st { ws_regs = Map.insert k regInfo (ws_regs st) }
        return ()
  where
          regInfo :: RegisterInfo 
          regInfo = RegisterInfo 
                { ri_regs    = mapMPad fn
                , ri_assigns = toUni (disabledS :: Seq (Enabled a))
                }

-- Should be reg
registerAction :: forall a . (Rep a) => REG a -> Seq Bool -> Seq a -> WakarusaComp ()
registerAction (R k) en val = do
        let updateAssign regInfo = return
                                 $ regInfo { ri_assigns = mapPad (chooseEnabled (packEnabled en val))
                                                                 (ri_assigns regInfo)
                                           }

        modify $ \ st -> st { ws_regs = Map.update updateAssign k $ ws_regs st }
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
              Just pc_pred -> pc_pred .&&. fromPred (ws_pred st)

-- set a predicate inside a context.

setPred :: Seq Bool -> WakarusaComp a -> WakarusaComp a
setPred p m = do
        st0 <- get
        put (st0 { ws_pred = andPred p (ws_pred st0) })
        r <- m
        st1 <- get
        -- Two control flow branches; one predicated (and might terminate/jump),
        -- the other passing over.
        put (st1 { ws_pred = orPred (bitNot p) (ws_pred st1) })
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

addToFabric :: Fabric a -> WakarusaComp a
addToFabric f = lift (lift f)

-- No more execution statements, so we unset the label
--noFallThrough :: WakarusaComp ()
--noFallThrough = modify (\ st -> st { ws_label = Nothing })

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

