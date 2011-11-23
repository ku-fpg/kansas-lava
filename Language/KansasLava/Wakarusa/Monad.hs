{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa.Monad where

import Language.KansasLava.Wakarusa.AST

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Universal
import Language.KansasLava.Utils

import Data.Sized.Ix

import Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

data WakarusaState = WakarusaState
        { ws_uniq   :: Uniq
        , ws_regs        :: Map Uniq (Pad -> Pad)       
          -- ^ add the register, please, 
        , ws_assignments :: Map Uniq Pad
          -- ^ (untyped) assignments, chained into a single Seq (Enabled a).
        , ws_inputs      :: Map Uniq Pad                -- sig a
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

-- Placeholder
instance Show WakarusaState where
        show (WakarusaState u regs assignments inputs pc _ pcs) = 
                "uniq : " ++ show u ++ "\n" ++
                "regs : " ++ show (fmap (const ()) regs) ++ "\n" ++
                "assignments : " ++ show (fmap (const ()) assignments) ++ "\n" ++
                "inputs : " ++ show (fmap (const ()) inputs) ++ "\n" ++                
                "pc : " ++ show pc ++ "\n" ++
                "pcs : " ++ show (fmap (const ()) pcs)

data WakarusaEnv = WakarusaEnv
        { we_label    :: Maybe LABEL            --
        , we_pred     :: Maybe (Seq Bool)       -- optional predicate on specific instructions

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

type WakarusaComp = StateT WakarusaState (ReaderT WakarusaEnv Fabric)

------------------------------------------------------------------------------

type Uniq = Int
type PC = X256

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


addInput :: forall a. (Rep a) => REG a -> Seq a -> WakarusaComp ()
addInput (R k) inp = do
        st <- get
        let m = Map.insert k (toUni inp) (ws_inputs st)
        put $ st { ws_inputs = m }
        return ()

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
              Just pc_sig -> foldr1 (.&&.) $
                                [ isEnabled pc_sig 
                                , enabledVal pc_sig .==. pureS pc
                                ] ++ case we_pred env of
                                        Nothing -> []
                                        Just local_pred -> [ local_pred ]


setPred :: Seq Bool -> WakarusaComp a -> WakarusaComp a
setPred p m = local f m
  where
   f env = env { we_pred = case we_pred env of
                             Nothing -> Just p
                             Just p' -> Just (p' .&&. p)
               }

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
          
addToFabric :: Fabric a -> WakarusaComp a
addToFabric f = lift (lift f)

noFallThrough :: WakarusaComp ()
noFallThrough = modify (\ st -> st { ws_fallthrough = False })
