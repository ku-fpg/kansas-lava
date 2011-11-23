{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa 
        ( STMT(..)
        , LABEL(..)
        , REG(..)
        , EXPR(..)
        , thread
        , VAR'(..)
        , compileToFabric
        ) where

import Language.KansasLava.Wakarusa.AST
import Language.KansasLava.Wakarusa.Monad

import Language.KansasLava.Signal
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Utils

import Control.Monad.Fix
import Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader

------------------------------------------------------------------------------

compileToFabric :: STMT [LABEL] -> Fabric () 
compileToFabric prog = do 
        let res0 = runStateT (compWakarusa prog)
        let res1 = res0 $ WakarusaState 
                    { ws_uniq = 0 
                    , ws_regs = Map.empty
                    , ws_assignments = Map.empty
                    , ws_inputs = Map.empty
                    , ws_pc = 0
                    , ws_fallthrough = False
                    , ws_pcs = Map.empty
                    }
        let res2 = runReaderT res1

        rec res3 <- res2 $ WakarusaEnv 
                    { we_label = Nothing
                    , we_pred  = Nothing
                    , we_writes = ws_assignments st
                    , we_reads  = ws_inputs st `Map.union`
                                  (placeRegisters (ws_regs st) $ ws_assignments st)
                    , we_pcs = Map.mapWithKey (placePC labels) $ ws_pcs st
                    , we_labels = Map.empty
                    } 
            let (labels,st) = res3

        return ()

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
        addToFabric (connect wt)
        return $ R uq
compWakarusa (INPUT fab) = do
        inp <- addToFabric fab
        -- Why not just return the inp?
        --  * Can not use OP0 (requires combinatorial value)
        --  * We *could* add a new constructor, IN (say)
        --  * but by using a REG, we are recording the
        --    use of an INPUT that changes over time,
        --    in the same way as registers are accesssed.
        u <- getUniq
        let reg = R u
        addInput reg inp
        return (REG reg)
compWakarusa (THREAD prog) = do
        -- get the number of the thread
        uq <- getUniq
        let lab = L uq
        compThread lab $ compWakarusa prog
        return $ lab
compWakarusa e@(_ := _) = compWakarusaSeq e
compWakarusa e@(GOTO _) = compWakarusaSeq e
compWakarusa e@(_ :? _) = compWakarusaSeq e
compWakarusa e@(PAR _)  = compWakarusaPar e
compWakarusa _ = error "compWakarusa _"


------------------------------------------------------------------------------

compWakarusaSeq :: STMT () -> WakarusaComp ()
compWakarusaSeq e = do
        more <- compWakarusaStmt e
        if not more then noFallThrough else incPC
        return ()

compWakarusaPar :: STMT () -> WakarusaComp ()
compWakarusaPar e = do
        more <- compWakarusaPar' e
        if not more then noFallThrough else incPC
        return ()        
  where
   compWakarusaPar' (PAR es) = do
        mores <- mapM compWakarusaPar' es
        return $ and mores
   compWakarusaPar' o = compWakarusaStmt o
        

------------------------------------------------------------------------------

compWakarusaStmt :: STMT () -> WakarusaComp Bool
compWakarusaStmt (R n := expr) = do
        exprCode <- compWakarusaExpr expr
        addAssignment (R n) exprCode
        return True
compWakarusaStmt (GOTO lab) = do
        recordJump lab
        return False
compWakarusaStmt (e1 :? m) = do
        predCode <- compWakarusaExpr e1
        _ <- setPred predCode $ compWakarusaStmt m
        return True  -- if predicated, be pesamistic, and assume no jump was taken
compWakarusaStmt _ = error "compWakarusaStmt : unsupport operation construct"

------------------------------------------------------------------------------

compWakarusaExpr :: (Rep a) => EXPR a -> WakarusaComp (Seq a)
compWakarusaExpr (REG (R r)) = getRegRead r
compWakarusaExpr (OP0 lit) = do
        return $ lit
compWakarusaExpr (OP1 f e) = do
        c <- compWakarusaExpr e
        return $ f c
compWakarusaExpr (OP2 f e1 e2) = do
        c1 <- compWakarusaExpr e1
        c2 <- compWakarusaExpr e2
        return $ f c1 c2

------------------------------------------------------------------------------

-- add assignment in the context of the PC
addAssignment :: (Rep a) => REG a -> Seq a -> WakarusaComp ()
addAssignment reg expr = do
        p <- getPred
        registerAction reg p expr

------------------------------------------------------------------------------