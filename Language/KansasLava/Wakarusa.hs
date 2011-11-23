{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}

module Language.KansasLava.Wakarusa 
        ( STMT(..)
        , LABEL(..)
        , REG(..)
        , EXPR(..)
        , VAR(..)
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
                    , ws_label = Nothing
                    , ws_regs = Map.empty
                    , ws_assignments = Map.empty
                    , ws_inputs = Map.empty
                    , ws_pc = 0
                    , ws_labels = Map.empty
                    , ws_pcs = []
                    }
        let res2 = runReaderT res1

        let

        rec res3 <- res2 $ WakarusaEnv 
                    { we_pred  = Nothing
                    , we_writes = ws_assignments st
                    , we_reads  = ws_inputs st `Map.union`
                                  (placeRegisters (ws_regs st) $ ws_assignments st)
                    , we_pcs = generatePredicates (ws_labels st) (ws_pcs st) (ws_pc st) labels
--                    Map.mapWithKey (placePC labels) $ ws_pcs1
                    } 
            let (labels,st) = res3
--                ws_pcs1 = Map.union (ws_pcs st)
--                                    (Map.fromList [(lab,disabledS) | lab <- labels])

        return ()

generatePredicates 
        :: Map LABEL PC                         -- ^ label table
        -> [(PC,Maybe (Seq Bool),LABEL)]        -- ^ jumps
        -> PC                                   -- ^ last PC number + 1
        -> [LABEL]                              -- ^ thread starts
        -> Map PC (Seq Bool)                    -- ^ table of predicates
                                                --   for each row of instructions
generatePredicates label_table jumps pc threads = Map.fromList 
        [ (n,pureS n .==. head pcs)
        | n <- [0..(pc - 1)]
        ]
  where
          -- a list of thread PC's
          pcs :: [Seq PC]
          pcs = [ let pc_reg = register pc
                                        -- we are checking the match with PC twice?
                             $ cASE [ (pureS pc_src .==. pc_reg  .&&.
                                       (case opt_pred of
                                          Nothing -> high
                                          Just p -> p),         pureS dest_pc)

                                    | (pc_src,opt_pred,dest_label) <- jumps
                                    , let Just dest_pc = Map.lookup dest_label label_table
                                    ]
                                    (pc_reg + 1)
                  in pc_reg
                | th_label <- threads
                , let Just pc = Map.lookup th_label label_table
                ]

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

compWakarusa (REGISTER def) = do
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
compWakarusa (LABEL) = do
        -- get the number of the thread
        uq <- getUniq
        let lab = L uq
        newLabel lab
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
          -- can be replaced with compWakarusaStmt?
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
compWakarusaStmt (PAR es) = do
        mores <- mapM compWakarusaStmt es
        return $ and mores        
compWakarusaStmt s = error $ "compWakarusaStmt : unsupport operation construct : \n" ++ show s

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