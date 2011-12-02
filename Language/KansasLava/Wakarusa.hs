{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies, FlexibleContexts #-}

module Language.KansasLava.Wakarusa 
        ( STMT(..)
        , LABEL(..)
        , REG(..)
        , EXPR(..)
        , VAR(..)
        , MEM(..)
        , (|||)
        , compileToFabric
        , ReadableAckBox(..)
        , newAckBox
        , connectReadableAckBox
        , WritableAckBox(..)
        , connectWritableAckBox
        , takeAckBox
        , putAckBox
        , Variable(..)
        , var, undefinedVar
        , memory, Memory(..)
        , always
        , for
        , if_then_else
        , mkEnabled
        , mkChannel
        ) where

import Language.KansasLava.Wakarusa.AST
import Language.KansasLava.Wakarusa.Monad

import Language.KansasLava.Signal
import Language.KansasLava.Probes
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Types
import Language.KansasLava.Universal


import Data.Sized.Ix

import Control.Monad.Fix
import Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader

import Debug.Trace
import System.IO.Unsafe

import qualified Data.Set as Set
import Data.Set (Set)

------------------------------------------------------------------------------

traceRet :: (a -> String) -> String -> a -> a 
traceRet showMe msg a = trace (msg ++ " : " ++ showMe a) a


compileToFabric :: STMT () -> Fabric () 
compileToFabric prog = do -- traceRet (show . unsafePerformIO . reifyFabric) "compileToFabric" $ do 
        let res0 = runStateT (compWakarusa prog)
        let res1 = res0 $ WakarusaState 
                    { ws_uniq = 0 
                    , ws_pred  = falsePred
                    , ws_filled = SlotStatus False False
                       -- predicates
                    , ws_preds = Map.empty
                       -- registers
                    , ws_regs = []
                    , ws_assigns = Map.empty
                      -- memories
                    , ws_mems       = Map.empty
                    , ws_mem_reads  = Map.empty
                    , ws_mem_writes = Map.empty

                    , ws_pc = 0
                    , ws_labels = Map.empty
                    , ws_pcs = []
                    , ws_fork = []

                    , ws_pp = []
                    }
        let res2 = runReaderT res1

        rec (_,st) <- res2 $ WakarusaEnv 
                    { we_reads     = the_vars
                    , we_pcs       = generatePredicates st (ws_labels st) (ws_pcs st) (ws_pc st) (ws_fork st)
                    , we_preds     = ws_preds st
                    , we_mem_reads = placeMemories (ws_mems st) (ws_mem_reads st) (ws_mem_writes st)
                    , we_pp        = Parsed
                    , we_pp_scope  = 0
                    }

            () <- trace ("--parsed debugging") $ return ()
            () <- trace (concat $ reverse $ ws_pp $ st) $ return ()
            () <- trace ("--end of parsed debugging") $ return ()
            
            -- connect the inputs and outputs; instantiate the registers
            the_vars <- sequence
                       [ placeRegister reg $ (ws_assigns st)
                       | reg <- ws_regs st
                       ] >>= (return . Map.unions)
           

        return ()


generateThreadIds 
        :: WakarusaState                -- ^ the (final) state
        -> Map LABEL PC                 -- ^ label table
        -> [(PC,Pred,LABEL)]            -- ^ jumps
        -> PC                           -- ^ last PC number + 1
        -> [LABEL]                      -- ^ thread starts
        -> Map PC Int                   -- Which thread am I powered by
generateThreadIds st label_table jumps pc threads  = 
        trace (show ("generateThreadIds",msg,result)) $
         result
   where
        msg = (label_table,jumps,pc,threads)

        links :: Map PC (Set PC)
        links = Map.fromListWith (Set.union) $
                [ (src_pc, Set.singleton dest_pc)
                | (src_pc,_,dest_label) <- jumps
                , let Just dest_pc = Map.lookup dest_label label_table
                ] ++
                [ (n,Set.singleton $ n+1)
                | n <- if pc == 0 then [] else [0..(pc-1)]
                        -- not pc-1, becasuse the last instruction 
                        -- can not jump to after the last instruction
                , (n `notElem` unconditional_jumps)
                ]


        -- PC values that have unconditional jumps (does not matter where to)
        unconditional_jumps =
                [ src_pc
                | (src_pc,LitPred True,_) <- jumps
                ] 

        result :: Map PC Int
        result =  Map.fromList
                [ (a,pid)
                | (lab,pid) <- zip threads [0..]
                , let Just fst_pc = Map.lookup lab label_table
                , a <- Set.toList $ transitiveClosure (\ a -> Map.findWithDefault (Set.empty) a links) fst_pc
                ]

generatePredicates
        :: WakarusaState                -- ^ the (final) state
        -> Map LABEL PC                         -- ^ label table
        -> [(PC,Pred,LABEL)]                    -- ^ jumps
        -> PC                                   -- ^ last PC number + 1
        -> [LABEL]                              -- ^ thread starts
        -> Map PC (Seq Bool)                    -- ^ table of predicates
                                                --   for each row of instructions
generatePredicates st label_table jumps pc threads = {- trace (show ("generatePredicates",length pcs)) $ -} result
  where
        threadIds = generateThreadIds st label_table jumps pc threads

        -- mapping from *every* instruction to its thread id
        result = mapWithKey (\ k tid -> pureS k .==. pcs !! tid) threadIds

        -- a list of thread PC's
        pcs :: [Seq PC]
        pcs = [ let pc_reg = probeS ("pc for thread " ++ show (pid :: Int))
                           $ register first_pc
                                        -- we are checking the match with PC twice?
                             $ cASE [ ( this_inst .&&. fromPred opt_pred (ws_preds st)
                                      , pureS dest_pc
                                      )
                                    | x@(pc_src,opt_pred,dest_label) <- jumps
                                    , case Map.lookup pc_src threadIds of
                                         Just ans -> ans == pid
                                         Nothing -> error $ "can not find my threadId " ++ show (pc_src,pid,threadIds)
                                    , let Just dest_pc = Map.lookup dest_label label_table
                                    , let this_inst = Map.findWithDefault
                                                                (error $ "this_inst " ++ show (pc_src,fmap (const ()) result))
                                                                pc_src
                                                                result
--                                    , () <- trace (show ("insisde",(x,dest_pc))) [()]
                                    ]
                                    (pc_reg + 1)
                  in pc_reg
                | (th_label,pid) <- zip threads [0..]
                , let Just first_pc = Map.lookup th_label label_table
                ]
{-
placePC :: [LABEL] -> LABEL -> Seq (Enabled (Enabled PC)) -> Seq (Enabled PC)
placePC starts lab inp = out
   where
           out = registerEnabled initial 
               $ cASE [ (isEnabled inp, inp)
                      , (isEnabled out, enabledS $ enabledS $ (enabledVal out + 1))
                      ] disabledS

           initial :: Enabled PC
           initial = if lab `elem` starts then Just 0 else Nothing
-}

placeRegister :: WritePortInfo -> Map Uniq Pad -> Fabric (Map Uniq Pad)
placeRegister (WritePortInfo { ri_regs = reg_fn, ri_read_ports = rds, ri_write_ports = wts }) mp = do
        res <- reg_fn [mp Map.! rd | rd <- rds]
        return (Map.fromList (zip wts res))
 
placeMemories :: Map Uniq (Maybe Pad -> Maybe Pad -> Pad) -> Map Uniq Pad -> Map Uniq Pad -> Map Uniq Pad
placeMemories memMap rdMap wtMap = Map.mapWithKey fn memMap
  where
          fn k f = error "XFDD"
{-
                  case Map.lookup k assignMap of
                     Nothing -> f $ toUni Nothing
                     Just a -> f $ Just a) 
-}

------------------------------------------------------------------------------

--output :: (Seq (Maybe a) -> Fabric ()) -> STMT (REG a)
---output f = CHANEL fn >>

--defVar' :: 

------------------------------------------------------------------------------

compWakarusa :: forall a . STMT a -> WakarusaComp a
compWakarusa (RETURN a) = return a
compWakarusa (BIND m1 k1) = do
        r1 <- compWakarusa m1
        compWakarusa (k1 r1)
compWakarusa (MFIX fn) = mfix (compWakarusa . fn)

compWakarusa (CHANNEL fn) = do
        uq <- addChannel fn
        return $ uq
compWakarusa (SIGNAL fn) = do
        -- add the register to the table
        uq <- addChannel (return . fn)
        return (VAR $ toVAR $ (R uq, REG (R uq)))
compWakarusa (OUTPUT connect) = do
        uq <- addChannel $ \ wt -> do
                        connect wt
                        return (pureS ())
        return $ R $ uq
compWakarusa (INPUT connect) = do
        uq <- addChannel (const connect :: (a ~ EXPR b) => Seq (Enabled b) -> Fabric (Seq b))
        return (REG $ R $ uq)
compWakarusa (PATCH p) = do
        undefined
--        uq <- addChannel fn
--        return $ uq
compWakarusa (LABEL) = do
        -- LABEL implies new instruction block (because you jump to a label)
        prepareInstSlot
        -- get the number of the thread
        uq <- getUniq
        let lab = L uq
        newLabel lab
        prettyPrint (\ _ -> "L" ++ show uq ++ ":")
        topLevelPrettyPrint "\n" 
        return $ lab
compWakarusa (R n := expr) = do
        prepareInstSlot
        exprCode <- compWakarusaExpr expr
        addAssignment (R n) exprCode
        markInstSlot
        prettyPrint (\ p -> " r" ++ show n ++ " := " ++ ppEXPR p expr)
        topLevelPrettyPrint "\n" 
        return ()
compWakarusa (GOTO lab) = do
        prepareInstSlot
        recordJump lab
        markInstSlot
        prettyPrint (\ _ -> " GOTO " ++ show lab)
        topLevelPrettyPrint "\n" 
        return ()
compWakarusa (e1 :? m) = do
        prepareInstSlot
        predCode <- compWakarusaExpr e1
        prettyPrint (\ p -> ppEXPR p e1 ++ " :?")
        setPred predCode $ addPrettyScope 2 $ compWakarusa m
        topLevelPrettyPrint "\n" 
        return ()
compWakarusa (PAR [e1,e2]) = do
        prepareInstSlot
        we <- ask
        addPrettyScope 1 $ do
                parInstSlot $ compWakarusa e1
                prettyPrint (\ _ -> "\n    |||")
                parInstSlot $ compWakarusa e2
        topLevelPrettyPrint "\n" 
        return ()


compWakarusa STEP       = do
-- TODO REMove
--        incPC
--        modify (\ st -> st { ws_pred = falsePred })
        return ()
compWakarusa (FORK lab) = do
        addFork lab
compWakarusa o = error ("compWakarusa : " ++ show o)


------------------------------------------------------------------------------
{-
compWakarusaSeq :: STMT () -> WakarusaComp ()
compWakarusaSeq e = do

        compWakarusaStmt e
        return ()

compWakarusaPar :: STMT () -> WakarusaComp ()
compWakarusaPar e = do
        _ <- compWakarusaPar' e
        incPC
        return ()        
  where
          -- can be replaced with compWakarusaStmt?
   compWakarusaPar' (PAR es) = do
        mores <- mapM compWakarusaPar' es
        return $ and mores
   compWakarusaPar' o = compWakarusaStmt o
        

------------------------------------------------------------------------------

compWakarusaStmt :: STMT () -> WakarusaComp ()

compWakarusaStmt (GOTO lab) = do
        recordJump lab
        return ()
compWakarusaStmt (e1 :? m) = do
        predCode <- compWakarusaExpr e1
        _ <- setPred predCode $ compWakarusaStmt m
        return ()
compWakarusaStmt (PAR es) = do
        mores <- mapM compWakarusaStmt es
        return $ and mores                return ()
compWakarusaStmt (RETURN ()) = return False
compWakarusaStmt (BIND LABEL k1) = do -- getting hacky; breaks monad laws
        r1 <- compWakarusa LABEL
        compWakarusaStmt (k1 r1)
compWakarusaStmt s = error $ "compWakarusaStmt : unsupport operation construct : \n" ++ show s
-}
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
compWakarusaExpr (OP3 f e1 e2 e3) = do
        c1 <- compWakarusaExpr e1
        c2 <- compWakarusaExpr e2
        c3 <- compWakarusaExpr e3
        return $ f c1 c2 c3

ppEXPR :: Pass -> EXPR a -> String
ppEXPR _ (REG r)   = show r
ppEXPR _ (OP0 lit) = "(OP0 " ++ show lit ++ ")"
ppEXPR p (OP1 _ e1) = "(OP1 (...) " ++ ppEXPR p e1 ++ ")"
ppEXPR p (OP2 _ e1 e2) = "(OP2 (...) " ++ ppEXPR p e1 ++ " " ++ ppEXPR p e2 ++ ")"
ppEXPR p (OP3 _ e1 e2 e3) = "(OP3 (...) " ++ ppEXPR p e1 ++ " " ++ ppEXPR p e2 ++ " " ++ ppEXPR p e3 ++ ")"

------------------------------------------------------------------------------

-- add assignment in the context of the PC
addAssignment :: (Rep a) => REG a -> Seq a -> WakarusaComp ()
addAssignment reg expr = do
        p <- getPred
        registerAction reg p expr

------------------------------------------------------------------------------

-- technically, we could just look at the frontier each time.

transitiveClosure :: (Ord a) => (a -> Set a) -> a -> Set a
transitiveClosure f a = fixpoint $ iterate step (Set.singleton a)
   where
           fixpoint (x0:x1:_) | x0 == x1 = x0
           fixpoint (_:xs)    = fixpoint xs
           fixpoint _         = error "reached end of infinite list"
           
           step x = x `Set.union` Set.unions (fmap f (Set.toList x))

--------------------------------------------------------------------------------


data ReadableAckBox a = ReadableAckBox (EXPR (Enabled a)) (REG ())

connectReadableAckBox
        :: forall a . (Rep a, Size (ADD (W a) X1), Show a)
        => String -> String -> STMT (ReadableAckBox a)
connectReadableAckBox inpName ackName = do
        i :: EXPR (Maybe a)   <- INPUT  (inStdLogicVector inpName)
        o :: REG ()           <- OUTPUT (outStdLogic ackName . isEnabled)
        return $ ReadableAckBox i o
                       
takeAckBox :: Rep a => ReadableAckBox a -> (EXPR a -> STMT ()) -> STMT ()
takeAckBox (ReadableAckBox iA oA) cont = do
        self <- LABEL
        do OP1 (bitNot . isEnabled) iA :? GOTO self
                ||| oA := OP0 (pureS ())
                ||| cont (OP1 enabledVal iA)

data WritableAckBox a = WritableAckBox (REG a) (EXPR (Enabled ()))

connectWritableAckBox
        :: forall a . (Rep a, Size (ADD (W a) X1), Show a)
        => String -> String -> STMT (WritableAckBox a)
connectWritableAckBox outName ackName = do
        iB :: EXPR (Enabled ()) <- INPUT  (inStdLogic ackName)
        oB :: REG a             <- OUTPUT (outStdLogicVector outName)
        return $ WritableAckBox oB iB

putAckBox :: Rep a => WritableAckBox a -> EXPR a -> STMT ()
putAckBox (WritableAckBox oB iB) val = do
        self <- LABEL 
        oB := val 
                ||| OP1 (bitNot . isEnabled) iB :? GOTO self

newAckBox :: forall a . (Rep a) => STMT (WritableAckBox a, ReadableAckBox a)
newAckBox = do
        (val_r,val_e) <- mkChannel (id :: Seq (Enabled a) -> Seq (Enabled a))
        (ack_r,ack_e) <- mkChannel (id :: Seq (Enabled ()) -> Seq (Enabled ()))
        return ( WritableAckBox val_r ack_e
               , ReadableAckBox val_e ack_r
               )

-------------------------------------------------------------------------
-- Memory is set up as duel ported to allow a *single* instances of the
-- memory entity/array in Hardware.

data Memory ix a = Memory 
        { writeM  :: REG (ix,a)  -- ^ where you write index-value pairs
        , readM   :: REG ix      -- ^ where you send read requests
        , valueM  :: EXPR a      -- ^ where the read requests appear (same cycle)
        }

memory :: forall a ix . (Rep a, Rep ix, Size ix) => STMT (Memory ix a)
memory = do
        (r1,e1) <- mkChannel (writeMemory :: Seq (Enabled (ix,a)) -> Seq (ix -> a))
        VAR v2 :: VAR ix <- mkTemp
        VAR v3 :: VAR a  <- mkTemp

        always $ v3 := OP2 asyncRead e1 v2

        return $ Memory
          { writeM = r1
          , readM  = v2
          , valueM = v3
          }

--------------------------------------------------------------------------
-- macros

always :: STMT () -> STMT ()
always m = do
        loop <- LABEL
        m ||| GOTO loop

        -- and start it running
        FORK loop

for :: (Rep a, Ord a, Num a) 
    => EXPR a -> EXPR a -> (EXPR a -> STMT ()) -> STMT ()
for start end k = do
        VAR i <- SIGNAL $ undefinedVar
        i := start
        loop <- LABEL
        k i
        i := i + 1 
                ||| (OP2 (.<.) i end :? GOTO loop)

if_then_else :: EXPR Bool -> STMT () -> STMT () -> STMT ()
if_then_else i t e = do 
        rec i :? GOTO t_lab
            e
            GOTO end_lab
            t_lab <- LABEL
            t
            rec end_lab <- LABEL
        return ()

--------------------------------------------------------------------------

mkChannel :: forall a b . (Rep a, Rep b) => (Seq (Enabled a) -> Seq b) -> STMT (REG a, EXPR b)
mkChannel fn = do
        uq <- CHANNEL (return . fn)
        return (R uq,REG (R uq))

mkTemp :: forall a . (Rep a) => STMT (VAR a)
mkTemp = do
        uq <- CHANNEL (\ (a :: Seq (Enabled a)) -> return $ mux (isEnabled a) (undefinedS,enabledVal a))
        return $ VAR $ toVAR $ (R uq, REG (R uq))

mkEnabled :: forall a . (Rep a) => STMT (REG a, EXPR (Maybe a))
mkEnabled = mkChannel id

--  :: (Seq (Maybe a) -> Fabric (Seq b)) -> STMT Int
