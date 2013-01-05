{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, RecursiveDo, DoRec, TypeFamilies, FlexibleContexts,
  DataKinds, TypeOperators #-}

module Language.KansasLava.Wakarusa
        ( STMT(..)
        , LABEL(..)
        , REG(..)
        , EXPR(..)
        , VAR(..)
        , MEM(..)
        , (|||)
        , compileToFabric
        , ReadAckBox(..)
        , newAckBox
        , connectReadAckBox
        , WriteAckBox(..)
        , connectWriteAckBox
        , takeAckBox
        , readAckBox
        , fullAckBox
        , putAckBox
        , ReadReadyBox(..)
        , newReadyBox
        , connectReadReadyBox
        , WriteReadyBox(..)
        , connectWriteReadyBox
        , takeReadyBox
        , readReadyBox
        , fullReadyBox
        , putReadyBox
        , readEnabled
        , Variable(..)
        , var, undefinedVar
        , memory, Memory(..), readMemory
        , always
        , for
        , mkEnabled
        , mkChannel
        , mkChannel2
        , mkChannel3
        , EVENT(..)
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
import Language.KansasLava.Protocols.Types
import Language.KansasLava.Types
import Language.KansasLava.Universal


import Data.Sized.Sized
import Data.Sized.Matrix as M

import Control.Monad.Fix
import Data.Array.IArray as A
import Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader

import GHC.TypeLits

import Debug.Trace
import System.IO.Unsafe

import qualified Data.Set as Set
import Data.Set (Set)

------------------------------------------------------------------------------

traceRet :: (a -> String) -> String -> a -> a
traceRet showMe msg a = trace (msg ++ " : " ++ showMe a) a


compileToFabric :: STMT a -> Fabric a
compileToFabric prog = do -- traceRet (show . unsafePerformIO . reifyFabric) "compileToFabric" $ do
        let res0 = runStateT $ do
                        compWakarusa prog
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
                    , ws_tidtable = Map.empty
                    , ws_fork = []
                    , ws_pc_locations = Map.empty



                    , ws_pp = []
                    }
        let res2 = runReaderT res1

        rec (r,st) <- res2 $ WakarusaEnv
                    { we_reads     = the_vars
                    , we_pcs       = generatePredicates st -- (ws_labels st) (ws_pcs st) (ws_pc st) (ws_fork st)
                    , we_preds     = ws_preds st
                    , we_mem_reads = placeMemories (ws_mems st) (ws_mem_reads st) (ws_mem_writes st)
                    , we_tid       = Nothing
                    , we_pidtable  = ws_tidtable st
                    , we_pp
--                        = Parsed
                          = Threaded
                    , we_pp_scope  = 0
                    }

            -- connect the inputs and outputs; instantiate the registers
            the_vars <- sequence
                       [ placeRegister reg $ (ws_assigns st)
                       | reg <- ws_regs st
                       ] >>= (return . Map.unions)

{-
        () <- trace ("--parsed debugging") $ return ()
        () <- trace (concat $ reverse $ ws_pp $ st) $ return ()
        () <- trace ("--end of parsed debugging") $ return ()
-}

        return r

{-
generateThreadIds
        :: WakarusaState                -- ^ the (final) state
        -> Map PC Int                   -- Which thread am I powered by
generateThreadIds st =
        trace (show ("generateThreadIds",msg,result)) $
         result
   where
        label_table = ws_labels st
        jumps = ws_pcs st
        pc = ws_pc st
        threads = ws_fork st

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
-}
generatePredicates
        :: WakarusaState                -- ^ the (final) state
{-
        -> Map LABEL PC                         -- ^ label table
        -> [(PC,TID,Pred,LABEL)]                    -- ^ jumps
        -> PC                                   -- ^ last PC number + 1
        -> [(LABEL,Uniq)]                              -- ^ thread starts
-}
        -> Map (PC,TID) (Seq Bool)                    -- ^ table of predicates
                                                --   for each row of instructions
generatePredicates st =
--        trace msg
        result
  where
        msg = show (ws_fork st)
{-
                 [ ((pc,tid),tid_counter .==. pureS pc)
                 | tid <- ws_fork st
                 , let tid_counter = pcs ! tid
                 , pc <- [0.. (ws_pc_locations st ! tid)]
                ]
-}
        result :: Map (PC,TID) (Seq Bool)
        result = Map.fromList
                 [ ((pc,tid),tid_counter .==. pureS pc)
                 | tid <- ws_fork st
                 , let tid_counter = findWithDefault (error "tid_counter not found")
                                                     tid pcs
                 , let mx = findWithDefault (error "mx not found")
                                                     tid (ws_pc_locations st)
                 , pc <- [0.. mx]
                 ]

        pcs :: Map TID (Seq PC)
        pcs = Map.fromList
              [ let pc_reg = probeS ("pc for thread " ++ show (tid :: Int))
                           $ register 0 -- always start at PC = 0
                             $ cASE [ ( pc_pred .&&. fromPred opt_pred (ws_preds st)
                                      , pureS dest_pc
                                      )
                                    | x@(pc_src,tid',opt_pred,dest_label) <- ws_pcs st
                                    , tid == tid'
                                    , let dest_pc = findWithDefault (error $ "dest_pc:"  ++ show (dest_label,ws_labels st))
                                                        dest_label (ws_labels st)
                                    , let pc_pred = findWithDefault (error "pc_pred")
                                                        (pc_src,tid) result
                                    ]
                                    (pc_reg + 1)
                  in (tid,pc_reg)
                | tid <- ws_fork st
                ]

{-
{- trace (show ("generatePredicates",length pcs)) $ -} result
  where
        threadIds = generateThreadIds st

        -- mapping from *every* instruction to its thread id
        result = mapWithKey (\ k tid -> pureS k .==. pcs !! tid) threadIds

        -- a list of PC result for each thread.
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
        prettyPrint (\ _ -> " R" ++ show uq ++ " <- CHANNEL (...)")
        topLevelPrettyPrint "\n"
        return $ uq
compWakarusa (SIGNAL fn) = do
        -- add the register to the table
        uq <- addChannel (return . fn)
        return (VAR $ toVAR $ (R uq, REG (R uq)))
compWakarusa (OUTPUT connect) = do
        uq <- addChannel $ \ wt -> do
                        connect wt
                        return (pureS ())
        prettyPrint (\ _ -> " R" ++ show uq ++ " <- OUTPUT (...)")
        topLevelPrettyPrint "\n"
        return $ R $ uq
compWakarusa (INPUT connect) = do
        uq <- addChannel (const connect :: (a ~ EXPR b) => Seq (Enabled b) -> Fabric (Seq b))
        prettyPrint (\ _ -> " R" ++ show uq ++ " <- INPUT (...)")
        topLevelPrettyPrint "\n"
        return (REG $ R $ uq)
compWakarusa (PATCH p) = do
        (a,b,c,d) <- addPatch p
        return (R a, REG (R b), REG (R c), R d)
compWakarusa (GENERIC f) = do
        (ins,outs) <- addGeneric f
        return (ins,outs)
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
compWakarusa (PAR e1 e2) = do
        prepareInstSlot
        we <- ask
        addPrettyScope 1 $ do
                parInstSlot $ compWakarusa e1
                prettyPrint (\ _ -> "\n    |||")
                parInstSlot $ compWakarusa e2
        topLevelPrettyPrint "\n"
        return ()
compWakarusa NOP = do
        prepareInstSlot
        -- no instruction here
        markInstSlot
        prettyPrint (\ _ -> " NOP")
        return ()
compWakarusa (SPARK code) = do
  resetInstSlot
  -- NOTE: BUG the PC does not get preserved by the SPARK
  newTid $ \ lab -> do
        prettyPrint $ const $ "BEGIN SPARK: " ++ show lab
        topLevelPrettyPrint "\n"
        prettyPrintPC
        prepareInstSlot
        newLabel lab
        prettyPrint (\ _ -> show lab ++ ":")
        topLevelPrettyPrint "\n"
        compWakarusa (code lab)
        st <- get
        case ws_pred st of
          LitPred False -> do
               return ()
          _ -> do
               prettyPrint $ const $ "-- fallthrough possible for SPARK " ++ show lab
               topLevelPrettyPrint "\n"

        prettyPrint $ const $ "END SPARK: " ++ show lab
        topLevelPrettyPrint "\n"
  resetInstSlot

compWakarusa (IF i t e) =
     compWakarusa $ do
        rec i :? GOTO t_lab
            e
            GOTO end_lab
            t_lab <- LABEL
            t
            rec end_lab <- LABEL
        return ()


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
compWakarusaExpr (OPM f es) = do
        cs <- mapM compWakarusaExpr $ A.elems es
        return $ f $ matrix cs

ppEXPR :: Pass -> EXPR a -> String
ppEXPR _ (REG r)   = show r
ppEXPR _ (OP0 lit) = "(OP0 " ++ show lit ++ ")"
ppEXPR p (OP1 _ e1) = "(OP1 (...) " ++ ppEXPR p e1 ++ ")"
ppEXPR p (OP2 _ e1 e2) = "(OP2 (...) " ++ ppEXPR p e1 ++ " " ++ ppEXPR p e2 ++ ")"
ppEXPR p (OP3 _ e1 e2 e3) = "(OP3 (...) " ++ ppEXPR p e1 ++ " " ++ ppEXPR p e2 ++ " " ++ ppEXPR p e3 ++ ")"
ppEXPR p (OPM _ es) = "(OPM (...) " ++ ppEXPRs p (A.elems es) ++ ")"

ppEXPRs :: Pass -> [EXPR a] -> String
ppEXPRs p [] = "[]"
ppEXPRs p (e:es) = "[" ++ ppEXPR p e ++ concatMap pp es ++ "]"
 where
         pp e = "," ++ ppEXPR p e

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


data ReadAckBox a = ReadAckBox (EXPR (Enabled a)) (REG ())

connectReadAckBox
        :: forall a . (Rep a, SingI ((W a) + 1), Show a)
        => String -> String -> STMT (ReadAckBox a)
connectReadAckBox inpName ackName = do
        i :: EXPR (Maybe a)   <- INPUT  (inStdLogicVector inpName)
        o :: REG ()           <- OUTPUT (outStdLogic ackName . isEnabled)
        return $ ReadAckBox i o

takeAckBox :: Rep a => ReadAckBox a -> (EXPR a -> STMT ()) -> STMT ()
takeAckBox (ReadAckBox iA oA) cont = do
        self <- LABEL
        do OP1 (bitNot . isEnabled) iA :? GOTO self
                ||| oA := OP0 (pureS ())
                ||| cont (OP1 enabledVal iA)

-- | A version of takeAckBox that does not acknowleage. Blocks till data is present.

readAckBox :: Rep a => ReadAckBox a -> (EXPR a -> STMT ()) -> STMT ()
readAckBox (ReadAckBox iA oA) cont = readEnabled iA cont

fullAckBox :: (Rep a) => ReadAckBox a -> EXPR Bool
fullAckBox (ReadAckBox iA _) = OP1 isEnabled iA

data WriteAckBox a = WriteAckBox (REG a) (EXPR (Enabled ()))

connectWriteAckBox
        :: forall a . (Rep a, SingI ((W a)), SingI (W(Enabled a)), SingI (W(Enabled ())), W(Enabled ()) ~ 1, Show a)
        => String -> String -> STMT (WriteAckBox a)
connectWriteAckBox outName ackName = do
        iB :: EXPR (Enabled ()) <- INPUT  (inStdLogic ackName)
        oB :: REG a             <- OUTPUT (outStdLogicVector outName)
        return $ WriteAckBox oB iB

putAckBox :: Rep a => WriteAckBox a -> EXPR a -> STMT ()
putAckBox (WriteAckBox oB iB) val = do
        self <- LABEL
        oB := val
                ||| OP1 (bitNot . isEnabled) iB :? GOTO self

newAckBox :: forall a . (Rep a) => STMT (WriteAckBox a, ReadAckBox a)
newAckBox = do
        (val_r,val_e) <- mkChannel (id :: Seq (Enabled a) -> Seq (Enabled a))
        (ack_r,ack_e) <- mkChannel (id :: Seq (Enabled ()) -> Seq (Enabled ()))
        return ( WriteAckBox val_r ack_e
               , ReadAckBox val_e ack_r
               )

-------------------------------------------------------------------------

data ReadReadyBox a = ReadReadyBox (EXPR (Enabled a)) (REG ())

connectReadReadyBox
        :: forall a . (Rep a, SingI (W a), Show a)
        => String -> String -> String -> STMT (ReadReadyBox a)
connectReadReadyBox inpName inpValid ackName = do
        i :: EXPR (Maybe a)   <- INPUT $ do
                                   inp <- inStdLogicVector inpName
                                   val <- inStdLogic inpValid
                                   return $ packEnabled val inp
        o :: REG ()           <- OUTPUT (outStdLogic ackName . isEnabled)
        return $ ReadReadyBox i o

takeReadyBox :: Rep a => ReadReadyBox a -> (EXPR a -> STMT ()) -> STMT ()
takeReadyBox (ReadReadyBox iA oA) cont = do
        self <- LABEL
        do oA := OP0 (pureS ())
                ||| OP1 (bitNot . isEnabled) iA :? GOTO self
                ||| cont (OP1 enabledVal iA)


-- | A version of takeReadyBox that does not acknowleage. Blocks till data is present.
readReadyBox :: Rep a => ReadReadyBox a -> (EXPR a -> STMT ()) -> STMT ()
readReadyBox (ReadReadyBox iA oA) cont = readEnabled iA cont


fullReadyBox :: (Rep a) => ReadReadyBox a -> EXPR Bool
fullReadyBox (ReadReadyBox iA _) = OP1 isEnabled iA

data WriteReadyBox a = WriteReadyBox (REG a) (EXPR (Enabled ()))

connectWriteReadyBox
        :: forall a . (Rep a, SingI (W a), Show a, SingI (W(Enabled ())), W(Enabled ()) ~ 1)
        => String -> String -> String -> STMT (WriteReadyBox a)
connectWriteReadyBox outName outValid ackName = do
        iB :: EXPR (Enabled ()) <- INPUT  ((inStdLogic ackName) :: Fabric (Seq (Enabled ())))
        oB :: REG a             <- OUTPUT $ \ o -> do
                                outStdLogicVector outName (enabledVal o)
                                outStdLogicVector outValid (isEnabled o)
        return $ WriteReadyBox oB iB


putReadyBox :: Rep a => WriteReadyBox a -> EXPR a -> STMT ()
putReadyBox (WriteReadyBox oB iB) val = do
        self <- LABEL
        OP1 (bitNot . isEnabled) iB :? GOTO self
                ||| oB := val

newReadyBox :: forall a . (Rep a) => STMT (WriteReadyBox a, ReadReadyBox a)
newReadyBox = do
        (val_r,val_e) <- mkChannel (id :: Seq (Enabled a) -> Seq (Enabled a))
        (ack_r,ack_e) <- mkChannel (id :: Seq (Enabled ()) -> Seq (Enabled ()))
        return ( WriteReadyBox val_r ack_e
               , ReadReadyBox val_e ack_r
               )
{-

-}
-------------------------------------------------------------------------
-- blocks until the enabled is present

readEnabled :: Rep a => EXPR (Maybe a) -> (EXPR a -> STMT ()) -> STMT ()
readEnabled inp assign = do
        loop <- LABEL
        ((OP1 (bitNot . isEnabled) inp) :? GOTO loop)
                ||| assign (OP1 enabledVal inp)

-------------------------------------------------------------------------
-- Memory is set up as duel ported to allow a *single* instances of the
-- memory entity/array in Hardware.

data Memory ix a = Memory
        { writeM  :: REG (ix,a)  -- ^ where you write index-value pairs
        , readM   :: REG ix      -- ^ where you send read requests
        , valueM  :: EXPR a      -- ^ where the read requests appear (same cycle)
        }

memory :: forall a ix . (Rep a, SingI ix) => STMT (Memory (Sized ix) a)
memory = do
        (r1,e1) <- mkChannel (writeMemory :: Seq (Enabled ((Sized ix),a)) -> Seq ((Sized ix) -> a))
        VAR v2 :: VAR (Sized ix) <- mkTemp
        VAR v3 :: VAR a  <- mkTemp

        always $ v3 := OP2 asyncRead e1 v2

        return $ Memory
          { writeM = r1
          , readM  = v2
          , valueM = v3
          }

readMemory :: (Rep a, Rep ix) => Memory ix a -> EXPR ix -> (EXPR a -> STMT ()) -> STMT ()
readMemory mem ix cont = readM mem := ix ||| cont (valueM mem)

--------------------------------------------------------------------------
-- macros

always :: STMT () -> STMT ()
always m = do
        SPARK $ \ loop -> do
                m ||| GOTO loop

--        -- and start it running
--        FORK loop

for :: (Rep a, Ord a, Num a)
    => EXPR a -> EXPR a -> (EXPR a -> STMT ()) -> STMT ()
for start end k = do
    rec VAR i <- SIGNAL $ undefinedVar
        i := start
        loop <- LABEL
        k i
        (OP2 (.==.) i end) :? GOTO done
        i := i + 1
        GOTO loop
        done <- LABEL
    return ()

--------------------------------------------------------------------------

mkChannel :: forall a b . (Rep a, Rep b) => (Seq (Enabled a) -> Seq b) -> STMT (REG a, EXPR b)
mkChannel fn = do
        uq <- CHANNEL (return . fn)
        return (R uq,REG (R uq))

-- We should be able to use overloading here.
mkChannel2 :: forall a b c . (Rep a, SingI (W (Enabled a)), Rep b, SingI (W (Enabled b)), Rep c, SingI (W c))
           => (Seq (Enabled a) -> Seq (Enabled b) -> Seq c) -> STMT (REG a, REG b, EXPR c)
mkChannel2 fn = do
        ([a,b],[r]) <- GENERIC $ do
                        i0 <- inStdLogicVector "i0"
                        i1 <- inStdLogicVector "i1"
                        outStdLogicVector "o0" (fn i0 i1)
        return $ (R a,R b,REG (R r))

mkChannel3 :: forall a b c . (Rep a, SingI (W (Enabled a)), Rep b, SingI (W b))
           => (Seq (Enabled a) -> Seq b) -> STMT (REG a, EXPR b)
mkChannel3 fn = do
        ([a],[r]) <- GENERIC $ do
                        i0 <- inStdLogicVector "i0"
                        outStdLogicVector "o0" (fn i0)
        return $ (R a,REG (R r))

mkTemp :: forall a . (Rep a) => STMT (VAR a)
mkTemp = do
        uq <- CHANNEL (\ (a :: Seq (Enabled a)) -> return $ mux (isEnabled a) (undefinedS,enabledVal a))
        return $ VAR $ toVAR $ (R uq, REG (R uq))

-- TODO: call this mkEVENT, and use type EVENT a = EXPR (Maybe a)

mkEnabled :: forall a . (Rep a) => STMT (REG a, EXPR (Maybe a))
mkEnabled = mkChannel id

--  :: (Seq (Maybe a) -> Fabric (Seq b)) -> STMT Int

--------------------------------------------------------------------------------------

-- type EVENT a = EXPR (Maybe a)
