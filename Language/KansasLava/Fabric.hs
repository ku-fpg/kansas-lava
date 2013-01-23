{-# LANGUAGE ExistentialQuantification, TypeFamilies,
    ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances,
    FlexibleContexts, UndecidableInstances, GADTs, RecursiveDo, RankNTypes, DataKinds, InstanceSigs, MultiParamTypeClasses  #-}


-- | The Fabric module is used for generating a top-level VHDL entity for a Lava
-- circuit, with inputs and outputs.
module Language.KansasLava.Fabric
{-         ( Fabric(..)
        , Pad(..)
        , runFabric
        , inStdLogic
        , inStdLogicVector
        , inGeneric
        , outStdLogic
        , outStdLogicVector
        , padStdLogicType
        , theClk
        , theRst
        , theClkEn
        , reifyFabric
        , runFabricWithResult
        , runFabricWithDriver
        , writeFabric
        , hWriteFabric
        , readFabric
        , hReadFabric
        , fabricAPI
        , traceFabric
        , ioFabric
        , Reify(..)
        , observeFabric
        ) where -} where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Monoid
import Data.List as L
import Data.Reify
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Ord(comparing)
import System.IO
import Control.Concurrent

import GHC.TypeLits

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Universal
import Language.KansasLava.Probes

import Debug.Trace


         -- TODO: the 2D Array

{- | The 'Fabric' structure, which is also a monad.

> fabric_example :: Fabric ()
> fabric_example = do
>        i0 <- inStdLogic "i0"
>        i1 <- inStdLogic "i1"
>        let (c,s) = halfAdder i0 i1
>        outStdLogic "carry" c
>        outStdLogic "sum" s
>  where
>          halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool,Seq Bool)
>          halfAdder a b = (carry,sum_)
>                where carry = and2 a b
>                      sum_  = xor2 a b

-}

data FabricInput c = FabricInput
        { in_inPorts :: [(String,Pad c)]
        , in_vars :: [(Int,Pad c)]                -- can be many Int -> Pad
        }


initFabricInput :: FabricInput c
initFabricInput = FabricInput [] []

data FabricState = FabricState
        { st_uniq :: Int                -- a unique number for gensym
        }

initFabricState :: FabricState
initFabricState = FabricState 0 -- Hmm

data FabricOutput c = FabricOutput
        { out_inPorts :: [(String,Pad c)]
        , out_outPorts :: [(String,Pad c)]
        , out_vars     :: [(Int,Pad c)]
        }

instance Monoid (FabricOutput c) where
        mempty = FabricOutput [] [] []
        mappend (FabricOutput i1 o1 v1) (FabricOutput i2 o2 v2) =
                FabricOutput (i1 <> i2) (o1 <> o2) (v1 <> v2)

data SuperFabric c m a = Fabric
        { unFabric :: FabricInput c -> FabricState -> m (a,FabricOutput c,FabricState) }

-- | A Fabric consists of a list of input ports, and yields a list of output
-- ports and generics.
type Fabric = SuperFabric CLK Pure -- Fabric { unFabric :: [(String,Pad)] -> (a,[(String,Pad)],[(String,Pad)]) }

class (MonadFix f, Functor f) => Reify f where
    purify :: f a -> Pure a

class (MonadFix m, LocalM m) => InOutM m where
        input :: (c ~ LocalClock m) => String -> Pad c -> m (Pad c)
        output :: (c ~ LocalClock m) => String -> Pad c -> m ()

instance Reify Pure where
    purify m = m

data Pure a = Pure { runPure :: a }

instance Functor Pure where
        fmap f (Pure a) = Pure (f a)

instance Monad Pure where
        return a = Pure a
        (Pure a) >>= k = k a

instance MonadFix Pure where
        mfix f = Pure $ let Pure x = f x in x

instance MonadFix m => Functor (SuperFabric c m) where
        fmap f fab = fab >>= \ a -> return (f a)

instance MonadFix m => Monad (SuperFabric c m) where
        return a = Fabric $ \ _ st -> return (a,mempty,st)
        (Fabric f) >>= k = Fabric $ \ ins st0 -> do
                          (a,outs1,st1) <- f ins st0
                          (r,outs2,st2) <- unFabric (k a) ins st1
                          return (r,outs1 <> outs2,st2)

instance MonadFix m => MonadFix (SuperFabric c m) where
        mfix f = Fabric $ \ env st0 -> do rec (a,outs,st1) <- unFabric (f a) env st0
                                          return (a,outs,st1)

instance MonadIO (SuperFabric c IO) where
        liftIO = lift

instance MonadTrans (SuperFabric c) where
        lift m = Fabric $ \ _ st -> do
                r <- m
                return (r,mempty,st)

-- TODO: use liftFabric
liftFabric :: (Reify f, Monad m) => SuperFabric c f a -> SuperFabric c m a
liftFabric (Fabric f) = Fabric $ \ inp st -> do
        let Pure x = purify $ f inp st
        return x

-- TODO: restore if/when needed
--liftFabric :: (forall a . m a -> n a) -> SuperFabric c m a -> SuperFabric c n a
--liftFabric g (Fabric f) = Fabric $ \ inp st -> do
--        g (f inp st)



instance (Clock c, MonadFix m) => InOutM (SuperFabric c m) where
  input nm deepPad = Fabric $ \ ins st -> do
        let p = case lookup nm (in_inPorts ins) of
                   Just v -> v
                   _ -> error $ "input internal error finding : " ++ show nm
        return (p,mempty { out_inPorts = [(nm,deepPad)] },st)

  output nm pad = Fabric $ \ _ins st -> return ((),mempty { out_outPorts = [(nm,pad)] },st)


-- | Generate a named std_logic input port.
inStdLogic :: forall a m . (InOutM m, Rep a, W a ~ 1) => String -> m (Signal (LocalClock m) a)
inStdLogic nm = do
        pad <- input nm (StdLogic $ mkDeepS $ D $ Pad nm)
        return $ case pad of
          StdLogic sq -> bitwise sq
          StdLogicVector sq -> case toStdLogicType ty of
					     SL -> unsafeId sq
                                             _  -> error "internal type error in inStdLogic (not SL)"
          _           -> error "internal type error in inStdLogic"
   where
	ty = repType (Witness :: Witness a)

{-
-- | Generate a named generic.
inGeneric :: String -> Fabric Integer
inGeneric nm = do
        pad <- input nm (GenericPad $ error "Fix Generic")
        return $ case pad of
          GenericPad g -> g
          _            -> error "internal type error in inGeneric"
-}

-- | Generate a named std_logic_vector port input.
inStdLogicVector :: forall a m . (InOutM m, Rep a, SingI (W a)) => String -> m (Signal (LocalClock m) a)
inStdLogicVector nm = do
	let seq' = mkDeepS $ D $ Pad nm :: Signal (LocalClock m) (ExternalStdLogicVector (W a))
        pad <- input nm (StdLogicVector seq')
        return $ case pad of
                     -- This unsigned is hack, but the sizes should always match.
          StdLogicVector sq -> case toStdLogicType ty of
					     SLV _ -> unsafeId sq
					     G     -> error "inStdLogicVector type mismatch: requiring StdLogicVector, found Generic"
					     SL    -> unsafeId sq
					     SLVA _ _ -> unsafeId sq
          _                  -> error "internal type error in inStdLogic"
  where
	ty = repType (Witness :: Witness a)

-- | theClk gives the external name for the clock.
theClk   :: String -> Fabric ()
theClk nm  = input nm TheClk >> return ()

-- | theRst gives the external name for the reset signal [default = low].
theRst   :: String -> Fabric ()
theRst nm = input nm TheRst >> return ()

-- | theClkEn gives the external name for the clock enable signal [default = high].
theClkEn :: String -> Fabric ()
theClkEn nm = input nm TheClkEn >> return ()

-------------------------------------------------------------------------------

-- | Generate a named std_logic output port, given a Lava circuit.
outStdLogic ::
	(InOutM m, Rep a, W a ~ 1) => String -> Signal (LocalClock m) a -> m ()
outStdLogic nm seq_bool = output nm (StdLogic (bitwise seq_bool))

-- | Generate a named std_logic_vector output port, given a Lava circuit.
outStdLogicVector
  :: forall a m .
     (InOutM m, Rep a, SingI (W a)) => String -> Signal (LocalClock m) a -> m ()
outStdLogicVector nm sq =
		  case toStdLogicType (typeOfS sq) of
		    G -> error "outStdLogicVector type mismatch: requiring StdLogicVector, found Generic"
		    _    -> output nm $ StdLogicVector
		    	     	       $ (bitwise sq :: Signal (LocalClock m) (ExternalStdLogicVector (W a)))


-------------------------------------------------------------------------------

-- | Reify a fabric, returning the output ports and the result of the Fabric monad.
runFabric :: (MonadFix m) => SuperFabric c m a -> [(String,Pad c)] -> m (a,[(String,Pad c)])
runFabric (Fabric f) args = do
        rec (a,out,_) <- f (initFabricInput { in_inPorts = args,  in_vars = out_vars out }) (initFabricState)
        return (a,out_outPorts out)

-- | 'runFabric'  runs a Fabric a with arguments, and gives a value result.
-- must have no (monadic) outputs. (TODO: err, this should be checked)
runFabricWithResult :: (MonadFix m) => SuperFabric c m a -> [(String,Pad c)] -> m a
runFabricWithResult (Fabric f) args = do
        (a,_,_) <- f (initFabricInput { in_inPorts = args}) (initFabricState)
        return a


-- | 'runFabricWithDriver' runs a Fabric a using a driver Fabric b.
runFabricWithDriver :: (MonadFix m) => SuperFabric c m a -> SuperFabric c m b -> m (a,b)
runFabricWithDriver (Fabric f) (Fabric g) = do
        rec (a,f_result,st1) <- f (initFabricInput { in_inPorts = out_outPorts g_result, in_vars = out_vars f_result }) (initFabricState)
            (b,g_result,_st2)  <- g (initFabricInput { in_inPorts = out_outPorts f_result, in_vars = out_vars g_result }) (st1)
        return (a,b)

recordFabric :: (MonadFix m) => SuperFabric c m a -> SuperFabric c m (a,[(String,Pad c)],[(Int,Pad c)],[(String,Pad c)])
recordFabric (Fabric f) = Fabric $ \ inps st0 -> do
        rec (a,f_result,st1) <- f inps st0
        return ((a,in_inPorts inps,out_vars f_result,out_outPorts f_result),f_result,st1)

-- TODO: only used in Wakarusa Monad?
-- 'fabricAPI' explains what the API is for a specific fabric.
-- The input Pad's are connected to a (deep) Pad nm.
fabricAPI :: (MonadFix m) => SuperFabric c m a -> m (a,[(String,Pad c)],[(String,Pad c)])
fabricAPI (Fabric f) = do
        rec (a,result,_) <- f (initFabricInput { in_inPorts = out_inPorts result }) (initFabricState)
        return (a,out_inPorts result,out_outPorts result)

-- 'traceFabric' returns the actual inputs and outputs, inside the monad.
-- TODO: This is broken!!
traceFabric :: Fabric a -> Fabric (a,[(String,Pad c)],[(String,Pad c)])
traceFabric (Fabric f) = error "TODO!"
--Fabric $ \ (FabricInput ins0) st0 -> do
--        (a,FabricOutput tys1 outs1,st1) <- f (FabricInput ins0) st0
--        return ((a,[],[]),FabricOutput tys1 outs1,st1)

data IN m = forall a. (Rep a) => IN (m (Seq a))

hWriterFabric :: (MonadIO m) => Handle -> [IN m] -> m ()
hWriterFabric h table = do
        xs <- sequence
                [ do sq <- f
                     return $ S.toList $ fmap toRep $ shallowS sq
                | IN f <- table
                ]

        let loop _n [] = error "hWriteFabric output finished (should never happen)"
            loop n (t:ts) = do
                    hPutStrLn h t
                    hFlush h
--                    print (n,t)
                    loop (n+1) ts

        _ <- ($) liftIO $ forkIO $ loop 0 $  map (concatMap showPackedRepValue) $ transpose xs

        return ()

consume :: (Monad m) => [IN m] -> m (Seq ())
consume table = do
        xs <- sequence
                [ do sq <- f
                     return $ S.toList $ fmap (showPackedRepValue . toRep) $ shallowS sq
                | IN f <- table
                ]

        let unit [] = ()
            unit (x:xs') = x `seq` unit xs'

        return $ toS $ fmap unit $ fmap concat $ transpose xs



data OUT m = forall a . (Rep a) => OUT (Seq a -> m ())

hReaderFabric :: (MonadIO m) => Handle -> [OUT m] -> m ()
hReaderFabric h table = do
        str <- liftIO $ hGetContents h
        let strs :: [String] = lines str

        _ <- foldl (>>=) (return 0)
                [ \ p -> do -- infinite stream of inputs, space leak generator
                            -- NOTE: External is MSF, internally (RepValue) is LSF.
                     let xs = [ case readPackedRepValue $ take w $ drop p $ s of
                                     Nothing -> error "bad input value for hReadFabric"
                                     Just v  -> v
                                  | s <- strs ]
                         a = mkShallowS $ fmap fromRep $ S.fromList $ xs
                         w = widthS a
                     f a        -- This is actual connection to the fabric
                     return (p + w)
                | (OUT f) <- table
                ]

        return ()

-- This gives a version of fabric where a given 'observe' is applied to all inputs and outputs.
-- TODO: call this probeFabric (loops in modules stops this right now)
observeFabric :: (MonadFix f) => SuperFabric c f a -> SuperFabric c f a
observeFabric (Fabric f) = Fabric $ \ inps st0 -> do
        rec (a,outs,st1) <- f (inps { in_inPorts = map (obs "in") (in_inPorts inps)}) st0
        return (a,outs { out_outPorts = map (obs "out") (out_outPorts outs)},st1)
  where
        obs :: String -> (String,Pad c) -> (String,Pad c)
        obs io (nm,pad) = (nm,rank2MapPad (probeS (io ++ "/" ++ nm)) pad)


-------------------------------------------------------------------------------


data SignalVar clk a = SignalVar Int
        deriving Show

-- What do we call SparkM?
-- TODO: BlockM?
class (Clock (LocalClock m), Monad m) => LocalM m where
        type LocalClock m :: *
        newSignalVar   :: (clk ~ LocalClock m) => m (SignalVar clk a)
        writeSignalVar :: (clk ~ LocalClock m, Rep a, SingI (W a))
                       => SignalVar clk a -> Signal clk a -> m ()
        readSignalVar  :: (clk ~ LocalClock m, Rep a, SingI (W a))
                       => SignalVar clk a
                       -> ([Signal clk a] -> Signal clk b)
                       -> m (Signal clk b)

instance forall c m . (Clock c, MonadFix m) => LocalM (SuperFabric c m) where
        type LocalClock (SuperFabric c m) = c
        newSignalVar = Fabric $ \ _ st -> return (SignalVar $ st_uniq st, mempty,st { st_uniq = st_uniq st + 1 })
--        writeSignalVar = write

        writeSignalVar :: forall clk a
                        . (clk ~ LocalClock (SuperFabric c m), Rep a, SingI (W a))
                       => SignalVar clk a
                       -> Signal clk a
                       -> SuperFabric c m ()
        writeSignalVar (SignalVar uq) sig = Fabric $ \ _inps st -> return ((),mempty { out_vars = [(uq,pad)] },st)
                where pad = StdLogicVector $ (bitwise sig :: Signal c (ExternalStdLogicVector (W a)))


        readSignalVar :: forall clk a b
                       . (clk ~ LocalClock (SuperFabric c m), Rep a, SingI (W a))
                      => SignalVar clk a
                      -> ([Signal clk a] -> Signal clk b)
                      -> SuperFabric c m (Signal clk b)
        readSignalVar (SignalVar uq) f = Fabric $ \ inps st ->
                return (f
                        $ [ unsafeId s
                        | (uq',StdLogicVector s) <- in_vars inps
                        , uq' == uq
                        ], mempty,st)


-------------------------------------------------------------------------------

-- | 'reifyFabric' does reification of a 'Fabric ()' into a 'KLEG'.
reifyFabric :: forall c m . (Reify m) => SuperFabric c m () -> IO KLEG
reifyFabric (Fabric circuit) = do
        -- This is knot-tied with the output from the circuit execution
        let Pure (_,out,_) = purify
                           $ circuit (initFabricInput { in_inPorts = ins0, in_vars = vars0 })
                                     initFabricState
            ins0 =  out_inPorts out
            outs0 = out_outPorts out
            vars0 = out_vars out

        let mkU :: forall a . (Rep a) => Signal c a -> Type
            mkU _ = case toStdLogicType ty of
		      G      -> error $ "reifyFabric, outputing a non stdlogic[vector]: " ++ show ty
	    	      SLV {} -> ty
		      _      -> V $ typeWidth ty
	       where
	       	     ty = repType (Witness :: Witness a)

        let top_outs = [ (nm, B,    unD $ deepS s) | (nm,StdLogic s) <- outs0 ] ++
                       [ (nm, mkU s, unD $ deepS s) | (nm,StdLogicVector s) <- outs0 ]

        let o = Port "top"
                $ E
                $ Entity (Prim "top") []
                top_outs

        -- Get the graph, and associate the output drivers for the graph with
        -- output pad names.
        (gr, outpads) <- case o of
                Port _ o' -> do
                   (Graph gr gr_out) <- reifyGraph o'
                   let gr' = [ (nid,nd) | (nid,nd) <- gr
                                        , nid /= gr_out
                             ]
                   case lookup gr_out gr of
                     Just (Entity (Prim "top")  _ ins) ->
                       return (gr',[(nm,ity, driver)
                                       | (nm,ity,driver) <- ins
                                       ])
                     _ -> error $ "reifyFabric: " ++ show o
                v -> fail $ "reifyGraph failed in reifyFabric" ++ show v

	let ins0' = clk' ++ ins0

    	    -- only clock has a default always connecteda; this may check for clk somewhere else.
	    clk'    = if null [ () | (_,TheClk) <- ins0 ] then [("clk",TheClk)] else []

	    clk_name    = head $ [ Pad nm | (nm,TheClk) <- ins0' ]   ++ error "bad clk_name"
	    rst_name    = head $ [ Pad nm | (nm,TheRst) <- ins0' ]   ++ [Lit (RepValue [Just False])]
	    clk_en_name = head $ [ Pad nm | (nm,TheClkEn) <- ins0' ] ++ [Lit (RepValue [Just True])]

	    gr1 = map replaceEnv gr

	    replaceEnv (u,Entity name outs ins) = (u,Entity name outs
						    [ (s,t,case d of
							Pad "clk" -> clk_name
							Pad "rst" -> rst_name
							Pad "clk_en" -> clk_en_name
							other -> other)
						    | (s,t,d) <- ins
						    ])


        let rCit = KLEG { theCircuit = gr1
                        , theSrcs = [ (nm,fromStdLogicType $ padStdLogicType pad) | (nm,pad) <- ins0' ]
                        , theSinks = outpads
                        }

        -- find the clock domains

        let start :: [(EntityClock,Set (Driver Unique))]
            start = [( EntityClock $ clk_en_name
                     , Set.fromList [ p | (_,_,p) <- theSinks rCit ]
                     ) ]

        let theCircuitFM = Map.fromList (theCircuit rCit)

        let follow :: EntityClock -> String -> Unique -> [(EntityClock, Driver Unique)]
            follow clk nm u = case Map.lookup u theCircuitFM of
                        Nothing -> []
                        Just (Entity (External "upflux") _outs [("i0",_,i0), ("go",_,p)]) ->
                                        [ (EntityClock $ Port "o_clk_en" u,i0)
                                        , (clk,p)
                                        ]
			Just (Entity (External "downflux") _ _)
				| nm == "o0" -> -- do not follow into downflux
                                        [ ]

			Just (Entity _nm _outs ins) -> [ (clk,dr) | (_,_,dr) <- ins ]

        let normalize :: [(EntityClock, Driver Unique)] -> [(EntityClock, Set (Driver Unique))]
            normalize = map (\ xss -> (fst (head xss),Set.fromList [ p | (_,p) <- xss ]))
                      . L.groupBy (\ a b -> fst a == fst b)
                      . L.sortBy (comparing fst)


        -- given a working set, find the next working set.
        let step :: [(EntityClock,Set (Driver Unique))] -> [(EntityClock,Set (Driver Unique))]
            step val = normalize
                        [ (c,d)
                        | (clk,xs) <- val
                        , Port n s <- Set.toList xs
                        , (c,d) <- follow clk n s
                        ]


        -- given a previous result, and a new result, figure out the new Uniques (the front)
        let front :: [(EntityClock,Set (Driver Unique))] -> [(EntityClock,Set (Driver Unique))] -> [(EntityClock,Set (Driver Unique))]
            front old new = concat
                [ case (lookup clk old, lookup clk new) of
                    (Just o',Just n) -> [(clk,n `Set.difference` o')]
                    (Nothing,Just n) -> [(clk,n)]
                    (Just _,Nothing) -> []
                    _                -> error "internal error"
                | (clk,_) <- new
                ]

        let join :: [(EntityClock,Set (Driver Unique))] -> [(EntityClock,Set (Driver Unique))] -> [(EntityClock,Set (Driver Unique))]
            join old new =
                [ case (lookup clk old, lookup clk new) of
                    (Just o',Just n)  -> (clk,n `Set.union` o')
                    (Nothing,Just n)  -> (clk,n)
                    (Just o',Nothing) -> (clk,o')
                    _                 -> error "internal error"
                | clk <- Set.toList (Set.fromList (map fst old) `Set.union` Set.fromList (map fst new))
                ]

        let interp :: [(EntityClock,Set (Driver Unique))]  -- working set
                   -> [(EntityClock,Set (Driver Unique))]  -- new set
                   -> IO [(EntityClock,Set (Driver Unique))]  -- result
            interp working [] = return working
            interp working new = do
--                print ("working",working)
--                print ("new",new)
                let new' = step new
--                print ("new'",new')
                let working' = join working new'
--                print ("working'",working')
                let new'' = front working new'
--                print ("new''",new'')
                interp working' new''

        clocks <- interp start start

--	let clocks = undefined


        let uqToClk :: Map (Driver Unique) [EntityClock]
            uqToClk = Map.fromListWith (++)
                               [ (port,[clk])
                               | (clk,uqs) <- clocks
                               , port <- Set.toList uqs
                               ]

--	print uqToClk

        let final_cir
              = rCit { theCircuit =
                       [  (u,case e of
                              Entity nm outs ins ->
			 	case clkEnPort nm of
				   Nothing -> e
			           Just port_nm ->
					  let (_,p) = entityFind port_nm e
					  in Entity nm outs $
					      ins ++
					      [ case Map.lookup p uqToClk of
                                                       Nothing -> error $ "can not find port: " ++ show p
                                                       Just [EntityClock dr] -> ("clk_en",B,dr)
						       Just xs -> error $ "node " ++ show u ++
								   " has multiple clocks domains " ++
								   show xs
					      ]

                                    )
                                | (u,e) <- theCircuit rCit ]
                          }
        return $ id
               $ joinStdLogicVector
               $ final_cir



-------------------------------------------------------------------------------------------
{-
entity main is                                          entity main is
  port(clk : in std_logic;                                port(clk : in std_logic;
       ROT_B : in std_logic;                                   ROT_B : in std_logic;
       ROT_A : in std_logic;                                   ROT_A : in std_logic;
       LED<7> : out std_logic;          ===>                   LED : out std_logic_vector(7 downto 0);
       LED<6> : out std_logic;                            end entity main;
       LED<5> : out std_logic;
       LED<4> : out std_logic;
       LED<3> : out std_logic;
       LED<2> : out std_logic;
       LED<1> : out std_logic;
       LED<0> : out std_logic);
end entity main;
-}

joinStdLogicVector :: KLEG -> KLEG
joinStdLogicVector kleg =
                  trace (show ("newOutputNames",newOutputNames))
                $ kleg { theCircuit = fmap fixSrcs (theCircuit kleg) ++ newInputs ++ newOutputs
                       , theSinks   = [ (nm,ty,src)
                                      | (nm,ty,src) <- theSinks kleg
                                      , not ('>' `elem` nm)     -- remove the partuals
                                      ]  ++
                                      [ (nm,V (mx + 1),Port "o0" uq)
                                      | ((nm,mx),(uq,_)) <- newOutputNames `zip` newOutputs
                                      ]
                       , theSrcs    = [ (nm,ty)
                                      | (nm,ty) <- theSrcs kleg
                                      , not ('>' `elem` nm)     -- remove the partuals
                                      ]  ++
                                      [ (nm,V (mx + 1))
                                      | (nm,mx) <- newInputNames
                                      ]

                       }
  where
          fixSrcs (uq,Entity nm outs ins) =
                  (uq, Entity nm outs [ (nm0,ty,src')
                                      | (nm0,ty,src) <- ins
                                      , let src'= case src of
                                              Pad nm1 -> case lookup nm1 oldInputs of
                                                        Just port -> port
                                                        Nothing   -> src
                                              _other -> src
                                      ])

          newNames = allocEntities kleg

          newOutputNames = combineNames [ nm | (nm,B,_) <- theSinks kleg ]

          newOutputs = [ (uq,Entity (Prim "concat")
                                    [ ("o0",V (mx + 1)) ]
                                    [ ("i" ++ show n,B,src)
                                    | n <- [0..mx]
                                    , src <- case lookup (nm ++ "<" ++ show n ++ ">")
                                                         [ (nm',src) | (nm',B,src) <- theSinks kleg ] of
                                          Nothing  -> return $ Lit $ RepValue [return False]
                                          Just src -> return src

                                    ])
                       | (uq,(nm,mx)) <- take (length newOutputNames) newNames `zip` newOutputNames
                       ]

          newInputNames = combineNames [ nm | (nm,B) <- theSrcs kleg ]

          newInputs = [ (uq,Entity (Prim "unconcat")
                                    [ ("o" ++ show n,B) | n <- [0..mx]]
                                    [ ("i0",V (mx+1),src)])
                       | (uq,(nm,mx)) <- drop (length newOutputNames) newNames `zip` newInputNames
                       , let src = Pad nm
                       ]

          oldInputs = [ (nm ++ "<" ++ show n ++ ">",Port ("o" ++ show n) uq)
                      | ((uq,_),(nm,mx))  <- newInputs `zip` newInputNames
                      , n <- [0..mx]
                      ]


          combineNames names = id
                    $ fmap last
                    $ groupBy (\ (nm1,_) (nm2,_) -> nm1 == nm2)
                    $ sort
                    $ [(nm,read n :: Int)
                      | s0 <- names
                      , (nm,n) <- take 1
                        [ (nm,n)
                        | (nm,s1)  <- lex s0
                        , ("<",s2) <- lex s1
                        , (n,s3)   <- lex s2
                        , (">",[]) <- lex s3
                        ]
                       ]

{-
ex1 = do
        outStdLogic "bbd<0>" high
        outStdLogic "abd<22>" low
        outStdLogic "bbd<2>" high


ex2 = do
        a <- inStdLogic "bbd<0>"  :: Fabric (Seq Bool)
        b <- inStdLogic "abd<4>":: Fabric (Seq Bool)
        c <- inStdLogic "bbd<2>" :: Fabric (Seq Bool)
        outStdLogic "foo" (a `and2` b)
        return ()

t ex =reifyFabric ex >>= return . joinStdLogicVector
-}
-------------------------------------------------------------------------------------------

-- Each one needs a i0 to look at

-- | Return the name of the clock-enable port, given an Id.
clkEnPort :: Id -> Maybe String
clkEnPort (Prim "register")     = return "i0"
clkEnPort (Prim "delay")        = return "i0"
clkEnPort (Prim "write")	= return "wData"
clkEnPort (External "upflux")   = return "go"
clkEnPort (External "downflux") = return "i0"
clkEnPort _ = Nothing


-------------------------------------------------------------------------------
-- | A clock is represented using its 'clock enable'.
data EntityClock = EntityClock (Driver Unique)
        deriving (Eq,Ord,Show)

---------------------------------------------------------------------------------
