{-# LANGUAGE ExistentialQuantification, TypeFamilies,
    ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances,
    FlexibleContexts, UndecidableInstances, GADTs #-}


-- | The Fabric module is used for generating a top-level VHDL entity for a Lava
-- circuit, with inputs and outputs.
module Language.KansasLava.Fabric
        ( Fabric(..)
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
        , fabricAPI
        , traceFabric
        ) where

import Control.Monad.Fix
import Control.Monad hiding (join)
import Data.Sized.Ix
import Data.List as L
import Data.Reify
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Ord(comparing)
import System.IO
import Control.Concurrent

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Universal

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
-- | A Fabric consists of a list of input ports, and yields a list of output
-- ports and generics.
data Fabric a = Fabric { unFabric :: [(String,Pad)] -> (a,[(String,Pad)],[(String,Pad)]) }

instance Functor Fabric where
        fmap f fab = fab >>= \ a -> return (f a)

instance Monad Fabric where
        return a = Fabric $ \ _ -> (a,[],[])
        (Fabric f) >>= k = Fabric $ \ ins -> let
                          (a,in_names,outs) = f ins
                          (r,in_names',outs') = unFabric (k a) ins
                       in (r,in_names ++ in_names',outs ++ outs')

instance MonadFix Fabric where
        mfix f = Fabric $ \ env -> let (a,in_names,outs) = unFabric (f a) env
                                   in (a,in_names,outs)


-- | Generate a named input port.
input :: String -> Pad -> Fabric Pad
input nm deepPad = Fabric $ \ ins ->
        let p = case lookup nm ins of
                   Just v -> v
                   _ -> error $ "input internal error finding : " ++ show nm
        in (p,[(nm,deepPad)],[])

-- | Generate a named output port.
output :: String -> Pad -> Fabric ()
output nm pad = Fabric $ \ _ins -> ((),[],[(nm,pad)])

-- | Generate a named std_logic input port.
inStdLogic :: forall a . (Rep a, W a ~ X1) => String -> Fabric (Seq a)
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

-- | Generate a named generic.
inGeneric :: String -> Fabric Integer
inGeneric nm = do
        pad <- input nm (GenericPad $ error "Fix Generic")
        return $ case pad of
          GenericPad g -> g
          _            -> error "internal type error in inGeneric"

-- | Generate a named std_logic_vector port input.
inStdLogicVector :: forall a . (Rep a, Size (W a)) => String -> Fabric (Seq a)
inStdLogicVector nm = do
	let seq' = mkDeepS $ D $ Pad nm :: Seq (ExternalStdLogicVector (W a))
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
	(Rep a, W a ~ X1) => String -> Seq a -> Fabric ()
outStdLogic nm seq_bool = output nm (StdLogic (bitwise seq_bool))

-- | Generate a named std_logic_vector output port, given a Lava circuit.
outStdLogicVector
  :: forall a .
     (Rep a, Size (W a)) => String -> Seq a -> Fabric ()
outStdLogicVector nm sq =
		  case toStdLogicType (typeOfS sq) of
		    G -> error "outStdLogicVector type mismatch: requiring StdLogicVector, found Generic"
		    _    -> output nm $ StdLogicVector
		    	     	       $ (bitwise sq :: Seq (ExternalStdLogicVector (W a)))

-------------------------------------------------------------------------------

-- | Reify a fabric, returning the output ports and the result of the Fabric monad.
runFabric :: Fabric a -> [(String,Pad)] -> (a,[(String,Pad)])
runFabric (Fabric f) args = (a,result)
        where (a,_arg_types,result) = f args

-- | 'runFabric'  runs a Fabric a with arguments, and gives a value result.
-- must have no (monadic) outputs.
runFabricWithResult :: Fabric a -> [(String,Pad)] -> a
runFabricWithResult (Fabric f) args = a
        where (a,_arg_types,[]) = f args

-- | 'runFabricWithDriver' runs a Fabric () using a driver Fabric.
runFabricWithDriver :: Fabric () -> Fabric a -> a
runFabricWithDriver (Fabric f) (Fabric g) = a
        where ((),_,f_result) = f g_result
              (a,_,g_result)  = g f_result

-- 'fabricAPI' explains what the API is for a specific fabric.
-- The input Pad's are connected to a (deep) Pad nm.
fabricAPI :: Fabric a -> (a,[(String,Pad)],[(String,Pad)])
fabricAPI (Fabric f) = (a,args,result)
        where (a,args,result) = f args
              withType (nm,pad) = (nm,pad)

-- 'traceFabric' returns the actual inputs and outputs, inside the monad.
traceFabric :: Fabric a -> Fabric (a,[(String,Pad)],[(String,Pad)])
traceFabric (Fabric f) = Fabric $ \ ins0 ->
        let (a,tys1,outs1) = f ins0
        in ((a,[],[]),tys1,outs1)

------------------------------------------------------------------------------

-- 'writeFabric' writes the output from a circuit
writeFabric :: String -> Fabric () -> IO ()
writeFabric fileName fab = do
        let h = stdout
--        h <- openFile fileName WriteMode
        hWriteFabric h fab

hWriteFabric :: Handle -> Fabric () -> IO ()
hWriteFabric h (Fabric f) = do
        let (_,_,result) = f []
        -- now turn the list into RepValues
        let txt = id
                $ map (concatMap showPackedRepValue)
                $ L.transpose
                $ map padToRepValues
                $ map snd
                $ result

        let loop [] = error "hWriteFabric output finished (should never happen)"
            loop (t:ts) = do
                    hPutStrLn h t
                    hFlush h
                    loop ts

        forkIO $ loop txt

        return ()



------------------------------------------------------------------------------


-- | 'reifyFabric' does reification of a 'Fabric ()' into a 'KLEG'.
reifyFabric :: Fabric () -> IO KLEG
reifyFabric (Fabric circuit) = do
        -- This is knot-tied with the output from the circuit execution
        let (_,ins0,outs0) = circuit ins0

        let mkU :: forall a . (Rep a) => Seq a -> Type
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
                   (Graph gr out) <- reifyGraph o'
                   let gr' = [ (nid,nd) | (nid,nd) <- gr
                                        , nid /= out
                             ]
                   case lookup out gr of
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
                                              other -> src
                                      ])

          newNames = allocEntities kleg

          newOutputNames = combineNames [ nm | (nm,B,_) <- theSinks kleg ]

          newOutputs = [ (uq,Entity (Prim "concat")
                                    [ ("o0",V (mx + 1)) ]
                                    [ ("i" ++ show n,B,src)
                                    | n <- [0..mx]
                                    , src <- case lookup (nm ++ "<" ++ show n ++ ">")
                                                         [ (nm,src) | (nm,B,src) <- theSinks kleg ] of
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


