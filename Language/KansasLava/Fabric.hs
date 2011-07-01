{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs
 #-}
module Language.KansasLava.Fabric
        ( Fabric(..)
        , Pad(..)
        , runFabric
        , runFabric'                    -- TODO: rename as runFabric
        , inStdLogic
        , inStdLogicVector
        , inGeneric
        , outStdLogic
        , outStdLogicVector
        , padStdLogicType
        , reifyFabric
        , runFabricWithResult
        , runFabricWithDriver
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

import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Comb
import Language.KansasLava.Signal

-- The '_' will disappear soon from these names.

data Pad = StdLogic (Seq Bool)
         | forall a x . (Size (W a), Show a, Rep a)
                => StdLogicVector (Seq a)
--         | TypedPad (...)
         | GenericPad Integer

padStdLogicType :: Pad -> StdLogicType
padStdLogicType (StdLogic _)       = SL
padStdLogicType (StdLogicVector s) = SLV $ size (untype s)
    where untype :: (Size (W a)) => Seq a -> W a
          untype = error "untype"
padStdLogicType (GenericPad _)        = G

instance Show Pad where
        show (StdLogic sq)       = "StdLogic " ++ show sq
        show (StdLogicVector sq) = "StdLogicVector " ++ show sq
        show (GenericPad i)      = "Generic " ++ show i

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



input :: String -> Pad -> Fabric Pad
input nm deepPad = Fabric $ \ ins ->
        let p = case lookup nm ins of
                   Just v -> v
                   _ -> error $ "input internal error finding : " ++ show nm
        in (p,[(nm,deepPad)],[])


output :: String -> Pad -> Fabric ()
output nm pad = Fabric $ \ _ins -> ((),[],[(nm,pad)])

inStdLogic :: String -> Fabric (Seq Bool)
inStdLogic nm = do
        pad <- input nm (StdLogic $ deepSeq $ D $ Pad (OVar 0 nm))
        return $ case pad of
          StdLogic sq -> sq
          _           -> error "internal type error in inStdLogic"

inGeneric :: String -> Fabric Integer
inGeneric nm = do
        pad <- input nm (GenericPad $ error "Fix Generic")
        return $ case pad of
          GenericPad g -> g
          _            -> error "internal type error in inGeneric"

inStdLogicVector :: forall a . (Rep a, Show a, Size (W a)) => String -> Fabric (Seq a)
inStdLogicVector nm = do
	let seq' = deepSeq $ D $ Pad (OVar 0 nm) :: Seq a
        pad <- input nm (StdLogicVector $ seq')
        return $ case pad of
                     -- This unsigned is hack, but the sizes should always match.
          StdLogicVector sq -> case toStdLogicType ty of
					     SLV _ -> unsafeId sq
					     G -> error $ "inStdLogicVector type mismatch: requiring StdLogicVector, found Generic"
					     _     -> liftS1 (\ (Comb a (D ae)) -> Comb (fromRep $ toRep a) 
					     	      	 $ D $ Port ("o0") $ E $ Entity (Prim "coerce")
							   	    	            [("o0",ty)]
									            [("i0",V $ typeWidth ty,ae)]) sq
          _                  -> error "internal type error in inStdLogic"
  where
	ty = repType (Witness :: Witness a)


-------------------------------------------------------------------------------

outStdLogic :: String -> Seq Bool -> Fabric ()
outStdLogic nm seq_bool = output nm (StdLogic seq_bool)

outStdLogicVector
  :: forall a .
     (Rep a, Show a, Size (W a)) => String -> Seq a -> Fabric ()
outStdLogicVector nm sq = 
		  case toStdLogicType (typeOfSeq sq) of
		    SLV _ -> output nm (StdLogicVector sq)
		    G -> error $ "outStdLogicVector type mismatch: requiring StdLogicVector, found Generic"
		    _     -> output nm $ StdLogicVector
		    	     	       $ liftS1 (\ (Comb a (D ae)) -> Comb a
				                      $ D $ Port ("o0") $ E $ Entity (Prim "coerce")
							   	    	            [("o0",V $ typeWidth ty)]
									            [("i0",ty,ae)]) 
				       $ sq					    		
  where
	ty = repType (Witness :: Witness a)

-------------------------------------------------------------------------------

-- 'runFabric'  runs a Fabric () with arguments, and gives a (structured) reply.
runFabric :: Fabric () -> [(String,Pad)] -> [(String,Pad)]
runFabric (Fabric f) args = result
        where ((),_arg_types,result) = f args

runFabric' :: Fabric a -> [(String,Pad)] -> (a,[(String,Pad)])
runFabric' (Fabric f) args = (a,result)
        where (a,_arg_types,result) = f args

-- 'runFabric'  runs a Fabric a with arguments, and gives a value result.
-- must have no (monadic) outputs.
runFabricWithResult :: Fabric a -> [(String,Pad)] -> a
runFabricWithResult (Fabric f) args = a
        where (a,_arg_types,[]) = f args

-- 'runFabricWithDriver' runs a Fabric () using a driver Fabric,
-- returning 
runFabricWithDriver :: Fabric () -> Fabric a -> a
runFabricWithDriver (Fabric f) (Fabric g) = a
        where ((),_,f_result) = f g_result
              (a,_,g_result)  = g f_result

-------------------------------------------------------------------------------

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

        let top_outs = [ (nm, B,    unD $ seqDriver s) | (nm,StdLogic s) <- outs0 ] ++
                       [ (nm, mkU s, unD $ seqDriver s) | (nm,StdLogicVector s) <- outs0 ]

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
                       return (gr',[(OVar 0 nm,ity, driver)
                                       | (nm,ity,driver) <- ins
                                       ])
                     _ -> error $ "reifyFabric: " ++ show o
                v -> fail $ "reifyGraph failed in reifyFabric" ++ show v

        let rCit = KLEG { theCircuit = gr
                        , theSrcs = 
                                [ (OVar 0 "clk",ClkTy)
                                , (OVar 0 "clk_en",B)
                                , (OVar 0 "rst", B)             -- Reset Ty?
                                ] ++
                                [ (OVar 0 nm,fromStdLogicType $ padStdLogicType pad) | (nm,pad) <- ins0 ]
                        , theSinks = outpads
                        }

        -- find the clock domains
        
        let start :: [(EntityClock,Set Unique)]
            start = [( EntityClock $ Pad $ OVar 0 "clk_en"
                     , Set.fromList [ u | (_,_,Port _ u) <- theSinks rCit ]
                     ) ]


        let theCircuitFM = Map.fromList (theCircuit rCit)

        let follow :: EntityClock -> Unique -> [(EntityClock, Driver Unique)]
            follow clk u = case Map.lookup u theCircuitFM of
                        Nothing -> []
                        Just (Entity (Prim "retime") _outs [("i0",_,i0), ("pulse",_,p)]) -> 
                                        [ (EntityClock p,i0)
                                        , (clk,p)
                                        ]
                        Just (Entity _nm _outs ins) -> [ (clk,dr) | (_,_,dr) <- ins ]

        let normalize :: [(EntityClock, Driver Unique)] -> [(EntityClock, Set Unique)]
            normalize = map (\ xss -> (fst (head xss),Set.fromList [ u | (_,Port _ u) <- xss ]))
                      . L.groupBy (\ a b -> fst a == fst b)
                      . L.sortBy (\ a b -> fst a `compare` fst b)


        -- given a working set, find the next working set.
        let step :: [(EntityClock,Set Unique)] -> [(EntityClock,Set Unique)]
            step val = normalize 
                        [ (c,d)
                        | (clk,xs) <- val 
                        , s <- Set.toList xs
                        , (c,d) <- follow clk s
                        ]

        -- given a previous result, and a new result, figure out the new Uniques (the front)
        let front :: [(EntityClock,Set Unique)] -> [(EntityClock,Set Unique)] -> [(EntityClock,Set Unique)]
            front old new = concat
                [ case (lookup clk old, lookup clk new) of
                    (Just o',Just n) -> [(clk,n `Set.difference` o')]
                    (Nothing,Just n) -> [(clk,n)]
                    (Just _,Nothing) -> []
                    _                -> error "internal error"
                | (clk,_) <- new
                ]

        let join :: [(EntityClock,Set Unique)] -> [(EntityClock,Set Unique)] -> [(EntityClock,Set Unique)]
            join old new = 
                [ case (lookup clk old, lookup clk new) of
                    (Just o',Just n) -> (clk,n `Set.union` o')
                    (Nothing,Just n) -> (clk,n)
                    (Just o',Nothing) -> (clk,o')
                    _                -> error "internal error"
                | clk <- Set.toList (Set.fromList (map fst old) `Set.union` Set.fromList (map fst new))
                ]

        let interp :: [(EntityClock,Set Unique)]  -- working set
                   -> [(EntityClock,Set Unique)]  -- new set
                   -> IO [(EntityClock,Set Unique)]  -- result
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


        let uqToClk :: Map Unique (EntityClock)
            uqToClk = Map.fromList
                               [ (uq,clk)
                               | (clk,uqs) <- clocks
                               , uq <- Set.toList uqs
                               ]

        return $ rCit { theCircuit =
                                [  (u,case e of
                                        Entity nm outs ins -> Entity
                                                nm
                                                outs
                                                [ case p of
                                                   ("clk_en",B,ClkDom _) ->
                                                     case Map.lookup u uqToClk of
                                                       Nothing -> error $ "can not find port: " ++ show u
                                                       Just (EntityClock dr) -> ("clk_en",B,dr)
                                                   _ -> p
                                                | p <- ins ]
                                    )
                                | (u,e) <- theCircuit rCit ]
                          } 



-------------------------------------------------------------------------------
-- A clock is represented using its 'clock enable'.
data EntityClock = EntityClock (Driver Unique)
        deriving (Eq,Ord,Show)
