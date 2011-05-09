{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs
 #-}
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
        , reifyFabric
        ) where

import Control.Monad.Fix
import Control.Monad
import Data.Sized.Ix
import Data.List as L
import Data.Maybe(fromMaybe)
import Data.Reify


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
        case pad of
          StdLogic sq -> return sq
          _            -> fail "internal type error in inStdLogic"

inGeneric :: String -> Fabric Integer
inGeneric nm = do
        pad <- input nm (GenericPad $ error "Fix Generic")
        case pad of
          GenericPad g -> return g
          _            -> fail "internal type error in inGeneric"

inStdLogicVector :: forall a . (Rep a, Show a, Size (W a)) => String -> Fabric (Seq a)
inStdLogicVector nm = do
	let seq' = deepSeq $ D $ Pad (OVar 0 nm) :: Seq a
        pad <- input nm (StdLogicVector $ seq')
        case pad of
                     -- This unsigned is hack, but the sizes should always match.
          StdLogicVector sq -> return $ case toStdLogicType ty of
					     SLV _ -> unsafeId sq
					     G -> error $ "inStdLogicVector type mismatch: requiring StdLogicVector, found Generic"
					     _     -> liftS1 (\ (Comb a (D ae)) -> Comb (fromRep $ toRep a) 
					     	      	 $ D $ Port ("o0") $ E $ Entity (Prim "coerce")
							   	    	            [("o0",ty)]
									            [("i0",V $ typeWidth ty,ae)]) sq
          _                  -> fail "internal type error in inStdLogic"
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
                        , theSrcs = [ (OVar 0 nm,fromStdLogicType $ padStdLogicType pad) | (nm,pad) <- ins0 ]
                        , theSinks = outpads
                        }


        let domains = nub $ concat $ visitEntities rCit $ \ _ (Entity _ _ outs) ->
                return [ nm | (_,ClkDomTy,ClkDom nm) <- outs ]

        let envIns = [("clk_en",B),("clk",ClkTy),("rst",B)]     -- in reverse order for a reason

        let domToPorts =
                [ (dom, [ (nm,ty,Pad (OVar idx nm))
                       | ((nm,ty),idx) <- zip envIns [i*3-1,i*3-2,i*3-3]
                       ])
                | (dom,i) <- zip domains [0,-1..]
                ]
        return $ rCit { theCircuit =
                                [  (u,case e of
                                        Entity nm outs ins -> Entity
                                                nm
                                                outs
                                                (concat
                                                [ case p of
                                                   (_,ClkDomTy,ClkDom cdnm) ->
                                                     fromMaybe (error $ "can not find port: " ++ show cdnm)
                                                               (lookup cdnm domToPorts)
                                                   _ -> [p]
                                                | p <- ins ])
                                    )
                                | (u,e) <- theCircuit rCit ]
                          , theSrcs =
                                [ (ovar,ty) | (_,outs) <- domToPorts
                                            , (_,ty,Pad ovar) <- outs
                                ] ++
                                theSrcs rCit
                          }



-------------------------------------------------------------------------------
