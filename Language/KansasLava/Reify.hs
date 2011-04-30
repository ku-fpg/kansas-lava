-- | The 'Reify' module converts a the deep embedding of a Lava circuit, wrapped
-- in a 'Fabric', into a 'KLEG' graph.
{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Language.KansasLava.Reify (reifyFabric) where

import Data.List as L
import Data.Maybe(fromMaybe)
import Data.Reify

import qualified Language.KansasLava.Fabric as F
import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Types

-- | 'reifyFabric' does reification of a 'Fabric ()' into a 'KLEG'.
reifyFabric :: F.Fabric () -> IO KLEG
reifyFabric (F.Fabric circuit) = do
        -- This is knot-tied with the output from the circuit execution
        let (_,ins0,outs0) = circuit ins0

        let mkU :: forall a . (Rep a) => Seq a -> Type
            mkU _ = repType (Witness :: Witness a)

        let top_outs = [ (nm, B,    unD $ seqDriver s) | (nm,F.StdLogic s) <- outs0 ] ++
                       [ (nm, mkU s, unD $ seqDriver s) | (nm,F.StdLogicVector s) <- outs0 ]

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
                        , theSrcs = [ (OVar 0 nm,fromStdLogicType $ F.padStdLogicType pad) | (nm,pad) <- ins0 ]
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


