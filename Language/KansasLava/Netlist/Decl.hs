-- | This module generates Netlist 'Decl's for a circuit graph.
module Language.KansasLava.Netlist.Decl where

import Language.KansasLava.Types
import Language.Netlist.AST

import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils

-- Entities that need a _next special *extra* signal.
--toAddNextSignal :: [Id]
--toAddNextSignal = [Prim "register"]


-- We have a few exceptions, where we generate some extra signals,
-- but in general, we generate a single signal decl for each
-- entity.
-- | Generate declarations.
genDecl :: (Unique, Entity Unique) -> [Decl]
-- Special cases
{-
genDecl (i,Entity nm outputs _)
        | nm `elem` toAddNextSignal
	= concat
	  [ [ NetDecl (next $ sigName n i) (sizedRange nTy) Nothing
	    , MemDecl (sigName n i) Nothing (sizedRange nTy)
	    ]
	  | (n,nTy) <- outputs  ]
genDecl (i,e@(Entity nm outputs@[_] inputs)) | nm == Prim "BRAM"
	= concat
	  [ [ MemDecl (sigName n i) (memRange aTy) (sizedRange nTy)
	    , NetDecl (sigName n i) (sizedRange nTy) Nothing
	    ]
	  | (n,nTy) <- outputs ]
  where
	aTy = lookupInputType "wAddr" e

genDecl (i,Entity nm outputs _)
        | nm `elem` isVirtualEntity
	= []
-}
-- General case
genDecl (i,e@(Entity _ outputs _))
	= [ case toStdLogicTy nTy of
	      MatrixTy x (V y)
	        -> let x' = head [ po2 | po2 <- iterate (*2) 1
	                               , po2 >= x
	                         ]
	           in MemDecl
	            (sigName n i)
	            (sizedRange (V x'))
	            (sizedRange (V y))
                    (case e of
                        Entity (Prim "rom")
                               [("o0",_)]
                               [("defs",RomTy _,Lits lits)]
                          -- This is reversed because we defined from (n-1) downto 0
                          -> Just $ reverse $ map (toTypedExpr (V y))
                                            $ take x'
                                              (lits ++ repeat (RepValue $ replicate y $ Just False))
                        _ -> Nothing
                    )
	      _ -> NetDecl
	            (sigName n i)
	            (sizedRange nTy)
	            (case e of
	                Entity (Prim "register")
	                        [("o0",ty)]
	                        [ _, ("def",GenericTy,gn), _, _, _] ->
	                     Just (toTypedExpr ty gn)
	                _ -> Nothing)
	  | (n,nTy) <- outputs
	  , toStdLogicTy nTy /= V 0
	  ]
