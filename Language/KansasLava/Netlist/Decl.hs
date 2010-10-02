
module Language.KansasLava.Netlist.Decl where

import Language.KansasLava.Types
import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils

import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils

-- Entities that need a _next special *extra* signal.
toAddNextSignal :: [Id]
toAddNextSignal = [Name "Memory" "delay",Name "Memory" "register"]

-- We have a few exceptions, where we generate some extra signals,
-- but in general, we generate a single signal decl for each 
-- entity.

genDecl :: (Unique, MuE Unique) -> [Decl]
-- Special cases
genDecl (i,Entity nm outputs _ _)
        | nm `elem` toAddNextSignal
	= concat
	  [ [ NetDecl (next $ sigName n i) (sizedRange nTy) Nothing
	    , MemDecl (sigName n i) Nothing (sizedRange nTy)
	    ]
	  | (n,nTy) <- outputs  ]
genDecl (i,e@(Entity nm outputs@[_] inputs _)) | nm == Name "Memory" "BRAM"
	= concat 
	  [ [ MemDecl (sigName n i) (memRange aTy) (sizedRange nTy) 
	    , NetDecl (sigName n i) (sizedRange nTy) Nothing
	    ]
	  | (n,nTy) <- outputs ]
  where
	aTy = lookupInputType "wAddr" e
-- General case
genDecl (i,Entity nm outputs _ _)
	= [ NetDecl (sigName n i) (sizedRange nTy) Nothing
	  | (n,nTy) <- outputs  ]
