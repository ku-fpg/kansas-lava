{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables
 #-}

-- | This module contains ways of building deep embedings.

module Language.KansasLava.Deep where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative
import Data.Monoid

import Language.KansasLava.Types
import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Types
import Language.KansasLava.Wire
import Language.KansasLava.Stream

import Control.Applicative
import Data.Dynamic

entity0 :: forall o . (Rep o) => Id -> D o
entity0 nm = D $ Port ("o0") $ E $
 	Entity nm [("o0",oTy)]
		  []
		  []
   where oTy = wireType (Witness :: Witness o)

entity1 :: forall a o . (Rep a, Rep o) => Id -> D a -> D o
entity1 nm (D w1) = D $ Port ("o0") $ E $
 	Entity nm [("o0",oTy)]
		  [(inp,ty,val) | inp <- ["i0","i1"]
				| ty <- [aTy]
				| val <- [w1]
		  ] []
   where aTy = wireType (Witness :: Witness a)
         oTy = wireType (Witness :: Witness o)

entity2 :: forall a b o . (Rep a, Rep b, Rep o) => Id -> D a -> D b -> D o
entity2 nm (D w1) (D w2) = D $ Port ("o0") $ E $
 	Entity nm [("o0",oTy)]
		  [(inp,ty,val) | inp <- ["i0","i1"]
				| ty <- [aTy,bTy]
				| val <- [w1,w2]
		  ] []
   where aTy = wireType (Witness :: Witness a)
         bTy = wireType (Witness :: Witness b)
         oTy = wireType (Witness :: Witness o)

entity3 :: forall a b c o . (Rep a, Rep b, Rep c, Rep o) => Id -> D a -> D b -> D c -> D o
entity3 nm (D w1) (D w2) (D w3) = D $ Port ("o0") $ E $
 	Entity nm [("o0",oTy)]
		  [(inp,ty,val) | inp <- ["i0","i1","i2"]
				| ty <- [aTy,bTy,cTy]
				| val <- [w1,w2,w3]
		  ] []
   where aTy = wireType (Witness :: Witness a)
         bTy = wireType (Witness :: Witness b)
         cTy = wireType (Witness :: Witness c)
         oTy = wireType (Witness :: Witness o)

entityN :: forall a b o . (Rep a, Rep o) => Id -> [D a] -> D o
entityN nm ds = D $ Port ("o0") $ E $
 	Entity nm [("o0",oTy)]
		  [(inp,ty,val) | inp <- ["i" ++ show n | n <- [0..]]
				| ty <- repeat aTy
				| val <- [w | D w <- ds]
		  ] []
   where aTy = wireType (Witness :: Witness a)
         oTy = wireType (Witness :: Witness o)
