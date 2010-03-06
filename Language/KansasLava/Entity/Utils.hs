{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables
 #-}
module Language.KansasLava.Entity.Utils where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative
import Data.Monoid
import Data.Dynamic

import Language.KansasLava.Type
import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Type
import Language.KansasLava.Wire
import Language.KansasLava.Entity

import Control.Applicative


-----------------------------------------------------------------------------------------------
-- And the utilties that get this done.


entity0 :: forall o . (Wire o) => Name -> D o
entity0 nm = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  []
		  []
   where oTy = wireType (error "entity0" :: o)

entity1 :: forall a o . (Wire a, Wire o) => Name -> D a -> D o
entity1 nm (D w1) = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  [(inp,ty,val) | inp <- map Var ["i0","i1"]
				| ty <- [aTy]
				| val <- [w1]
		  ] []
   where aTy = wireType (error "entity1" :: a)
         oTy = wireType (error "entity1" :: o)

entity2 :: forall a b o . (Wire a, Wire b, Wire o) => Name -> D a -> D b -> D o
entity2 nm (D w1) (D w2) = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  [(inp,ty,val) | inp <- map Var ["i0","i1"]
				| ty <- [aTy,bTy]
				| val <- [w1,w2]
		  ] []
   where aTy = wireType (error "entity2" :: a)
         bTy = wireType (error "entity2" :: b)
         oTy = wireType (error "entity2" :: o)

entity3 :: forall a b c o . (Wire a, Wire b, Wire c, Wire o) => Name -> D a -> D b -> D c -> D o
entity3 nm (D w1) (D w2) (D w3) = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  [(inp,ty,val) | inp <- map Var ["i0","i1","i2"]
				| ty <- [aTy,bTy,cTy]
				| val <- [w1,w2,w3]
		  ] []
   where aTy = wireType (error "entity3" :: a)
         bTy = wireType (error "entity3" :: b)
         cTy = wireType (error "entity3" :: c)
         oTy = wireType (error "entity3" :: o)

entityN :: forall a b o . (Wire a, Wire o) => Name -> [D a] -> D o
entityN nm ds = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  [(inp,ty,val) | inp <- map Var ["i" ++ show n | n <- [0..]]
				| ty <- repeat aTy
				| val <- [w | D w <- ds]
		  ] []
   where aTy = wireType (error "entity2" :: a)
         oTy = wireType (error "entity2" :: o)
