{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables
 #-}
module Language.KansasLava.Entity where

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

import Control.Applicative

data Name = Name String String
    deriving (Eq, Ord)

instance Show Name where
    show (Name "" nm)  = nm
    show (Name pre nm) =  pre ++ "::" ++ nm

name :: String -> Name
name n  = Name "" n

data Var = Var String
	 | UqVar [Int]		-- uniquely generated name
         | NoVar                -- not needed, because the default does fine???
    deriving (Eq,Ord)

instance Show Var where
    show (Var nm)     = nm
    show (UqVar path) = "<" ++ foldr (\ p s -> "_" ++ show p ++ s) ">" path
    show NoVar = "NoVar"

-- We tie the knot at the 'Entity' level, for observable sharing.
data Entity ty s = Entity Name [(Var,ty)] [(Var,ty,Driver s)] [(String,Dynamic)]
		 | Table (Var,ty) (Var,ty,Driver s) [(Int,String,Int,String)]
              deriving (Show, Eq, Ord)


-- UGGGGGG! This is the wrong place for these things.
instance Eq Dynamic where
 _ == _ = True

instance Ord Dynamic where
 compare _ _ = EQ


-- These can all be unshared without any problems.
data Driver s = Port Var s      -- a specific port on the entity
              | Pad Var         --
	      | PathPad [Int]	-- a unique path to a pad
              | Lit Integer --
              | BitIndex Int (Driver s)
              | BitSlice Int Int (Driver s)
              deriving (Show, Eq, Ord)


instance T.Traversable (Entity ty) where
  traverse f (Entity v vs ss dyn) =
    Entity v vs <$> (T.traverse (\ (val,ty,a) -> ((,,) val ty) `fmap` T.traverse f a) ss) <*> pure dyn
  traverse f (Table (v0,t0) (v1,t1,d) tbl) =
	(\ d' -> Table (v0,t0) (v1,t1,d') tbl) <$> T.traverse f d

instance T.Traversable Driver where
  traverse f (Port v s)    = Port v <$> f s
  traverse _ (Pad v)       = pure $ Pad v
  traverse _ (PathPad v)   = pure $ PathPad v
  traverse _ (Lit i)       = pure $ Lit i
  traverse f (BitIndex idx s) = BitIndex idx <$> T.traverse f s
  traverse f (BitSlice hi lo s) = BitSlice hi lo <$> T.traverse f s


instance F.Foldable (Entity ty) where
  foldMap f (Entity _ _ ss _) = mconcat [ F.foldMap f d | (_,_,d) <- ss ]

instance F.Foldable Driver where
  foldMap f (Port _ s)    = f s
  foldMap _ (Pad _)       = mempty
  foldMap _ (PathPad _)   = mempty
  foldMap _ (Lit _)       = mempty

instance Functor (Entity ty) where
    fmap f (Entity v vs ss dyn) = Entity v vs (fmap (\ (var,ty,a) -> (var,ty,fmap f a)) ss) dyn

instance Functor Driver where
    fmap f (Port v s)    = Port v (f s)
    fmap _ (Pad v)       = Pad v
    fmap _ (PathPad v)   = PathPad v
    fmap _ (Lit i)       = Lit i

---------------------------------------------------------------------------------------------------------

newtype E = E (Entity BaseTy E)

-- You want to observe
instance MuRef E where
  type DeRef E = Entity BaseTy
  mapDeRef f (E s) = T.traverse f s

instance Show E where
    show (E s) = show s

instance Eq E where
   (E s1) == (E s2) = s1 == s2

---------------------------------------------------------------------------------------------------------

-- A pin to an E/Entity
newtype D a = D (Driver E)
	deriving Show
-- is this used?

unD :: D a -> Driver E
unD (D a) = a

---------------------------------------------------------------------------------------------------------

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
