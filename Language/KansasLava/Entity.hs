{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
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
{-
-- The old defintion.
data Entity s = Entity Name [(Var,s)]      -- an entity
              | Port Var s                 -- projection; get a specific named port of an entity
              | Pad Var                    -- some in-scope signal
              | Lit Integer
              deriving (Show, Eq, Ord)
-}

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
              | Lit Integer
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
 
{-
entity0 :: OpType a => Name -> [Var] -> a -> ESignal a
entity0 nm outs f
        = ESignal (pure f)
        $ E
        $ Entity nm [(o,ty) | o <- outs] [] []
  where ty = tyRep f

entity1 :: forall a b . (OpType a, OpType b) =>
           Name -> [Var] -> [Var]  -> (a -> b) -> Signal a -> ESignal b
entity1 nm ins outs f  (~(Signal vs1 w1))
        = ESignal (pure f <*> vs1)
        $ E
        $ Entity nm [(o,bTy) | o <- outs] [(inp,ty,val) | inp <- ins | ty <- [aTy] | val <- [w1]] []
   where aTy = tyRep (error "entity1" :: a)
         bTy = tyRep (error "entity1" :: b)

entity2 :: forall a b c . (OpType a, OpType b,OpType c) =>
           Name -> [Var] -> [Var]  -> (a -> b -> c) -> Signal a -> Signal b -> ESignal c
entity2 nm ins outs f (~(Signal vs1 w1)) ~(Signal vs2 w2)
        = ESignal (pure f <*> vs1 <*> vs2)
        $ E
        $ Entity nm [(o,cTy) | o <- outs] [(inp,ty,val) | inp <- ins | ty <- [aTy,bTy] | val <- [w1,w2]] []
   where aTy = tyRep (error "entity2" :: a)
         bTy = tyRep (error "entity2" :: b)
         cTy = tyRep (error "entity2" :: c)

entity3 :: forall a b c d . (OpType a, OpType b,OpType c,OpType d) =>
           Name -> [Var] -> [Var]  -> (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> ESignal d
entity3 nm ins outs f  (~(Signal vs1 w1)) ~(Signal vs2 w2) ~(Signal vs3 w3)
        = ESignal (pure f <*> vs1 <*> vs2 <*> vs3)
        $ E
        $ Entity nm [(o,dTy) | o <- outs] [(inp,ty,val) | inp <- ins | ty <- [aTy,bTy,cTy] | val <- [w1,w2,w3]] []
   where aTy = tyRep (error "entity3" :: a)
         bTy = tyRep (error "entity3" :: b)
         cTy = tyRep (error "entity3" :: c)
         dTy = tyRep (error "entity3" :: d)
{-

and2 :: D Bool -> D Bool -> D Bool
and2 (D e1) (D e2) = D
 	 	  $ Port (Var "o0")
 		  $ E
		  $ Entity (Name "K" "and") 
			    [(Var "o0",B)] 
			    [(Var "o0",B,e1),(Var "o1",B,e2)]
			    []

-}

-}

--o0 :: E a -> Signal a
--o0 ~(E2 v e) = Signal v (Port (Var "o0") e)

