{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables
 #-}
module Language.KansasLava.Entity where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative
import Data.Monoid

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

-- We tie the knot at the 'Entity' level, for observable sharing.
data Entity ty a s = Entity Name [(Var,ty)] [(Var,ty,Driver s)] [a]
		   | Table (Var,ty) (Var,ty,Driver s) [(Integer,String,Integer,String)]
              deriving (Show, Eq, Ord)

{-
-- UGGGGGG! This is the wrong place for these things.
instance Eq Dynamic where
 _ == _ = True

instance Ord Dynamic where
 compare _ _ = EQ
-}

-- These can all be unshared without any problems.
data Driver s = Port Var s      -- a specific port on the entity
              | Pad Var         --
	      | PathPad [Int]	-- a unique path to a pad
              | Lit Integer --
              deriving (Show, Eq, Ord)


instance T.Traversable (Entity ty a) where
  traverse f (Entity v vs ss dyn) =
    Entity v vs <$> (T.traverse (\ (val,ty,a) -> ((,,) val ty) `fmap` T.traverse f a) ss) <*> pure dyn
  traverse f (Table (v0,t0) (v1,t1,d) tbl) =
	(\ d' -> Table (v0,t0) (v1,t1,d') tbl) <$> T.traverse f d

instance T.Traversable Driver where
  traverse f (Port v s)    = Port v <$> f s
  traverse _ (Pad v)       = pure $ Pad v
  traverse _ (PathPad v)   = pure $ PathPad v
  traverse _ (Lit i)       = pure $ Lit i


instance F.Foldable (Entity ty a) where
  foldMap f (Entity _ _ ss _) = mconcat [ F.foldMap f d | (_,_,d) <- ss ]

instance F.Foldable Driver where
  foldMap f (Port _ s)    = f s
  foldMap _ (Pad _)       = mempty
  foldMap _ (PathPad _)   = mempty
  foldMap _ (Lit _)       = mempty

instance Functor (Entity ty a) where
    fmap f (Entity v vs ss dyn) = Entity v vs (fmap (\ (var,ty,a) -> (var,ty,fmap f a)) ss) dyn

instance Functor Driver where
    fmap f (Port v s)    = Port v (f s)
    fmap _ (Pad v)       = Pad v
    fmap _ (PathPad v)   = PathPad v
    fmap _ (Lit i)       = Lit i

