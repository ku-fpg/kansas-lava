{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables
 #-}
module Language.KansasLava.Entity where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative
import Data.Monoid

import Language.KansasLava.Types.Type
import Data.Reify
import qualified Data.Traversable as T
import Control.Applicative
import Data.Unique as U

data Id = Name String String			-- external thing
	  | Prim String				-- built in thing
	  | UniqNm U.Unique 			-- uniq name of an entity
	  | Function [(Integer,Integer)] 	-- anonymous function
    deriving (Eq, Ord)

instance Show Id where
    show (Name "" nm)  = nm	-- do we use "" or "Lava" for the magic built-in?
    show (Name pre nm) = pre ++ "::" ++ nm
    show (Prim nm)     = nm
    show (UniqNm n)    = "#" ++ show (hashUnique n) -- might not be uniq
    show (Function _)  = "<fn>"

-- TODO: Just use String here
type Var = String
--data Var = Var String
--    deriving (Eq,Ord)

--instance Show Var where
--    show (Var nm)     = nm

-------------------------------------------------------------------------
-- Entity

-- We tie the knot at the 'Entity' level, for observable sharing.
data Entity ty a s = Entity Id [(Var,ty)] [(Var,ty,Driver s)] [a]
			-- specialized Entity, because tables (typically ROMs) are verbose.
		   | Table (Var,ty) (Var,ty,Driver s) [(Integer,String,Integer,String)]
              deriving (Show, Eq, Ord)

instance T.Traversable (Entity ty a) where
  traverse f (Entity v vs ss dyn) =
    Entity v vs <$> (T.traverse (\ (val,ty,a) -> ((,,) val ty) `fmap` T.traverse f a) ss) <*> pure dyn
  traverse f (Table (v0,t0) (v1,t1,d) tbl) =
	(\ d' -> Table (v0,t0) (v1,t1,d') tbl) <$> T.traverse f d

instance F.Foldable (Entity ty a) where
  foldMap f (Entity _ _ ss _) = mconcat [ F.foldMap f d | (_,_,d) <- ss ]

instance Functor (Entity ty a) where
    fmap f (Entity v vs ss dyn) = Entity v vs (fmap (\ (var,ty,a) -> (var,ty,fmap f a)) ss) dyn


-------------------------------------------------------------------------
-- Driver

data Driver s = Port Var s      -- a specific port on the entity
              | Pad OVar       	  --
              | Lit Integer
	      | Error String	-- A call to err, in Datatype format for reification purposes
              deriving (Eq, Ord)

instance Show i => Show (Driver i) where
  show (Port v i) = "(" ++ show i ++ ")." ++ show v 
  show (Lit x) = show x
  show (Pad v) = show v 
  show (Error msg) = show msg

instance T.Traversable Driver where
  traverse f (Port v s)    = Port v <$> f s
  traverse _ (Pad v)       = pure $ Pad v
--  traverse _ (PathPad v)   = pure $ PathPad v
  traverse _ (Lit i)       = pure $ Lit i
  traverse _ (Error s)     = pure $ Error s

instance F.Foldable Driver where
  foldMap f (Port _ s)    = f s
  foldMap _ (Pad _)       = mempty
--  foldMap _ (PathPad _)   = mempty
  foldMap _ (Lit _)       = mempty
  foldMap _ (Error s)     = mempty

instance Functor Driver where
    fmap f (Port v s)    = Port v (f s)
    fmap _ (Pad v)       = Pad v
--    fmap _ (PathPad v)   = PathPad v
    fmap _ (Lit i)       = Lit i
    fmap _ (Error s)     = Error s


-------------------------------------------------------------------------
-- OVar

data OVar = OVar Int String		-- The # is used purely for sorting order.
	deriving (Eq, Ord, Read)

instance Show OVar where
	show (OVar i nm) = nm ++ "$" ++ show i

-------------------------------------------------------------------------




