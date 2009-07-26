module Language.KansasLava.Entity where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative
import Data.Unique
import Data.Monoid
  
data Name = Name String String
    deriving (Eq, Ord)

instance Show Name where
    show (Name pre nm) = "<" ++ pre ++ ">" ++ nm
    show (Name "" nm)  = nm

name :: String -> Name
name n  = Name "" n

data Var = Var String
    deriving (Eq,Ord)

instance Show Var where
    show (Var nm)  = nm

data Entity s = Entity Name [s]      -- an entity
              | Port Var s           -- projection; get a specific named port of an entity
              | Pad Var
              | Lit Integer
              deriving (Show, Eq, Ord)

instance T.Traversable Entity where
  traverse f (Entity v ss) = Entity v <$> T.traverse f ss
  traverse f (Port v s)    = Port v   <$> f s
  traverse _ (Pad v)       = pure $ Pad v
  traverse _ (Lit i)       = pure $ Lit i
  
instance F.Foldable Entity where
  foldMap f (Entity v ss) = F.foldMap f ss
  foldMap f (Port v s)    = f s
  foldMap _ (Pad v)       = mempty
  foldMap _ (Lit i)       = mempty
    
instance Functor Entity where
    fmap f (Entity v ss) = Entity v (fmap f ss)
    fmap f (Port v s)    = Port v (f s)
    fmap _ (Pad v)       = Pad v
    fmap _ (Lit i)       = Lit i
