module Language.KansasLava.Entity where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative 

data Var = Var [String]
    deriving (Show, Eq)
    
var :: String -> Var
var v = Var [v]

data Entity s = Entity Var [s]      -- an entity
              | Port Var s          -- projection; get a specific port of an entity
              | Pad Var
              | Lit Integer
              deriving (Show, Eq)

instance T.Traversable Entity where
  traverse f (Entity v ss) = Entity v <$> T.traverse f ss
  traverse f (Port v s)    = Port v <$> f s
  traverse _ (Pad v)       = pure $ Pad v
  traverse _ (Lit i)       = pure $ Lit i
  
instance F.Foldable Entity where
    -- TODO
    
instance Functor Entity where
    fmap f (Entity v ss) = Entity v (fmap f ss)
    fmap f (Port v s)    = Port v (f s)
    fmap _ (Pad v)       = Pad v
    fmap _ (Lit i)       = Lit i
