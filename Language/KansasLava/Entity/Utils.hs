{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables
 #-}
module Language.KansasLava.Entity.Utils where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative
import Data.Monoid

import Language.KansasLava.Types
import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Types
import Language.KansasLava.Wire
-- import Language.KansasLava.Entity
import Language.KansasLava.Stream

import Control.Applicative
import Data.Dynamic


-----------------------------------------------------------------------------------------------
-- And the utilties that get this done.
