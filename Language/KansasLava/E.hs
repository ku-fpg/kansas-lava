{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.E where

import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Type

import Language.KansasLava.Entity
import Language.KansasLava.Seq as S

import Control.Applicative
import Data.Sized.Unsigned as UNSIGNED
import Data.Sized.Signed as SIGNED
import Data.Sized.Sampled as SAMPLED
import Data.Sized.Arith as Arith
import Data.Sized.Ix as X

{-
newtype E = E (Entity BaseTy E)

-- You want to observe
instance MuRef E where
  type DeRef E = Entity BaseTy
  mapDeRef f (E s) = T.traverse f s

instance Show E where
    show (E s) = show s

instance Eq E where
   (E s1) == (E s2) = s1 == s2


data E2 a = E2 a E


-}