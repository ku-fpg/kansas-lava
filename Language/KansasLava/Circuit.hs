module Language.KansasLava.Circuit 
	( Circuit(..)
	, CircuitOptions(..)
	, DepthOp(..)
	) where


import Data.Reify
import Data.List as L
import qualified Data.Map as Map

import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Types

import Debug.Trace

-------------------------------------------------------------------------

