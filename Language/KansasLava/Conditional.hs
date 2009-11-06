module Language.KansasLava.Conditional where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative


(.==.) :: (OpType a, Eq a) => Signal a -> Signal a -> Signal Bool
(.==.) sA sB = o0 $ entity2 (Name "Bool" ".==.")
		 [Var "i0", Var "i1"]
		 [Var "o0"]
		 (\ a b -> a == b)
		 sA sB
(.>=.) :: (OpType a, Ord a) => Signal a -> Signal a -> Signal Bool
(.>=.) sA sB = o0 $ entity2 (Name "Bool" ".>=.")
		 [Var "i0", Var "i1"]
		 [Var "o0"]
		 (\ a b -> a >= b)
		 sA sB
