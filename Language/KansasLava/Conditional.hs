module Language.KansasLava.Conditional where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative



(.==.) :: (Eq a) => Signal a -> Signal a -> Signal Bool
(.==.) sA sB = o0 $ entity2 (Name "Bool" ".==.")
		 [Var "i0", Var "i1"]
		 [Var "o0"]
		 [ [B,TyVar $ Var "o0"]
		 , [TyVar $ Var "i0", TyVar $ Var "i1"]
		 ]
		 (\ a b -> a == b)
		 sA sB
(.>=.) :: (Ord a) => Signal a -> Signal a -> Signal Bool
(.>=.) sA sB = o0 $ entity2 (Name "Bool" ".>=.")
		 [Var "i0", Var "i1"]
		 [Var "o0"]
		 [ [B,TyVar $ Var "o0"]
		 , [TyVar $ Var "i0", TyVar $ Var "i1"]
		 ]
		 (\ a b -> a >= b)
		 sA sB

