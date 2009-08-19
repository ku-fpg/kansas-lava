module Language.KansasLava.Applicative where
	
import Language.KansasLava.Signal
import Language.KansasLava.Entity
import Control.Applicative


instance Functor Signal where
   fmap f = o0 . entity1 (Name "Applicative" "fmap") [Var "o0"] [Var "i0"] [] f

instance Applicative Signal where
   pure a = o0 $ entity0 (Name "Applicative" "pure") [Var "o0"] [] a
   f1 <*> f2  = o0 $ entity2 (Name "Applicative" "<*>") [Var "o0"] [Var "i0",Var "i1"] [] ($) f1 f2

