module Language.KansasLava.Applicative where
	
import Language.KansasLava.Signal
import Language.KansasLava.Entity
import Language.KansasLava.Seq as S


import Control.Applicative


instance Functor Signal where
   fmap f = o0 . entity1 (Name "Applicative" "fmap") [Var "o0"] [Var "i0"] [] f

instance Applicative Signal where
   pure a = o0 $ entity0 (Name "Applicative" "pure") [Var "o0"] [] a
   f1 <*> f2  = o0 $ entity2 (Name "Applicative" "<*>") [Var "o0"] [Var "i0",Var "i1"] [] ($) f1 f2


-- Simulation only functions.
peek :: Signal a -> [Maybe a]
peek ~(Signal xs _) = S.toList xs

-- A signal, that changes over time
with :: [a] -> Signal a
with xs = poke (map Just xs ++ repeat Nothing) 

-- A signal with possible unknown values, that changes over time.
poke :: [Maybe a] -> Signal a
poke xs = Signal (S.fromList xs) undefined

-- A signal of a  constant value, for testing
be :: a -> Signal a
be e = pure e
-- Signal (pure e) (error "improper use of be")      -- for now




