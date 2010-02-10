module Language.KansasLava.Applicative where

- TO REINSTATE
{-
import Language.KansasLava.Signal
import Language.KansasLava.Entity
import Language.KansasLava.Seq as S

import Control.Applicative
import Data.Foldable
import Data.Traversable

instance Functor Signal where
   fmap f (Signal s _) = o0 $ ESignal (fmap f s) $ E $  e
     where e =
             (Entity (Name "Applicative" "fmap") (error "Can't have fmap in ent") (error "can't have fmap in ent") [])

instance Applicative Signal where
   pure a = o0 $ ESignal (pure a) $ E $  e
     where e =  (Entity (Name "Applicative" "pure") (error "Can't have pure in ent") (error "can't have pure in ent") [])

   (Signal f1 _) <*> (Signal f2 _) = o0 $ ESignal (f1 <*> f2) $ E $  e
     where e =  (Entity (Name "Applicative" "<*>") (error "Can't have pure in <*>") (error "can't have <*>  in ent") [])

instance Foldable Signal where
  foldMap _ _ = error "Foldable.foldMap(Signal) not defined"

instance Traversable Signal where
 traverse f (Signal s d) = Signal <$> traverse f s <*> pure d

-- Simulation only functions.
peek :: Signal a -> [Maybe a]
peek ~(Signal xs _) = S.toList xs

-- A signal, that changes over time
with :: String -> [a] -> Signal a
with sigName xs = poke sigName (map Just xs ++ repeat Nothing)

-- A signal with possible unknown values, that changes over time.
poke :: String -> [Maybe a] -> Signal a
poke sigName xs = Signal (S.fromList xs) (Pad (Var sigName))

-- A signal of a  constant value, for testing
be :: a -> Signal a
be e = pure e
-- Signal (pure e) (error "improper use of be")      -- for now

-}

