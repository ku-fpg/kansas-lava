{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

module Language.KansasLava.Signal where

import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Wire


class Signal f where
  liftS0 :: Comb a -> f a
  liftS1 :: (Comb a -> Comb b) -> f a -> f b
  liftS2 :: (Comb a -> Comb b -> Comb c) -> f a -> f b -> f c
  liftSL :: ([Comb a] -> Comb b) -> [f a] -> f b

bitTypeOf :: forall f w . (Signal f, Wire w) => f w -> BaseTy
bitTypeOf _ = wireType (error "bitTypeOf" :: w) 

op :: forall f w . (Signal f, Wire w) => f w -> String -> Name
op _ nm = Name (wireName (error "op" :: w)) nm

class Constant a where
  pureS :: (Signal s) => a -> s a

-- | k is a constant 

----------------------------------------------------------------------------------------------------

instance Signal Comb where
  liftS0 a     = a
  liftS1 f a   = f a
  liftS2 f a b = f a b
  liftSL f xs  = f xs

constComb :: (Constant a) => a -> Comb a
constComb = pureS