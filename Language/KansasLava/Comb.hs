{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}
-- | Data types and functions for representing and manipulating with
-- combinational logic.
module Language.KansasLava.Comb(
 Comb(..),toComb,
 undefinedComb,deepComb,shallowComb,
 combValue, combDriver
 ) where

import Language.KansasLava.Rep
import Language.KansasLava.Types

----------------------------------------------------------------------------------------------------
-- | An observable Combinatorial value. Not a functor, applicative functor, or monad.
data Comb a = Comb !(X a) (D a)

-- | Extract the shallow value from a combinational circuit.
combValue :: Comb a -> X a
combValue (Comb a _) = a

-- | Extract the deep value driver from a combinational circuit.
combDriver :: Comb a -> D a
combDriver (Comb _ d) = d

instance (Rep a) => Show (Comb a) where
	show (Comb x _) = showRep x


-- This is required for Arithmetic to be overloaded.
instance (Rep a, Eq a) => Eq (Comb a) where
	(Comb x _) == (Comb y _) = unX x == unX y

-- | Generate a circuit with a the given deep embedding. The shallow value is a stream of 'Nothing's, representing unknown values.
deepComb :: forall a. (Rep a) => D a -> Comb a
deepComb = Comb (optX Nothing)

-- | Generate a circuit with the given shallow value. The deep value is
-- undefined, and will result in an error if it is evaluated.
shallowComb :: X a -> Comb a
shallowComb a = Comb a (D $ Error "deep argument being used incorrectly")

-- | A undefined combinational circuit, where both the shallow and the deep values are undefined.
undefinedComb ::  forall a . (Rep a) => Comb a
undefinedComb = Comb (optX Nothing)
		     (D $ Lit $ toRep (optX (Nothing :: Maybe a)))


-- Hmm, not the same deep side as toSeq; why?
-- | Convert a value to a combinational circuit.
toComb :: forall a . (Rep a) => a -> Comb a
toComb a = Comb (pureX a) $ D $ Lit $ toRep (pureX a)

instance Dual (Comb a) where
    dual c d = Comb (combValue c) (combDriver d)


