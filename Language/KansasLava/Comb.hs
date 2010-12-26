{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

module Language.KansasLava.Comb where

-- import Language.KansasLava.Entity
import Language.KansasLava.Deep
import Language.KansasLava.Types
import Language.KansasLava.Wire
import Data.Sized.Unsigned as U


----------------------------------------------------------------------------------------------------
-- | An obserable Combinatoral value. Not a functor, applicative functor, or monad.

data Comb a = Comb !(X a) (D a)

combValue :: Comb a -> X a
combValue (Comb a d) = a

combDriver :: Comb a -> D a
combDriver (Comb a d) = d

instance forall a . (Rep a) => Show (Comb a) where
	show (Comb x _) = showRep (Witness :: Witness a) x


-- This is required for Arithmetic to be overloaded.
instance forall a . (Rep a, Eq a) => Eq (Comb a) where
	(Comb x _) == (Comb y _) = (unX x) == (unX y)

-- ACF: Since the shallow part of Comb is strict, we can't use error here.
--      This only seems to come up in debugging. We could create a special
--      wrapper to encode the error status like we did in the deep, but
--      adding the class constraint is less invasive for now.
-- deepComb e = Comb (error "shallow argument being used incorrectly") e
deepComb :: forall a. (Rep a) => D a -> Comb a
deepComb e = Comb (optX Nothing) e

shallowComb :: X a -> Comb a
shallowComb a = Comb a (D $ Error "deep argument being used incorrectly")

-- ACF: We should probably redefine this with dual:
--      undefinedComb = dual (deepComb $ error "undefinedComb") (shallowComb $ error "undefinedComb")
--      (the calls to error get thrown away by dual)
--      but this would require some major module rearrangement to avoid circular imports
undefinedComb ::  forall a . (Rep a) => Comb a
undefinedComb = Comb (optX Nothing)
		     (D $ Lit $ toRep (optX (Nothing :: Maybe a)))

applyComb0 :: (Rep a) => Comb a -> Maybe a
applyComb0 (Comb a _) = unX a

applyComb1 :: (Rep a, Rep b) => (Comb a -> Comb b) -> a -> Maybe b
applyComb1 f a = unX b
   where Comb b _ = f (Comb (pureX a) (D $ Error "deep embedding problem in apply1"))

applyComb2 :: (Rep a, Rep b, Rep c) => (Comb a -> Comb b -> Comb c) -> a -> b -> Maybe c
applyComb2 f a b = unX c
   where Comb c _ = f (Comb (pureX a) (D $ Error "deep embedding problem in apply2"))
	              (Comb (pureX b) (D $ Error "deep embedding problem in apply2"))

-- Hmm, not the same deep side as toSeq; why?
toComb :: forall a . (Rep a) => a -> Comb a
toComb a = Comb (pureX a) $ D $ Lit $ toRep (pureX a)

toComb' :: forall a . (Rep a) => Maybe a -> Comb a
toComb' a = shallowComb (optX a)

fromComb :: (Rep a) => Comb a -> Maybe a
fromComb comb = unX (combValue comb)

----------------------------------------------------------------------------------------------------
