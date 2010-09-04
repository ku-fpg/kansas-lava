{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Comb where

import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Types.Type
import Language.KansasLava.Wire
import Data.Sized.Unsigned as U


-- import Control.Applicative

----------------------------------------------------------------------------------------------------
-- an obserable (k)ombinatoral value. Not a functor, applicative functor, or monad.

data Comb a = Comb !(X a) (D a)

combValue :: Comb a -> X a
combValue (Comb a d) = a

combDriver :: Comb a -> D a
combDriver (Comb a d) = d

instance forall a . (RepWire a, Show a) => Show (Comb a) where
	show (Comb x _) = showRepWire (undefined :: a) x

instance forall a . (Wire a, Eq a) => Eq (Comb a) where
	(Comb x _) == (Comb y _) = (unX x :: Maybe a) == (unX y :: Maybe a)

-- ACF: Since the shallow part of Comb is strict, we can't use error here.
--      This only seems to come up in debugging. We could create a special
--      wrapper to encode the error status like we did in the deep, but
--      adding the class constraint is less invasive for now.
-- deepComb e = Comb (error "shallow argument being used incorrectly") e
deepComb :: forall a. (Wire a) => D a -> Comb a
deepComb e = Comb (optX (Nothing :: Maybe a)) e

shallowComb :: X a -> Comb a
shallowComb a = Comb a (D $ Error "deep argument being used incorrectly")

-- ACF: We should probably redefine this with dual:
--      errorComb = dual (deepComb $ error "errorComb") (shallowComb $ error "errorComb")
--      (the calls to error get thrown away by dual)
--      but this would require some major module rearrangement to avoid circular imports
errorComb ::  forall a . (Wire a) => Comb a
errorComb = Comb (optX $ (Nothing :: Maybe a)) (D $ Error "errorComb")

applyComb0 :: (Wire a) => Comb a -> Maybe a
applyComb0 (Comb a _) = unX a

applyComb1 :: (Wire a, Wire b) => (Comb a -> Comb b) -> a -> Maybe b
applyComb1 f a = unX b
   where Comb b _ = f (Comb (pureX a) (D $ Error "deep embedding problem in apply1"))

applyComb2 :: (Wire a, Wire b, Wire c) => (Comb a -> Comb b -> Comb c) -> a -> b -> Maybe c
applyComb2 f a b = unX c
   where Comb c _ = f (Comb (pureX a) (D $ Error "deep embedding problem in apply2"))
	              (Comb (pureX b) (D $ Error "deep embedding problem in apply2"))

-- Hmm, not the same deep side as toSeq; why?
toComb :: (RepWire a) => a -> Comb a
toComb a = Comb (pureX a) $ D $ Lit $ fromIntegral $ U.fromMatrix $ fromWireRep a

toComb' :: forall a . (RepWire a) => Maybe a -> Comb a
toComb' a = shallowComb (optX a)



----------------------------------------------------------------------------------------------------
