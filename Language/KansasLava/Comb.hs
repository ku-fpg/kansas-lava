{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Comb where
	
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Type
import Language.KansasLava.Wire
import Data.Sized.Unsigned as U


-- import Control.Applicative

----------------------------------------------------------------------------------------------------
-- an obserable (k)ombinatoral value. Not a functor, applicative functor, or monad.

data Comb a = Comb (X a) (D a)

combValue :: Comb a -> X a
combValue (Comb a d) = a

combDriver :: Comb a -> D a
combDriver (Comb a d) = d

instance forall a . (RepWire a, Show a) => Show (Comb a) where
	show (Comb x _) = showRepWire (undefined :: a) x

instance forall a . (Wire a, Eq a) => Eq (Comb a) where
	(Comb x _) == (Comb y _) = (unX x :: Maybe a) == (unX y :: Maybe a)

deepComb :: D a -> Comb a
deepComb e = Comb (error "shallow argument being used incorrectly") e

shallowComb :: X a -> Comb a
shallowComb a = Comb a (error "deep argument being used incorrectly")

errorComb ::  forall a . (Wire a) => Comb a 
errorComb = Comb (optX $ (Nothing :: Maybe a)) (D $ Lit 0)		-- perhaps add a I do not know

applyComb0 :: (Wire a) => Comb a -> Maybe a
applyComb0 (Comb a _) = unX a 

applyComb1 :: (Wire a, Wire b) => (Comb a -> Comb b) -> a -> Maybe b
applyComb1 f a = unX b
   where Comb b _ = f (Comb (pureX a) (error "deep embedding problem in apply1"))

applyComb2 :: (Wire a, Wire b, Wire c) => (Comb a -> Comb b -> Comb c) -> a -> b -> Maybe c
applyComb2 f a b = unX c
   where Comb c _ = f (Comb (pureX a) (error "deep embedding problem in apply2"))
	              (Comb (pureX b) (error "deep embedding problem in apply2"))

-- Hmm, not the same deep side as toSeq; why?
toComb :: (RepWire a) => a -> Comb a
toComb a = Comb (pureX a) $ D $ Lit $ fromIntegral $ U.fromMatrix $ fromWireRep a

toComb' :: forall a . (RepWire a) => Maybe a -> Comb a
toComb' a = shallowComb (optX a)



----------------------------------------------------------------------------------------------------
