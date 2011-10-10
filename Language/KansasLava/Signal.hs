{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses #-}
-- | The Signal module provides a number of classes and functions for constructing
-- Lava temporally varying signals.
module Language.KansasLava.Signal where

-- import Control.Applicative

import Language.KansasLava.Comb
--import Language.KansasLava.Entity
import Language.KansasLava.Rep
import Language.KansasLava.Types

--import Data.Sized.Ix
--import Data.Sized.Matrix as M
--import Data.Maybe(fromMaybe)

-- | The Signal class provides functions for lifting combinational values and
-- functions into a time-varying sream model.

class Signal' f where
  liftS0 :: (Rep a) => Comb a -> f a
  -- ^ Lift a combinatinal value to be a stream
  liftS1 :: (Rep a, Rep b) => (Comb a -> Comb b) -> f a -> f b
  -- ^ Lift a unary function over combinational circuits to a function over Signals.
  liftS2 :: (Rep a, Rep b, Rep c) => (Comb a -> Comb b -> Comb c) -> f a -> f b -> f c
  -- ^ Lift a binary function over combinational circuits to a function over Signals.
  liftS3 :: (Rep a, Rep b, Rep c) => (Comb a -> Comb b -> Comb c -> Comb d) -> f a -> f b -> f c -> f d
  -- ^ Lift a ternary function over combinational circuits to a function over Signals.
  liftSL :: (Rep a, Rep b) => ([Comb a] -> Comb b) -> [f a] -> f b
  -- ^ Lift an n-ary function (with homogeneous arguments) over combinational
  -- circuits to a function over Signals.
  deepS  :: f a -> D a
  -- ^ Extract the deep embedding from a signal.


--class Constant a where
--  pureS :: (Signal s) => a -> s a

{-
-- | Lift a representable Haskell value -- not a Comb -- to be a signal.
-- TO REMOVE
pureS' :: (Signal s, Rep a) => a -> s a
pureS' a = liftS0 (toComb a)


-- | Create nn unknown (X) signal.
undefinedS :: (Signal s, Rep a) => s a
undefinedS = liftS0 undefinedComb
-}

-- | k is a constant

----------------------------------------------------------------------------------------------------
-- Should these be merged?

-- TODO: insert Id/Comment
-- | Wrap a Signal with a comment.


----------------------------------------------------------------------------------------------------
{-
instance Signal Comb where
    liftS0 a    = a
    liftS1 f    = f
    liftS2 f    = f
    liftS3 f    = f
    liftSL f    = f
    deepS (Comb _ d) = d
-}


--------------------------------------------------------------------------------
{-
-- | Lift a value (with a primitively-defined name in the deep embedding) to be a signal.
fun0 :: forall a sig . (Signal sig, Rep a) => String -> a -> sig a
fun0 nm a = liftS0 $ Comb (optX $ Just a) $ entity0 (Prim nm)

-- | Lift a unary function (with a primitively-defined name in the deep embedding) to be a signal.
fun1 :: forall a b sig . (Signal sig, Rep a, Rep b) => String -> (a -> b) -> sig a -> sig b
fun1 nm f = liftS1 $ \ (Comb a ae) -> Comb (optX $ liftA f (unX a)) $ entity1 (Prim nm) ae

-- | Lift a possibly failing (indicated by returning Nothing) over values to be over signals.
fun1' :: forall a b sig . (Signal sig, Rep a, Rep b) => String -> (a -> Maybe b) -> sig a -> sig b
fun1' nm f = liftS1 $ \ (Comb a ae) -> Comb (optX $ fromMaybe Nothing (liftA f (unX a)))
                                            (entity1 (Prim nm) ae)

-- | Lift a binary function (with a primitively-defined name in the deep embedding) to be a signal.
fun2 :: forall a b c sig . (Signal sig, Rep a, Rep b, Rep c) => String -> (a -> b -> c) -> sig a -> sig b -> sig c
fun2 nm f = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (optX $ liftA2 f (unX a) (unX b))
	  $ entity2 (Prim nm) ae be

-}

-----------------------------------------------------------------------------------------------

{-
instance (Size ix, Rep ix, Rep a, Signal sig) => Pack sig (ix -> a) where
	type Unpacked sig (ix -> a) = sig ix -> sig a

	-- Example: pack :: (Seq X4 -> Seq Int) -> Seq (X4 -> Int)
	-- TODO: think some more about this
	pack f = error "Can not pack a function, sorry"

	unpack = liftS2 $ \ (Comb (XFunction f) me) (Comb x xe) ->
				Comb (case (unX x) of
				    	Just x' -> f x'
				    	Nothing -> optX Nothing
			     	     )
			$ entity2 (Prim "read") me xe

-}
