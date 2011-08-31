{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses #-}
-- | The Signal module provides a number of classes and functions for constructing
-- Lava temporally varying signals.
module Language.KansasLava.Signal where

import Control.Applicative

import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Rep
import Language.KansasLava.Types

import Data.Sized.Ix
import Data.Sized.Matrix as M
import Data.Maybe(fromMaybe)

-- | The Signal class provides functions for lifting combinational values and
-- functions into a time-varying sream model.
class Signal f where
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

-- | Return the Lava type of a representable signal.
bitTypeOf :: forall f w . (Signal f, Rep w) => f w -> Type
bitTypeOf _ = repType (Witness :: Witness w)

--class Constant a where
--  pureS :: (Signal s) => a -> s a

-- | Lift a representable Haskell value -- not a Comb -- to be a signal.
pureS :: (Signal s, Rep a) => a -> s a
pureS a = liftS0 (toComb a)

-- | Create nn unknown (X) signal.
undefinedS :: (Signal s, Rep a) => s a
undefinedS = liftS0 undefinedComb

-- | k is a constant

----------------------------------------------------------------------------------------------------
-- Should these be merged?

-- TODO: insert Id/Comment
-- | Wrap a Signal with a comment.
comment :: (Signal sig, Rep a) => String -> sig a -> sig a
comment msg = liftS1 $ \ (Comb s ae) -> Comb s $ entity1 (Comment [msg]) ae


----------------------------------------------------------------------------------------------------

instance Signal Comb where
    liftS0 a    = a
    liftS1 f    = f
    liftS2 f    = f
    liftS3 f    = f
    liftSL f    = f
    deepS (Comb _ d) = d

-- | The Pack class allows us to move between signals containing compound data
-- and signals containing the elements of the compound data. This is done by
-- commuting the signal type constructor with the type constructor representing
-- the compound data.  For example, if we have a value x :: Signal sig => sig
-- (a,b), then 'unpack x' converts this to a (sig a, sig b). Dually, pack takes
-- (sig a,sig b) to sig (a,b).
class (Signal sig) => Pack sig a where
 type Unpacked sig a
 -- ^ Pull the sig type *out* of the compound data type.
 pack :: Unpacked sig a -> sig a
 -- ^ Push the sign type *into* the compound data type.
 unpack :: sig a -> Unpacked sig a

--------------------------------------------------------------------------------

-- | Given a function over unpacked (composite) signals, turn it into a function
-- over packed signals.
mapPacked :: (Pack sig a, Pack sig b) => (Unpacked sig a -> Unpacked sig b) -> sig a -> sig b
mapPacked f = pack . f . unpack

-- | Lift a binary function operating over unpacked signals into a function over a pair of packed signals.
zipPacked :: (Pack sig a, Pack sig b, Pack sig c) => (Unpacked sig a -> Unpacked sig b -> Unpacked sig c) -> sig a -> sig b -> sig c
zipPacked f x y = pack $ f (unpack x) (unpack y)

-- | TODO: No idea what this function is supposed to do.
phi :: forall a sig . (Signal sig, Rep a) => sig a -> sig a -> sig a
phi = liftS2 $ \ (Comb a ea) (Comb b _) ->
        Comb (if toRep a == toRep b
		then a
		else optX (fail "phi problem" :: Maybe a))	-- an internal error, like an assert
		ea -- pick one, they are the same
			-- later, consider puting the phi nodes into the deep syntax


--------------------------------------------------------------------------------

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



-----------------------------------------------------------------------------------------------

instance (Rep a, Signal sig) => Pack sig (Maybe a) where
	type Unpacked sig (Maybe a) = (sig Bool, sig a)
	pack (a,b) = {-# SCC "pack_Maybe" #-}
			liftS2 (\ (Comb a' ae) (Comb b' be) ->
				    Comb (case unX a' of
					    Nothing -> optX Nothing
					    Just False -> optX $ Just Nothing
					    Just True ->
						case unX b' of
						   Just v -> optX (Just (Just v))
							-- This last one is strange.
						   Nothing -> optX (Just Nothing)
					 )
					 (entity2 (Prim "pair") ae be)
			     ) a b
	unpack ma = {-# SCC "unpack_Maybe" #-}
		    ( liftS1 (\ (Comb a abe) -> Comb (case unX a of
							Nothing -> optX Nothing
							Just Nothing -> optX (Just False)
							Just (Just _) -> optX (Just True)
						     )
						     (entity1 (Prim "fst") abe)
			      ) ma
		    , liftS1 (\ (Comb a abe) -> Comb (case unX a of
							Nothing -> optX Nothing
							Just Nothing -> optX Nothing
							Just (Just v) -> optX (Just v)
						     )
						     (entity1 (Prim "snd") abe)
			      ) ma
		    )

instance (Rep a, Rep b, Signal sig) => Pack sig (a,b) where
	type Unpacked sig (a,b) = (sig a, sig b)
	pack (a,b) = {-# SCC "pack(,)" #-}
			liftS2 (\ (Comb a' ae) (Comb b' be) -> {-# SCC "pack(,)i" #-} Comb (XTuple (a',b')) (entity2 (Prim "pair") ae be))
			    a b
	unpack ab = {-# SCC "unpack(,)" #-}
		    ( liftS1 (\ (Comb (XTuple (a,_)) abe) -> Comb a (entity1 (Prim "fst") abe)) ab
		    , liftS1 (\ (Comb (XTuple (_,b)) abe) -> Comb b (entity1 (Prim "snd") abe)) ab
		    )

instance (Rep a, Rep b, Rep c, Signal sig) => Pack sig (a,b,c) where
	type Unpacked sig (a,b,c) = (sig a, sig b,sig c)
	pack (a,b,c) = liftS3 (\ (Comb a' ae) (Comb b' be) (Comb c' ce) ->
				Comb (XTriple (a',b',c'))
				     (entity3 (Prim "triple") ae be ce))
			    a b c
	unpack abc = ( liftS1 (\ (Comb (XTriple (a,_b,_)) abce) -> Comb a (entity1 (Prim "fst3") abce)) abc
		    , liftS1 (\ (Comb (XTriple (_,b,_)) abce) -> Comb b (entity1 (Prim "snd3") abce)) abc
		    , liftS1 (\ (Comb (XTriple (_,_,c)) abce) -> Comb c (entity1 (Prim "thd3") abce)) abc
		    )



instance (Rep a, Signal sig, Size ix) => Pack sig (Matrix ix a) where
	type Unpacked sig (Matrix ix a) = Matrix ix (sig a)
	pack m = liftSL (\ ms -> let sh = M.fromList [ m' | Comb m' _ <- ms ]
				     de = entityN (Prim "concat") [ d | Comb _ d <- ms ]
				 in Comb (XMatrix sh) de) (M.toList m)
        -- unpack :: sig (Matrix ix a) -> Matrix ix (sig a)
	unpack s = forAll $ \ ix ->
			liftS1 (\ (Comb (XMatrix s') d) -> Comb (s' ! ix)
					       (entity2 (Prim "index")
							(D $ Generic (mx ! ix) :: D Integer)
							d
					       )
			        ) s
	   where mx :: (Size ix) => Matrix ix Integer
		 mx = matrix (Prelude.zipWith (\ _ b -> b) (M.indices mx) [0..])

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
