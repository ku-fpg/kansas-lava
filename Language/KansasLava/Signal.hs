{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses #-}

module Language.KansasLava.Signal where

import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Wire
import Language.KansasLava.Entity.Utils
import Control.Applicative

import Data.Sized.Ix
import Data.Sized.Unsigned as U
import Data.Sized.Matrix as M

class Signal f where
  liftS0 :: Comb a -> f a
  liftS1 :: (Comb a -> Comb b) -> f a -> f b
  liftS2 :: (Comb a -> Comb b -> Comb c) -> f a -> f b -> f c
  liftSL :: ([Comb a] -> Comb b) -> [f a] -> f b

bitTypeOf :: forall f w . (Signal f, Wire w) => f w -> BaseTy
bitTypeOf _ = wireType (error "bitTypeOf" :: w) 

op :: forall f w . (Signal f, Wire w) => f w -> String -> Name
op _ nm = Name (wireName (error "op" :: w)) nm

--class Constant a where
--  pureS :: (Signal s) => a -> s a

pureS :: (Signal s, RepWire a) => a -> s a
pureS a = liftS0 (constComb a)


-- | k is a constant 

----------------------------------------------------------------------------------------------------

instance Signal Comb where
  liftS0 a     = a
  liftS1 f a   = f a
  liftS2 f a b = f a b
  liftSL f xs  = f xs


class (Signal sig) => Pack sig a where
 type Unpacked sig a
 pack :: Unpacked sig a -> sig a
 unpack :: sig a -> Unpacked sig a

--------------------------------------------------------------------------------

constComb :: (RepWire a) => a -> Comb a
constComb a = Comb (pureX a) $ D $ Lit $ fromIntegral $ U.fromMatrix $ fromWireRep a

--------------------------------------------------------------------------------

liftS3 :: forall a b c d sig . (Signal sig, Wire a, Wire b, Wire c, Wire d)
       => (Comb a -> Comb b -> Comb c -> Comb d) -> sig a -> sig b -> sig c -> sig d
liftS3 f a b c = liftS2 (\ ab c -> uncurry f (unpack ab) c) (pack (a,b) :: sig (a,b)) c

--------------------------------------------------------------------------------

fun0 :: forall a sig . (Signal sig, Wire a) => String -> a -> sig a
fun0 nm a = liftS0 $ Comb (optX $ Just $ a) $ entity0 (Name (wireName (error "fun1" :: a)) nm)

fun1 :: forall a b sig . (Signal sig, Wire a, Wire b) => String -> (a -> b) -> sig a -> sig b
fun1 nm f = liftS1 $ \ (Comb a ae) -> Comb (optX $ liftA f (unX a)) $ entity1 (Name (wireName (error "fun1" :: b)) nm) ae

fun2 :: forall a b c sig . (Signal sig, Wire a, Wire b, Wire c) => String -> (a -> b -> c) -> sig a -> sig b -> sig c
fun2 nm f = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (optX $ liftA2 f (unX a) (unX b)) 
	  $ entity2 (Name (wireName (error "fun2" :: c)) nm) ae be


-----------------------------------------------------------------------------------------------

instance (Wire a, Wire b, Signal sig) => Pack sig (a,b) where 
	type Unpacked sig (a,b) = (sig a, sig b)
	pack ~(a,b) = liftS2 (\ ~(Comb a ae) ~(Comb b be) -> Comb (a,b) (entity2 (Name "Lava" "pair") ae be))
			    a b
	unpack ab = ( liftS1 (\ (Comb (~(a,b)) abe) -> Comb a (entity1 (Name "Lava" "fst") abe)) ab
		    , liftS1 (\ (Comb (~(a,b)) abe) -> Comb b (entity1 (Name "Lava" "snd") abe)) ab
		    )

instance (Wire a, Signal sig, Integral ix, Num ix, Size ix) => Pack sig (Matrix ix a) where 
	type Unpacked sig (Matrix ix a) = Matrix ix (sig a)
	pack m = liftSL (\ ms -> let sh = M.fromList [ m | Comb m  _ <- ms ] 
				     de = entityN (Name "Lava" "concat") [ d | Comb _ d <- ms ]
				 in Comb sh de) (M.toList m) 
	unpack s = forAll $ \ ix -> 
			liftS1 (\ (Comb s d) -> Comb (s ! ix) 
					       (entity2 (Name "Lava" "index") 
							(D $ Lit $ fromIntegral ix :: D Integer)
							d
					       )
			        ) s

