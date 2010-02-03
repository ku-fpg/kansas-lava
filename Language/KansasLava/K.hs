{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies #-}

module Language.KansasLava.K where
	
import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Language.KansasLava.E
import Language.KansasLava.Wire

import Control.Applicative

----------------------------------------------------------------------------------------------------

-- an obserable (k)ombinatoral value. Not a functor, applicative functor, or monad.
data K a = K (X a) (Driver E)

instance Show (X a) => Show (K a) where
	show (K x _) = show x

deepK :: Driver E -> K a
deepK e = K (error "shallow argument being used incorrectly") e

shallowK :: (Wire a) => a -> K a
shallowK a = K (pureX a) (error "deep argument being used incorrectly")

applyK0 :: (Wire a) => K a -> Maybe a
applyK0 (K a _) = unX a 

applyK1 :: (Wire a, Wire b) => (K a -> K b) -> a -> Maybe b
applyK1 f a = unX b
   where K b _ = f (K (pureX a) (error "deep embedding problem in apply1"))

applyK2 :: (Wire a, Wire b, Wire c) => (K a -> K b -> K c) -> a -> b -> Maybe c
applyK2 f a b = unX c
   where K c _ =  f (K (pureX a) (error "deep embedding problem in apply2"))
	            (K (pureX b) (error "deep embedding problem in apply2"))

----------------------------------------------------------------------------------------------------
-- This might move into a class called '*.Classes'.

class SIGNAL f where
  liftS0 :: K a -> f a
  liftS1 :: (K a -> K b) -> f a -> f b
  liftS2 :: (K a -> K b -> K c) -> f a -> f b -> f c

class Constant a where
  pureS :: (SIGNAL s) => a -> s a

-- | k is a constant 
k :: (Constant a) => a -> K a
k = pureS

----------------------------------------------------------------------------------------------------

instance SIGNAL K where
  liftS0 a     = a
  liftS1 f a   = f a
  liftS2 f a b = f a b

instance Constant Bool where
  pureS v = liftS0 $ K (pureX v) $ Lit $ fromWireRep v


{-
----------------------------------------------------------------------------------------------------

newtype E = E (Entity BaseTy E)
	deriving Show

and2K :: K Bool -> K Bool -> K Bool
and2K (K a1 e1) 
      (K a2 e2) = K (a1 && a2)
		  $ Port (Var "o0")
 		  $ E
		  $ Entity (Name "K" "and") 
			    [(Var "o0",B)] 
			    [(Var "o0",B,e1),(Var "o1",B,e2)]
			    []


and2 :: (Deliver f) => f Bool -> f Bool -> f Bool
and2 = liftD2 $ \ a b -> and2K a b 

true :: K Bool
true = K True $ entity0 (Name "K" "False") B

false :: K Bool
false = K False $ entity0 (Name "K" "False") B

data Signal a = Signal (Seq a) (Driver E)
	deriving Show



--k :: (Konstant a, Deliver f) => a -> f a
--k a = liftD0 a


instance Deliver Signal where
  liftD0 (K a e) = Signal (pure a) (entity1 (Name "K" "k") B e B)
  liftD1 f (Signal a ea) = Signal (fmap (apply1 f) a) eb
      where
	K _ eb = f (K (error "liftD1, f's arg, Signal") ea)
  liftD2 f (Signal a ea) (Signal b eb) = Signal (liftA2 (apply2 f) a b) ec
      where
	K _ ec = f (K (error "liftD2, f's arg, Signal") ea)
		   (K (error "liftD2, f's arg, Signal") eb)

entity0 :: Name -> BaseTy -> Driver E
entity0 nm ty = Port (Var "o0") $ E $ Entity nm
					[(Var "o0",ty)] 
					[]
					[]

entity1 :: Name -> BaseTy -> Driver E -> BaseTy -> Driver E
entity1 nm ty1 e ty2 = Port (Var "o0") $ E $ Entity nm
							[(Var "o0",ty2)] 
							[(Var "i0",ty1,e)]
							[]

--------

data Enabled a = Enabled (Signal a) (Signal Bool)
	deriving Show

instance Deliver Enabled where
  liftD0 a = Enabled (liftD0 a) (pureD True)
  liftD1 f (Enabled v en) = Enabled (liftD1 f v) en
  liftD2 f (Enabled va en1) (Enabled vb en2)
	= Enabled (liftD2 f va vb)
		  (and2 en1 en2)

enable :: Enabled a -> Signal Bool
enable (Enabled _ en) = en


------------------------------------------------------------------------------

delay :: K a -> Signal Bool -> Signal a -> Signal a
delay = undefined

circuit :: (K a -> K a) -> K a -> Signal Bool -> Signal a -> Signal a
circuit cir def rst inp = res
    where
	res = liftD1 cir (delay def rst res)


shiftRegister :: Int -> Enabled a -> Enabled a
shiftRegister = undefined


------------------------------------------------------------------------------

data HandShake a = HandShake (Signal Bool -> Enabled a)

instance Deliver HandShake where
  liftD0 a = HandShake $ \ _ -> liftD0 a
  liftD1 f (HandShake fn) = HandShake $ \ hand -> liftD1 f (fn hand)
	-- this, I think, is wrong (splits the request)
  liftD2 f (HandShake fn1) (HandShake fn2) = HandShake $ \ hand ->
		liftD2 f (fn1 hand) (fn2 hand)

------------------------------------------------------------------------------

-- A sequence of datams sent over time.
data Packet x a = Packet (Signal a) 		--- the data
		         (Signal Bool) 		-- this is valid data
			 (Signal Bool)		-- valid data starts (for x parts)

{-
-- can be Enabled, because we compress the signal.
packetToMatrix :: Packet x a -> Enabled (Matrix x a)
packetToMatrix packet =
-}	

--matrixToPacket :: HandShake (Matrix x a) -> Packet x a

------------------------------------------------------------------------------
{- Ideas

  ReadMemory a d = Signal a -> Signal d
  WriteMemory :: Enabled (a,d) -> Signal a -> Signal d

-}
-}