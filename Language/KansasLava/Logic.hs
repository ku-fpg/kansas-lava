{-# LANGUAGE TypeFamilies, RankNTypes, ScopedTypeVariables #-}
-- | The 'Logic' module provides basic combinational circuit combinators.

module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative
import Data.Sized.Unsigned as U


high :: Signal Bool
high = Signal (pure True) $ Lit 1

low :: Signal Bool
low = Signal (pure False) $ Lit 0


mux4 :: (MUX a) => Signal U2 -> a -> a -> a -> a -> a
mux4 u2 a b c d = mux2 b1 (mux2 b2 a b) (mux2 b2 c d)
   where
	b1 = testABit u2 0
	b2 = testABit u2 1
{-
class BROADWAY a where
--  type LANE a
  promote :: (forall x . (BROADWAY x) => x -> x -> x) -> a -> a -> a

instance BROADWAY (Signal a) where
  promote f = f

-- mux and delay can use this.
instance (BROADWAY a,BROADWAY b) => BROADWAY (a,b) where
  promote f (a,b) (a',b') = (f a a',f b b')
-}
{-
instance (BROADWAY a) => BROADWAY [a] where
  promote f (x:xs) (y:ys) = promote f x : [] -- promote f xs
  promote f []     [] = []
-}

class MUX a where
  mux2 :: Signal Bool -> a -> a -> a
  mux3 :: Signal Ordering -> a -> a -> a -> a

--  mux4 :: Signal (Bool,Bool) -> a -> a -> a -> a -> a
--   mux3 o a1 a2 a3 = (mux2 (o == EQ) a2) (mux2 (o == LT

instance (MUX a,MUX b) => MUX (a,b) where
   mux2 x (a,b) (a',b') = (mux2 x a a',mux2 x b b')
   mux3 x (a,b) (a',b') (a'',b'') = (mux3 x a a' a'', mux3 x b b' b'')

instance OpType a => MUX (Signal a) where
  mux2 sC@(~(Signal b _)) sT@(~(Signal t _)) sF@(~(Signal f _)) =
	clone (Signal (seqMux b t f) (error "bad entity for mux"))
              (o0 $ entity3 (Name "Bool" "mux2")
              [Var "c",Var "t", Var "f"]
              [Var "o0"]
	      (error "mux functionality misunderstood")
--              (\ a b c -> if a then b else c)
              sC sT sF)

  mux3 _ _ _ = error "undefined method mux3 in MUX (Signal a)"


cases :: (MUX cir) => [(Signal Bool,cir)] -> cir -> cir
cases []           def = def
cases ((b,c):rest) def = mux2 b c (cases rest def)

-- Misc Bool signal things.
-- We do not overload Bool at Bits because
-- Bool is not a Num, and correctly so. We use U1 instead.

--	    (Sized ix) => Signal a -> ix -> Signal Bool
testABit :: (Bits a, OpType a) => Signal a -> Int -> Signal Bool
testABit x y = o0 $ entity1 (Name "Bits" "testABit") inputs [Var "o0"] (\ x' -> testBit x' y) x
	where inputs   = take 2 defaultInputs

and2 :: Signal Bool -> Signal Bool -> Signal Bool
and2 x y = o0 $ entity2 (Name "Bool" "and2") inputs [Var "o0"] (&&) x y
	where inputs   = take 2 defaultInputs

or2 :: Signal Bool -> Signal Bool -> Signal Bool
or2 x y = o0 $ entity2 (Name "Bool" "or2") inputs [Var "o0"] (||) x y
        where inputs = take 2 defaultInputs

xor2 ::  Signal Bool -> Signal Bool -> Signal Bool
xor2 x y = o0 $ entity2 (Name "Bool" "xor2") inputs [Var "o0"] (/=) x y
	where inputs   = take 2 defaultInputs

bitNot :: Signal Bool -> Signal Bool
bitNot x = o0 $ entity1 (Name "Bool" "not") inputs [Var "o0"]  not x
	where inputs   = take 1 defaultInputs

-- Use QuickCheck to verify some laws of boolean algebra. This should
-- probably be moved to it's own file somewhere.
-- TODO: make sure our definition of equality isn't misleading us here
--       also, not all of these currently pass... will fix another day
prop_notNeverEqual s = bitNot s /= s
    where types = s::(Signal Bool)

prop_doubleNot s = s == (bitNot $ bitNot s)
    where types = s::(Signal Bool)

-- Identity
prop_orIdentity s = s `or2` low == s
    where types = s::(Signal Bool)

prop_andIdentity s = s `and2` high == s
    where types = s::(Signal Bool)

-- Annihiliation
prop_orAnni s = s `or2` high == high
    where types = s::(Signal Bool)

prop_andAnni s = s `and2` low == low
    where types = s::(Signal Bool)

-- Idempotence
prop_orIdemp s = s `or2` s == s
    where types = s::(Signal Bool)

prop_andIdemp s = s `and2` s == s
    where types = s::(Signal Bool)

-- Absorption
prop_absorb1 x y = x `and2` (x `or2` y) == x
    where types = (x::(Signal Bool),y::(Signal Bool))

prop_absorb2 x y = x `or2` (x `and2` y) == x
    where types = (x::(Signal Bool),y::(Signal Bool))

-- Associativity
prop_orAssoc x y z = (x `or2` (y `or2` z)) == ((x `or2` y) `or2` z)
    where types = (x::(Signal Bool),y::(Signal Bool),z::(Signal Bool))

prop_andAssoc x y z = (x `and2` (y `and2` z)) == ((x `and2` y) `and2` z)
    where types = (x::(Signal Bool),y::(Signal Bool),z::(Signal Bool))

-- Commutivity
prop_orComm x y = (x `or2` y) == (y `or2` x)
    where types = (x::(Signal Bool),y::(Signal Bool))

prop_andComm x y = (x `and2` y) == (y `and2` x)
    where types = (x::(Signal Bool),y::(Signal Bool))

-- Distributivity
prop_orDistOverAnd x y z = (x `or2` (y `and2` z)) == ((x `or2` y) `and2` (x `or2` z))
    where types = (x::(Signal Bool),y::(Signal Bool),z::(Signal Bool))

prop_andDistOverOr x y z = (x `and2` (y `or2` z)) == ((x `and2` y) `or2` (x `and2` z))
    where types = (x::(Signal Bool),y::(Signal Bool),z::(Signal Bool))

