{-# LANGUAGE ScopedTypeVariables #-}

module Language.KansasLava.QuickCheck where

import Language.KansasLava
import Test.QuickCheck
import Control.Applicative

-- So we can generate arbitrary combinatorial values for QuickCheck.
instance (Arbitrary a, Wire a) => Arbitrary (Comb a) where
    arbitrary = shallowComb <$> (optX :: Maybe a -> X a) <$> arbitrary
    -- TODO: shrink?

-- Note this isn't incredibly useful below, since Seq's don't
-- implement equality, but maybe useful in the future.
instance (Arbitrary a, Wire a) => Arbitrary (Seq a) where
    arbitrary = toSeq' <$> arbitrary
    -- TODO: shrink?

-- Use QuickCheck to verify some laws of boolean algebra.
-- TODO: Adjust these to handle '?' correctly. (absorb for instance)
prop_notNeverEqual s = bitNot s /= s
    where types = s::(Comb Bool)

prop_doubleNot s = s == (bitNot $ bitNot s)
    where types = s::(Comb Bool)

-- Identity
prop_orIdentity s = s `or2` false == s
    where types = s::(Comb Bool)

prop_andIdentity s = s `and2` true == s
    where types = s::(Comb Bool)

-- Annihiliation
prop_orAnni s = s `or2` true == true
    where types = s::(Comb Bool)

prop_andAnni s = s `and2` false == false
    where types = s::(Comb Bool)

-- Idempotence
prop_orIdemp s = s `or2` s == s
    where types = s::(Comb Bool)

prop_andIdemp s = s `and2` s == s
    where types = s::(Comb Bool)

-- Absorption
prop_absorb1 x y = x `and2` (x `or2` y) == x
    where types = (x::(Comb Bool),y::(Comb Bool))

prop_absorb2 x y = x `or2` (x `and2` y) == x
    where types = (x::(Comb Bool),y::(Comb Bool))

-- Associativity
prop_orAssoc x y z = (x `or2` (y `or2` z)) == ((x `or2` y) `or2` z)
    where types = (x::(Comb Bool),y::(Comb Bool),z::(Comb Bool))

prop_andAssoc x y z = (x `and2` (y `and2` z)) == ((x `and2` y) `and2` z)
    where types = (x::(Comb Bool),y::(Comb Bool),z::(Comb Bool))

-- Commutivity
prop_orComm x y = (x `or2` y) == (y `or2` x)
    where types = (x::(Comb Bool),y::(Comb Bool))

prop_andComm x y = (x `and2` y) == (y `and2` x)
    where types = (x::(Comb Bool),y::(Comb Bool))

-- Distributivity
prop_orDistOverAnd x y z = (x `or2` (y `and2` z)) == ((x `or2` y) `and2` (x `or2` z))
    where types = (x::(Comb Bool),y::(Comb Bool),z::(Comb Bool))

prop_andDistOverOr x y z = (x `and2` (y `or2` z)) == ((x `and2` y) `or2` (x `and2` z))
    where types = (x::(Comb Bool),y::(Comb Bool),z::(Comb Bool))

