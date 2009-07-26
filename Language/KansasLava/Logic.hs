{-# LANGUAGE FlexibleInstances #-}

module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Data.Bits

--
{-
instance Num Bool where

-- instance Bits (Signal Bool) where
    True .&. True = True
    _    .&. _    = False
    False .|. False = False
    _     .|. _     = True
    False `xor` False = True
    True `xor` True   = True
    _     `xor` _      = False
    s `shift` 0 = s
    s `shift` _ = False
    s `rotate` _ = s
 
    complement True = False
    complement False = True

    bitSize s                       = 1 
    isSigned s                      = False
-}
{-
and2 :: (Signal a, Signal a) -> Signal a
and2 (Signal w1,Signal w2) = Signal $ Wire $ Entity (name "and2") [w1,w2]

high :: Signal a
high = Signal $ Wire $ Pad $ name "high"

low :: (BOOL bool) => bool
low = error "low"
-- low  = Signal $ Wire $ Pad $ name "low"
-}
{-
-- mux :: (SEQ sig, BOOL bool, sig Bool ~ bool) => sig Bool -> (sig a,sig a) -> sig a
mux :: (COND bool a) => bool -> (a, a) ->  a
mux = error "undefined Mux"
-}
--mux :: Signal Bool -> (Signal a, Signal a) -> Signal a
--mux (Signal s) (Signal w1,Signal w2) = Signal $ Wire $ Entity (name "mux") [s,w1,w2]

bitAnd :: Signal Bool -> Signal Bool -> Signal Bool
bitAnd = entity2 (Name "Bool" "&&") (&&)
bitOr :: Signal Bool -> Signal Bool -> Signal Bool
bitOr  = entity2 (Name "Bool" "||") (||)
bitXor :: Signal Bool -> Signal Bool -> Signal Bool
bitXor = entity2 (Name "Bool" "xor") (/=)

bitNot :: Signal Bool -> Signal Bool
bitNot = entity1 (Name "Bool" "not") (not)
