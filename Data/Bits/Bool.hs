module Data.Bits.Bool where

import Data.Bits

-- These should really be in their own package.
instance Num Bool where
   True  + True    = False
   True  + False   = True
   False + True    = True
   False + False   = False
   -- (-) defined in terms of negate and +
   True  * True    = True
   True  * False   = False
   False * True    = False
   False * False   = False
   -- To negate 0 is 0, to negate 1 is -1, which is True.
   negate b     = b
   abs    b     = b
   signum b     = b
   fromInteger = odd

instance Bits Bool where
    True .&. True = True
    _    .&. _    = False
    False .|. False = False
    _     .|. _     = True
    False `xor` False = False
    True `xor` True   = False
    _     `xor` _     = True
    s `shift` 0 = s
    s `shift` _ = False
    s `rotate` _ = s
 
    complement True = False
    complement False = True

    bitSize s                       = 1 
    isSigned s                      = False
