module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative

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


high :: Signal Bool
high = Signal (pure True) $ Wire $ Pad $ name "high"

low :: Signal Bool
low = Signal (pure False) $ Wire $ Pad $ name "high"

-- entity3??
mux :: Signal Bool -> (Signal a, Signal a) -> Signal a
mux ~(Signal vs s) (~(Signal vs1 w1),~(Signal vs2 w2))
       = Signal (fromList [ if v then v1 else v2
                          | (v,v1,v2) <- zip3 (toList vs)
                                              (toList vs1)
                                              (toList vs2)
                          ])
       $ Wire $ Entity (name "mux") [s,w1,w2]



bitAnd :: Signal Bool -> Signal Bool -> Signal Bool
bitAnd = entity2 (Name "Bool" "&&") (&&)
bitOr :: Signal Bool -> Signal Bool -> Signal Bool
bitOr  = entity2 (Name "Bool" "||") (||)
bitXor :: Signal Bool -> Signal Bool -> Signal Bool
bitXor = entity2 (Name "Bool" "xor") (/=)

bitNot :: Signal Bool -> Signal Bool
bitNot = entity1 (Name "Bool" "not") (not)
