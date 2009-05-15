{-# LANGUAGE TypeFamilies #-}

module Language.KansasLava.Signal where

import Data.Ratio
import Data.Word
import Data.Int
import Data.Bits

import Data.Reify
import qualified Data.Traversable as T

import Language.KansasLava.Entity

newtype Signal a = Signal Wire 

newtype Wire = Wire (Entity Wire) 

instance MuRef Wire where 
  type DeRef Wire = Entity
  mapDeRef f (Wire s) = T.traverse f s 

instance Eq (Signal a) where
   (Signal s1) == (Signal s2) = s1 == s2

instance Eq Wire where
   (Wire s1) == (Wire s2) = s1 == s2

instance Show (Signal a) where
    show (Signal s) = show s

instance Show Wire where
    show (Wire s) = show s
    
    
class OpType a where
    op :: Signal a -> String -> Var
    signalOf :: Signal a -> a
    signalOf = undefined

instance OpType Int    where op _ nm = Var $ ["Int",nm]
instance OpType Float  where op _ nm = Var $ ["Float",nm]
instance OpType Double where op _ nm = Var $ ["Double",nm]

instance OpType Int32 where op _  nm = Var $ ["Int32",nm]
instance OpType Word32 where op _ nm = Var $ ["Word32",nm]

-------------------------------------------

instance (Num a, OpType a) => Num (Signal a) where
    s@(Signal s1) + (Signal s2) = Signal $ Wire $ Entity (op s "+")      [s1,s2]
    s@(Signal s1) - (Signal s2) = Signal $ Wire $ Entity (op s "-")      [s1,s2]
    s@(Signal s1) * (Signal s2) = Signal $ Wire $ Entity (op s "*")      [s1,s2]
    negate s@(Signal s1)        = Signal $ Wire $ Entity (op s "negate") [s1]
    abs s@(Signal s1)           = Signal $ Wire $ Entity (op s "abs")    [s1]
    signum s@(Signal s1)        = Signal $ Wire $ Entity (op s "signum") [s1]
    fromInteger n               = s
            where s = Signal $ Wire $ Entity (op s "lit")
                                    [ Wire $ Lit $ n
                                    ]

instance (Bits a,OpType a) => Bits (Signal a) where
    s@(Signal s1) .&. (Signal s2)   = Signal $ Wire $ Entity (op s ".&.") [s1,s2]
    s@(Signal s1) .|. (Signal s2)   = Signal $ Wire $ Entity (op s ".|.") [s1,s2]
    s@(Signal s1) `xor` (Signal s2) = Signal $ Wire $ Entity (op s "xor") [s1,s2]
    s@(Signal s1) `shift` n         = Signal $ Wire $ Entity (op s "shift") 
                                        [s1, Wire $ Lit $ fromIntegral n]
    s@(Signal s1) `rotate` n        = Signal $ Wire $ Entity (op s "rotate") 
                                        [s1, Wire $ Lit $ fromIntegral n]
    complement s@(Signal s1)        = Signal $ Wire $ Entity (op s "complement") [s1]
    bitSize s                       = bitSize (signalOf s)
    isSigned s                      = isSigned (signalOf s)

instance (Fractional a, OpType a) => Fractional (Signal a) where
    s@(Signal s1) / (Signal s2) = Signal $ Wire $ Entity (op s "/")     [s1,s2]
    recip s@(Signal s1)         = Signal $ Wire $ Entity (op s "recip") [s1]
    fromRational r              = s 
            where s = Signal $ Wire $ Entity (op s ":%")
                                    [ Wire $ Lit $ numerator r
                                    , Wire $ Lit $ denominator r
                                    ]

