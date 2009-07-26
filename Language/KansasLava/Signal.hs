{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies  #-}

module Language.KansasLava.Signal where

import Data.Ratio
import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Data.Reify
import qualified Data.Traversable as T

import Language.KansasLava.Entity
import Language.KansasLava.Bool
import Language.KansasLava.Seq as S
import Data.Monoid
import Debug.Trace
import Control.Applicative

-- AJG: to consider, adding AF and Functor to this type.

data Signal a = Signal (Seq a) Wire 

newtype Wire = Wire (Entity Wire) 

instance MuRef Wire where 
  type DeRef Wire = Entity
  mapDeRef f (Wire s) = T.traverse f s 

-- not sure about this, should this use the shallow part?
instance Eq (Signal a) where
   (Signal _ s1) == (Signal _ s2) = s1 == s2

instance Eq Wire where
   (Wire s1) == (Wire s2) = s1 == s2

instance (Show a) => Show (Signal a) where
    show (Signal v _) = show v

instance Show Wire where
    show (Wire s) = show s
    
class OpType a where
    op :: Signal a -> String -> Name
    signalOf :: Signal a -> a
    signalOf = undefined

instance OpType Int    where op _ nm = Name "Int" nm
instance OpType Float  where op _ nm = Name "Float" nm
instance OpType Double where op _ nm = Name "Double" nm

instance OpType Int32 where op _  nm = Name "Int32" nm
instance OpType Word32 where op _ nm = Name "Word32" nm

instance OpType Bool where op _  nm = Name "Bool" nm
instance OpType ()   where op _  nm = Name "()" nm

-- find the name of the type of the entity arguments.
findEntityTyModName :: (OpType a) => Entity a -> String
findEntityTyModName e = nm
  where
    (Name nm _) = fn e undefined
    fn :: (OpType a) => c a -> Signal a -> Name
    fn _ s = s `op` ""
    
-------------------------------------------

fun1 nm f s     = entity1 (op s nm) f s
fun2 nm f s1 s2 = entity2 (op s1 nm) f s1 s2
entity1 var f s@(~(Signal vs1 w1))
        = Signal (pure f <*> vs1)
        $ Wire 
        $ Entity var [w1]
entity2 var f s@(~(Signal vs1 w1)) ~(Signal vs2 w2) 
        = Signal (pure f <*> vs1 <*> vs2)
        $ Wire 
        $ Entity var [w1,w2]
entity3 var f s@(~(Signal vs1 w1)) ~(Signal vs2 w2) ~(Signal vs3 w3) 
        = Signal (pure f <*> vs1 <*> vs2 <*> vs3)
        $ Wire 
        $ Entity var [w1,w2,w3]

instance (Num a, OpType a) => Num (Signal a) where
    s1 + s2 = fun2 "+" (+) s1 s2
    s1 - s2 = fun2 "-" (-) s1 s2
    s1 * s2 = fun2 "*" (*) s1 s2
    negate s = fun1 "negate" (negate) s
    abs s = fun1 "abs" (abs) s
    signum s = fun1 "signum" (signum) s
    fromInteger n               = s
            where s = Signal (pure (fromInteger n))
                    $ Wire $ Entity (op s "fromInteger")
                                    [ Wire $ Lit $ n
                                    ]

instance (Bits a, OpType a) => Bits (Signal a) where
    s1 .&. s2 = fun2 ".&." (.&.) s1 s2
    s1 .|. s2 = fun2 ".&." (.|.) s1 s2
    s1 `xor` s2 = fun2 "xor" (xor) s1 s2
    s1 `shift` n = fun2 "shift" (shift) s1 (Signal (pure n) (Wire $ Lit $ fromIntegral n))
    s1 `rotate` n = fun2 "rotate" (rotate) s1 (Signal (pure n) (Wire $ Lit $ fromIntegral n))
    complement s = fun1 "complement" (complement) s
    bitSize s                       = bitSize (signalOf s)
    isSigned s                      = isSigned (signalOf s)

{-
-- TODO: represent in terms of new fun/entity interface.

instance (Fractional a, OpType a) => Fractional (Signal a) where
    s@(Signal s1) / (Signal s2) = Signal $ Wire $ Entity (op s "/")     [s1,s2]
    recip s@(Signal s1)         = Signal $ Wire $ Entity (op s "recip") [s1]
    fromRational r              = s 
            where s = Signal $ Wire $ Entity (op s "fromRational")
                                                -- The :% arguments are tupled here
                                    [ Wire $ Lit $ numerator r
                                    , Wire $ Lit $ denominator r
                                    ]


instance (Floating a, OpType a) => Floating (Signal a) where
    pi 				= s where s = Signal $ Wire $ Pad (op s "pi")
    exp s@(Signal s1)         = Signal $ Wire $ Entity (op s "exp") [s1]
    sqrt s@(Signal s1)         = Signal $ Wire $ Entity (op s "sqrt") [s1]
    log s@(Signal s1)         = Signal $ Wire $ Entity (op s "log") [s1]

    sin s@(Signal s1)         = Signal $ Wire $ Entity (op s "sin") [s1]
    tan s@(Signal s1)         = Signal $ Wire $ Entity (op s "tan") [s1]
    cos s@(Signal s1)         = Signal $ Wire $ Entity (op s "cos") [s1]
    asin s@(Signal s1)         = Signal $ Wire $ Entity (op s "asin") [s1]
    atan s@(Signal s1)         = Signal $ Wire $ Entity (op s "atan") [s1]
    acos s@(Signal s1)         = Signal $ Wire $ Entity (op s "acos") [s1]
    sinh s@(Signal s1)         = Signal $ Wire $ Entity (op s "sinh") [s1]
    tanh s@(Signal s1)         = Signal $ Wire $ Entity (op s "tanh") [s1]
    cosh s@(Signal s1)         = Signal $ Wire $ Entity (op s "cosh") [s1]
    asinh s@(Signal s1)         = Signal $ Wire $ Entity (op s "asinh") [s1]
    atanh s@(Signal s1)         = Signal $ Wire $ Entity (op s "atanh") [s1]
    acosh s@(Signal s1)         = Signal $ Wire $ Entity (op s "acosh") [s1]
-}

class Explode e where
  type Ex e
  explode :: Signal e -> Ex e

-- TODO: somehow wire in better names than o1 and o2.
instance Explode (a,b) where
  type Ex (a,b) = (Signal a,Signal b)
  explode ~(Signal v w) =
        ( Signal (fmap fst v) $ Wire $ Port (Var "o1") w
        , Signal (fmap snd v) $ Wire $ Port (Var "o2") w
        )


view :: Signal a -> [Maybe a]
view ~(Signal xs _) = S.toList xs
