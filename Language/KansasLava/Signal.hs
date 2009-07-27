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

data Signal a = Signal (Seq a) (Driver E)
-- newtype Wire = Wire (Driver E)

-- internal, special use only (when defining entities, for example).
data ESignal a = ESignal (Seq a) E
newtype E = E (Entity E)

-- You want to observe
instance MuRef E where 
  type DeRef E = Entity
  mapDeRef f (E s) = T.traverse f s


-- not sure about this, should this use the shallow part?
instance Eq (Signal a) where
   (Signal _ s1) == (Signal _ s2) = s1 == s2
{-
instance Eq Wire where
   (Wire s1) == (Wire s2) = s1 == s2
-}
instance (Show a) => Show (Signal a) where
    show (Signal (Constant v) _) = show v
    show (Signal vs _) = unwords [ show x ++ " :~ " 
                                | x <- take 20 $ S.toList vs
                                ] ++ "..."

{-
instance Show Wire where
    show (Wire s) = show s
-}

instance Show E where
    show (E s) = show s
    
instance Eq E where
   (E s1) == (E s2) = s1 == s2
    
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

--fun1 nm f s     = entity1 (op s nm) inputs f s
--fun2 nm f s1 s2 = entity2 (op s1 nm) inputs f s1 s2
inputs = [Var $ "i" ++ show (n :: Int) | n <- [0..]]
entity1 :: Name -> [Var] -> [Var] -> (a -> b) -> Signal a -> ESignal b
entity1 nm ins outs f s@(~(Signal vs1 w1)) 
        = ESignal (pure f <*> vs1)
        $ E
        $ Entity nm outs $ zip ins [w1]
        
entity2 :: Name -> [Var] -> [Var] -> (a -> b -> c) -> Signal a -> Signal b -> ESignal c
entity2 nm ins outs f s@(~(Signal vs1 w1)) ~(Signal vs2 w2)
        = ESignal (pure f <*> vs1 <*> vs2)
        $ E
        $ Entity nm outs $ zip ins [w1,w2]

entity3 :: Name -> [Var] -> [Var] -> (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> ESignal d
entity3 nm ins outs f s@(~(Signal vs1 w1)) ~(Signal vs2 w2) ~(Signal vs3 w3)
        = ESignal (pure f <*> vs1 <*> vs2 <*> vs3)
        $ E
        $ Entity nm outs $ zip ins [w1,w2,w3]

o0 :: ESignal a -> Signal a
o0 ~(ESignal v e) = Signal v (Port (Var "o0") e)

fun1 nm f s     = o0 $ entity1 (op s nm) inputs [Var "o0"] f s
fun2 nm f s1 s2 = o0 $ entity2 (op s1 nm) inputs [Var "o0"] f s1 s2


instance (Num a, OpType a) => Num (Signal a) where
    s1 + s2 = fun2 "+" (+) s1 s2
    s1 - s2 = fun2 "-" (-) s1 s2
    s1 * s2 = fun2 "*" (*) s1 s2
    negate s = fun1 "negate" (negate) s
    abs s    = fun1 "abs"    (abs)    s
    signum s = fun1 "signum" (signum) s
    fromInteger n               = s
            where s = Signal (pure (fromInteger n))
                    $ Lit $ n

instance (Bits a, OpType a) => Bits (Signal a) where
    s1 .&. s2 = fun2 ".&." (.&.) s1 s2
    s1 .|. s2 = fun2 ".|." (.|.) s1 s2
    s1 `xor` s2 = fun2 "xor" (xor) s1 s2
    s1 `shift` n = fun2 "shift" (shift) s1 (fromIntegral n)
    s1 `rotate` n = fun2 "rotate" (rotate) s1 (fromIntegral n)
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
  explode :: ESignal e -> Ex e
--  implode :: Ex e -> Signal e

-- TODO: somehow wire in better names than o1 and o2.
instance Explode (a,b) where
  type Ex (a,b) = (Signal a,Signal b)
  explode ~(ESignal v w) =
        ( Signal (fmap fst v) $ Port (Var "o1") w
        , Signal (fmap snd v) $ Port (Var "o2") w
        )
{-
  implode (~(Signal v1 w1),~(Signal v2 w2)) =
        Signal ((,) <$> v1 <*> v2) $ Wire $ Entity (Name "$" "implode") [w1,w2]  
-}
{-
implode2 :: (Signal a, Signal b) -> Signal (a,b)
implode2 = implode
-}

view :: Signal a -> [Maybe a]
view ~(Signal xs _) = S.toList xs
