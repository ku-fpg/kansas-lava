{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies  #-}

module Language.KansasLava.Signal where

import Data.Ratio
import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Type

import Language.KansasLava.Entity
import Language.KansasLava.Bool
import Language.KansasLava.Seq as S
import Data.Monoid
import Debug.Trace
import Control.Applicative

-- AJG: to consider, adding AF and Functor to this type.

data Signal a = Signal (Seq a) (Driver E)
-- newtype Wire = Wire (Driver E)

newtype E = E (Entity (Ty Var) E)

-- internal, special use only (when defining entities, for example).
data ESignal a = ESignal (Seq a) E


-- You want to observe
instance MuRef E where 
  type DeRef E = Entity (Ty Var)
  mapDeRef f (E s) = T.traverse f s

-- not sure about this, should this use the shallow part?
instance Eq (Signal a) where
   (Signal _ s1) == (Signal _ s2) = s1 == s2

instance (Show a) => Show (Signal a) where
    show (Signal v _) = show v

instance Show E where
    show (E s) = show s
    
instance Eq E where
   (E s1) == (E s2) = s1 == s2

------------------------------------------------------------------

-- Should be type 
{-
data WireType 
	= Bit
	| Wires Int		-- number of wires (vector)
	deriving (Show, Eq, Ord)
-}

------------------------------------------------------------------

class OpType a where
    op :: Signal a -> String -> Name
    bitTypeOf :: Signal a -> Ty b
    signalOf :: Signal a -> a
    signalOf = undefined

instance OpType Int    where op _ nm = Name "Int" nm     ; bitTypeOf _ = S 32
instance OpType Float  where op _ nm = Name "Float" nm   ; bitTypeOf _ = S 32
instance OpType Double where op _ nm = Name "Double" nm  ; bitTypeOf _ = S 64

instance OpType Int32 where op _  nm = Name "Int32" nm   ; bitTypeOf _ = S 32
instance OpType Int16 where op _  nm = Name "Int16" nm   ; bitTypeOf _ = S 16
instance OpType Word32 where op _ nm = Name "Word32" nm  ; bitTypeOf _ = U 32

instance OpType Bool where op _  nm = Name "Bool" nm     ; bitTypeOf _ = B
instance OpType ()   where op _  nm = Name "()" nm       ; bitTypeOf _ = U 0

-- find the name of the type of the entity arguments.
findEntityTyModName :: (OpType a) => Entity ty a -> String
findEntityTyModName e = nm
  where
    (Name nm _) = fn e undefined
    fn :: (OpType a) => c a -> Signal a -> Name
    fn _ s = s `op` ""
   
-------------------------------------------

--fun1 nm f s     = entity1 (op s nm) inputs f s
--fun2 nm f s1 s2 = entity2 (op s1 nm) inputs f s1 s2
inputs = [Var $ "i" ++ show (n :: Int) | n <- [0..]]

entity0 :: Name -> [Var] -> [[Ty Var]] -> a -> ESignal a
entity0 nm outs tyeqs f 
        = ESignal (pure f)
        $ E
        $ Entity nm outs [] tyeqs

entity1 :: Name -> [Var] -> [Var] -> [[Ty Var]] -> (a -> b) -> Signal a -> ESignal b
entity1 nm ins outs tyeqs f  s@(~(Signal vs1 w1)) 
        = ESignal (pure f <*> vs1)
        $ E
        $ Entity nm outs (zip ins [w1]) tyeqs
        
entity2 :: Name -> [Var] -> [Var] -> [[Ty Var]] -> (a -> b -> c) -> Signal a -> Signal b -> ESignal c
entity2 nm ins outs tyeqs f s@(~(Signal vs1 w1)) ~(Signal vs2 w2)
        = ESignal (pure f <*> vs1 <*> vs2)
        $ E
        $ Entity nm outs (zip ins [w1,w2]) tyeqs

entity3 :: Name -> [Var] -> [Var] -> [[Ty Var]] -> (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> ESignal d
entity3 nm ins outs tyeqs f  s@(~(Signal vs1 w1)) ~(Signal vs2 w2) ~(Signal vs3 w3)
        = ESignal (pure f <*> vs1 <*> vs2 <*> vs3)
        $ E
        $ Entity nm outs (zip ins [w1,w2,w3]) tyeqs

o0 :: ESignal a -> Signal a
o0 ~(ESignal v e) = Signal v (Port (Var "o0") e)

proj :: Var -> (a -> b) -> ESignal a -> Signal b
proj var f ~(ESignal v e) = Signal (fmap f v) (Port var e)

fun1 nm f s     = o0 $ entity1 (op s nm) inputs [Var "o0"] tyeqs f s
	where allNames = take 1 inputs ++ [Var "o0"]
	      tyeqs    = [ bitTypeOf s : map TyVar allNames ]
fun2 nm f s1 s2 = o0 $ entity2 (op s1 nm) inputs [Var "o0"] tyeqs f s1 s2
	where allNames = take 2 inputs ++ [Var "o0"]
	      tyeqs    = [ bitTypeOf s1 : map TyVar allNames]


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
  portsByType :: Signal e -> [String]           -- depends on the *type*, not the contents

class Explode e => Implode e where
  join :: Ex e -> Signal e

-- TODO: somehow wire in better names than o1 and o2.
instance Explode (a,b) where
  type Ex (a,b) = (Signal a,Signal b)
  explode ~(ESignal v w) =
        ( Signal (fmap fst v) $ Port (Var "o1") w
        , Signal (fmap snd v) $ Port (Var "o2") w
        )
  portsByType _ = ["1","2"] 


instance Implode (a,b) where
  join (s1,s2) = o0 $ entity2 (Name "$POLY" "join")
                           [Var "s1",Var "s2"]
                           [Var "o0"]
			   [] -- nothing in this type system
                           (,)
                           s1 s2

split :: (Explode e) => Signal e -> Ex e
split e = explode entity 
  where entity = entity1 (Name "$POLY" "split") 
                         [Var "i0"] 
                         (map Var (portsByType e))
			 []
                         id
                         e


-- Signal (a -> b) -> Signal a -> Signal b



{-
  implode (~(Signal v1 w1),~(Signal v2 w2)) =
        Signal ((,) <$> v1 <*> v2) $ Wire $ Entity (Name "$" "implode") [w1,w2]  
-}
{-
implode2 :: (Signal a, Signal b) -> Signal (a,b)
implode2 = implode
-}

peek :: Signal a -> [Maybe a]
peek ~(Signal xs _) = S.toList xs

-- A signal of a  constant value, for testing
be :: a -> Signal a
be e = Signal (pure e) undefined       -- for now

-- A signal, that changes over time
with :: [a] -> Signal a
with xs = poke (map Just xs)

-- A signal with possible unknown values, that changes over time.
poke :: [Maybe a] -> Signal a
poke xs = Signal (S.fromList xs) undefined



