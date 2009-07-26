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
import Language.KansasLava.Seq
import Data.Monoid
import Debug.Trace

newtype Signal a = Signal Wire 
--	      | forall b . Fmap (a -> b) (Signal a)

{-
-}
{-
 fmap (a -> b) -> Signal a -> Signal b	-- This is more like a witness.
 pure :: a -> Signal a			-- 
 <*> :: Signal (a -> b) -> Signal a -> Signal b

-- These would be constructors, in the EDDSL.
pair :: (Signal a,Signal b) -> Signal (a,b)
pair (s1,s2) = fmap (,) <*> s1 <*> s2
proj :: Signal (a,b) -> (Signal a,Signal b)
proj s = (fmap fst s,fmap snd s)

arr :: Array b (Signal a) -> Signal (Array b a)
arr = 

proj :: Signal (Array b a) -> Array b (Signal a)

extract :: Signal a -> a
extract :: Signa

class Functor f where
  fmap :: (Dynamic a,Dynamic b) => (a -> b) -> Signal a -> Signal b


-}




-- All possible

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

instance (Num a, OpType a) => Num (Signal a) where
    s@(Signal s1) + (Signal s2) = Signal $ Wire $ Entity (op s "+")      [s1,s2]
    s@(Signal s1) - (Signal s2) = Signal $ Wire $ Entity (op s "-")      [s1,s2]
    s@(Signal s1) * (Signal s2) = Signal $ Wire $ Entity (op s "*")      [s1,s2]
    negate s@(Signal s1)        = Signal $ Wire $ Entity (op s "negate") [s1]
    abs s@(Signal s1)           = Signal $ Wire $ Entity (op s "abs")    [s1]
    signum s@(Signal s1)        = Signal $ Wire $ Entity (op s "signum") [s1]
    fromInteger n               = s
            where s = Signal $ Wire $ Entity (op s "fromInteger")
                                    [ Wire $ Lit $ n
                                    ]
        


instance (Bits a, OpType a) => Bits (Signal a) where
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

{-
class Sig (sig :: * -> *) where 
   nothing :: sig a -> sig a
-}
 
class SEQ (seq :: *) where 
   delay :: seq -> seq -> seq
   
instance SEQ (Signal a) where {}

class BOOL (bool :: *) where 
   high :: bool
   low  :: bool
   inv  :: bool -> bool

instance BOOL (Signal a) where {}
instance BOOL Bool where {}

-- instance (SEQ seq) => BOOL (seq Bool) where {}
  
-- should conditionals imply SEQ, because they only make sense 
-- if run may times. 
class BOOL bool => COND bool a {- | a -> bool -} where 
  iF :: cond -> a -> a -> a

instance COND (Signal Bool) (Signal a) where {}
instance COND Bool Float where {}
instance COND Bool Int where {}

  
   
-- instance Sig Signal where {}

-- hacks
-- instance (Sig sig, Show (sig Int), Eq (sig Int)) => Num (sig Int) where  {}
--instance (Sig sig) => Show (sig Int) where  {}
--instance (Sig sig) => Eq (sig Int) where  {}

