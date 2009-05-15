{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Language.KansasLava where
    
import Data.Reify
import Control.Applicative 
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Ratio
import Data.Word
import Data.Int
import Data.Bits

--------------------------------------------------------

data Var = Var [String]
    deriving (Show, Eq)
    
var :: String -> Var
var v = Var [v]

--------------------------------------------------------

newtype Signal a = Signal Wire 

newtype Wire = Wire (Entity Wire) 

data Entity s = Entity Var [s]      -- an entitiy
              | Port Var s          -- get a specific port of an entity
              | Pad Var
              | Lit Integer
              deriving (Show, Eq)

--------------------------------------------------------

instance MuRef Wire where 
  type DeRef Wire = Entity
  mapDeRef f (Wire s) = T.traverse f s 
 
instance T.Traversable Entity where
  traverse f (Entity v ss) = Entity v <$> T.traverse f ss
  traverse f (Port v s)    = Port v <$> f s
  traverse f (Pad v)       = pure $ Pad v
  traverse f (Lit i)       = pure $ Lit i
  
instance F.Foldable Entity where
    
instance Functor Entity where
    fmap f (Entity v ss) = Entity v (fmap f ss)
    fmap f (Port v s)    = Port v (f s)
    fmap f (Pad v)       = Pad v
    fmap f (Lit i)       = Lit i

--------------------------------------------------------

and2 :: (Signal a, Signal a) -> Signal a
and2 (Signal w1,Signal w2) = Signal $ Wire $ Entity (var "and2") [w1,w2]

high = Signal $ Wire $ Pad $ var "high"
low  = Signal $ Wire $ Pad $ var "low"

class OpType a where
    op :: Signal a -> String -> Var
    signalOf :: Signal a -> a
    signalOf = undefined
    
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

instance Eq (Signal a) where
   (Signal s1) == (Signal s2) = s1 == s2

instance Eq Wire where
   (Wire s1) == (Wire s2) = s1 == s2

instance Show (Signal a) where
    show (Signal s) = show s

instance Show Wire where
    show (Wire s) = show s
    
--------------------------------------------------------

instance OpType Int    where op _ nm = Var $ ["Int",nm]
instance OpType Float  where op _ nm = Var $ ["Float",nm]
instance OpType Double where op _ nm = Var $ ["Double",nm]

instance OpType Int32 where op _ nm = Var $ ["Int32",nm]
instance OpType Word32 where op _ nm = Var $ ["Word32",nm]

-- Grab a signal, give me a graph, please.

reifyCircuit :: Signal a -> IO (Graph Entity)
reifyCircuit (Signal w) = reifyGraph w



