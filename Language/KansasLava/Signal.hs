{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}

module Language.KansasLava.Signal where

import Data.Ratio
import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Data.Reify
import qualified Data.Traversable as T

import Language.KansasLava.Entity
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
{-
evaluateNumClass :: (Num a, OpType a) => Entity a -> Maybe a
evaluateNumClass entity = case entity of
        Entity (Name nm "+") [v1,v2] | nm == nm' -> return $ v1 + v2
        Entity (Name nm "-") [v1,v2] | nm == nm' -> return $ v1 - v2
        Entity (Name nm "*") [v1,v2] | nm == nm' -> return $ v1 * v2
        _ -> Nothing
  where
          nm' = findEntityTyModName entity
-}

evaluateNumClass :: (Num a, Show a) => String -> Maybe ([a] -> a)
-- evaluateNumClass op | trace (show op) False = undefined
evaluateNumClass "+"      = return $ \ [v1,v2] -> v1 + v2
evaluateNumClass "-"      = return $ \ [v1,v2] -> v1 - v2
evaluateNumClass "*"      = return $ \ [v1,v2] -> v1 * v2
evaluateNumClass "negate" = return $ \ [v1] -> negate v1 
evaluateNumClass "abs"    = return $ \ [v1] -> abs v1
evaluateNumClass "signum" = return $ \ [v1] -> signum v1 
evaluateNumClass _   = fail "not in evaluateNum"

evalNumClass :: (Num a, OpType a) => Eval a
evalNumClass = Eval $ \ entity ->
    let modName = findEntityTyModName entity in
    case entity of
        (Entity (Name nm' op') _) | nm' == modName -> evaluateNumClass op'
        _ -> Nothing
        

data Seq a = a :~ Seq a
newtype Eval a = Eval (Entity a -> Maybe ([a] -> a))

repeatSeq :: a -> Seq a
repeatSeq a = a :~ repeatSeq a

transposeSeq :: [Seq a] -> Seq [a]

transposeSeq ((x :~ xs) : xss) = 
        (x : [h | (h:~t) <- xss]) :~ 
        transposeSeq (xs : [t | (h:~t) <- xss])
                           
instance Functor Seq where
   fmap f (a :~ as) = f a :~ fmap f as

-- This is *really* gunky, but works.
liftEntityEval :: Eval a -> Eval (Seq a)
liftEntityEval (Eval fn) = Eval $ \ entity ->
   case fn (demote entity) of
      Nothing -> Nothing
      Just fn' -> Just $ \ vs -> case vs of
         [] -> repeatSeq (fn' []) 
         _  -> fmap fn' (transposeSeq vs)
 where
    demote :: Entity (Seq a) -> Entity a
    demote (Entity nm _) = Entity nm []
    demote (Port nm v)   = Port nm (error "port problem") 
    demote (Pad pd)      = Pad pd
    demote (Lit i)       = Lit i

instance Monoid (Eval a) where
    mempty = Eval $ \ _ -> Nothing
    mappend (Eval f1) (Eval f2) = Eval $ \ e ->
        case f1 e of
          Nothing -> f2 e
          Just fn -> Just fn


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
            where s = Signal $ Wire $ Entity (op s "%")
                                    [ Wire $ Lit $ numerator r
                                    , Wire $ Lit $ denominator r
                                    ]


instance (Floating a, OpType a) => Floating (Signal a) where
    pi 				= s where s = Signal $ Wire $ Pad (op s "pi")
    exp s@(Signal s1)         = Signal $ Wire $ Entity (op s "recip") [s1]
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

