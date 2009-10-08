{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
-- This file neededs removed.

import Language.KansasLava

import Data.Int
import Debug.Trace
import Data.List
import Data.Monoid

import Prelude hiding (abs)

data V = VInt32 Int32
       | Pair V V

instance (V ~ Rep a, V ~ Rep b, Value a,Value b) => Value (a,b) where
   type Rep (a,b) = V
   rep (a,b) = Pair (rep a) (rep b)
   abs (Pair a b) = (abs a,abs b)


instance Value Int32 where
   type Rep Int32 = V
   rep = VInt32
   abs (VInt32 i) = i

test :: IO ()
test = do
        f <- evaluateCircuit circuit entityEval
        v0 <- f (2,3)
        print v0
        v1 <- f (1,4)
        print v1
        v2 <- f (3,5)
        print v2

entityEval :: Eval (Seq V)
entityEval = liftEntityEval $ mconcat
                [ liftToRep e1
                , primEval
                ]
 where
         e1 :: Eval Int32
         e1 = evalNumClass
        
{-
liftToRep :: (Value a) => Eval a -> Eval (Rep a)
liftToRep (Eval fn) = Eval $ \ entity ->
   case fn (demote entity) of
      Nothing -> Nothing
      Just fn' -> Just $ \ vs -> rep $ fn' (map abs vs)
 where
    demote :: (Value a) => Entity (Rep a) -> Entity a
    demote = fmap abs
-}

circuit :: Signal (Int32,Int32) -> Signal (Int32,Int32)
circuit ab = join (a + b,a - b)
   where
      (a,b) = split ab

join :: (Signal a, Signal b) -> Signal (a,b)
join (Signal s1,Signal s2) = Signal $ Wire $ Entity (Name "PRIM" "join") [s1,s2]
split :: Signal (a,b) -> (Signal a,Signal b)
split (Signal s) = 
        ( Signal $ Wire $ Entity (Name "PRIM" "fst") [s]
        , Signal $ Wire $ Entity (Name "PRIM" "snd") [s]
        )


primEval :: Eval V
primEval = Eval $ \ e ->
   case e of
     Entity (Name "PRIM" "join") _ -> return $ \ [v1,v2] -> Pair v1 v2
     Entity (Name "PRIM" "fst") _ -> return $ \ [Pair v1 v2] -> v1
     Entity (Name "PRIM" "snd") _ -> return $ \ [Pair v1 v2] -> v2
     _ -> Nothing 
