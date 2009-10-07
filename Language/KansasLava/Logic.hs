{-# LANGUAGE TypeFamilies, RankNTypes #-}

module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative
import Data.Sized.Unsigned as U
import Data.Sized.Ix as X

high :: Signal Bool
high = Signal (pure True) $ Pad $ Var "high"

low :: Signal Bool
low = Signal (pure False) $ Pad $ Var "low"

type U2 = U.Unsigned X2

mux4 :: (MUX a) => Signal U2 -> a -> a -> a -> a -> a
mux4 u2 a b c d = mux2 b1 (mux2 b2 a b) (mux2 b2 c d)
   where
	b1 = testABit u2 0 
	b2 = testABit u2 1

class BROADWAY a where
--  type LANE a
  promote :: (forall x . (BROADWAY x) => x -> x -> x) -> a -> a -> a

instance BROADWAY (Signal a) where
  promote f = f

-- mux and delay can use this.
instance (BROADWAY a,BROADWAY b) => BROADWAY (a,b) where
  promote f (a,b) (a',b') = (f a a',f b b')

{-
instance (BROADWAY a) => BROADWAY [a] where
  promote f (x:xs) (y:ys) = promote f x : [] -- promote f xs
  promote f []     [] = []
-}

class MUX a where
  mux2 :: Signal Bool -> a -> a -> a
  mux3 :: Signal Ordering -> a -> a -> a -> a

--  mux4 :: Signal (Bool,Bool) -> a -> a -> a -> a -> a
--   mux3 o a1 a2 a3 = (mux2 (o == EQ) a2) (mux2 (o == LT

instance (MUX a,MUX b) => MUX (a,b) where
   mux2 x (a,b) (a',b') = (mux2 x a a',mux2 x b b')
   mux3 x (a,b) (a',b') (a'',b'') = (mux3 x a a' a'', mux3 x b b' b'')

instance MUX (Signal a) where
  mux2 sC@(~(Signal b _)) sT@(~(Signal t _)) sF@(~(Signal f _)) = 
	clone (Signal (seqMux b t f) (error "bad entity for mux"))
              (o0 $ entity3 (Name "Bool" "mux2")
              [Var "c",Var "t", Var "f"]
              [Var "o0"]
	      [ [TyVar $ Var "c", BaseTy B ]
	      , [TyVar $ Var "t",TyVar $ Var "f",TyVar $ Var "o0"]
	      ]
	      (error "mux functionality misunderstood")
--              (\ a b c -> if a then b else c)
              sC sT sF)

cases :: (MUX cir) => [(Signal Bool,cir)] -> cir -> cir
cases []           def = def
cases ((b,c):rest) def = mux2 b c (cases rest def)

-- Misc Bool signal things. 
-- We do not overload Bool at Bits because
-- Bool is not a Num, and correctly so. We use U1 instead.

testABit :: (Bits a) => a -> Int -> Signal Bool
testABit = undefined

and2 :: Signal Bool -> Signal Bool -> Signal Bool
and2 x y = o0 $ entity2 (Name "Bool" "and2") inputs [Var "o0"] tyeqs (&&) x y
	where allNames = inputs ++ [Var "o0"]
	      tyeqs    = [ BaseTy B : map TyVar allNames ]
	      inputs   = map Var ["i0","i1"]
or2 :: Signal Bool -> Signal Bool -> Signal Bool
or2 x y = o0 $ entity2 (Name "Bool" "or2") inputs [Var "o0"] tyeqs (||) x y
	where allNames = inputs ++ [Var "o0"]
	      tyeqs    = [ BaseTy B : map TyVar allNames ]
	      inputs   = map Var ["i0","i1"]

xor2 ::  Signal Bool -> Signal Bool -> Signal Bool
xor2 x y = o0 $ entity2 (Name "Bool" "xor2") inputs [Var "o0"] tyeqs (/=) x y
	where allNames = inputs ++ [Var "o0"]
	      tyeqs    = [ BaseTy B : map TyVar allNames ]
	      inputs   = map Var ["i0","i1"]
bitNot :: Signal Bool -> Signal Bool
bitNot x = o0 $ entity1 (Name "Bool" "not") inputs [Var "o0"] tyeqs not x 
	where allNames = inputs ++ [Var "o0"]
	      tyeqs    = [ BaseTy B : map TyVar allNames ]
	      inputs   = map Var ["i0"]
