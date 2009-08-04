module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative

high :: Signal Bool
high = Signal (pure True) $ Pad $ Var "high"

low :: Signal Bool
low = Signal (pure False) $ Pad $ Var "low"

-- entity3??
type U = Int

newtype U2 = U2 U

mux4 :: (MUX a) => Signal U -> a -> a -> a -> a -> a
mux4 u2 a b c d = mux2 b1 (mux2 b2 a b) (mux2 b2 c d)
   where
	b1 = bit 0 
	b2 = bit 1

class MUX a where
  mux2 :: Signal Bool -> a -> a -> a
  mux3 :: Signal Ordering -> a -> a -> a -> a
--  mux4 :: Signal (Bool,Bool) -> a -> a -> a -> a -> a
--   mux3 o a1 a2 a3 = (mux2 (o == EQ) a2) (mux2 (o == LT

instance (MUX a,MUX b) => MUX (a,b) where
   mux2 x (a,b) (a',b') = (mux2 x a a',mux2 x b b')
   mux3 x (a,b) (a',b') (a'',b'') = (mux3 x a a' a'', mux3 x b b' b'')

instance MUX (Signal a) where
  mux2 sC sT sF = 
        o0 $ entity3 (Name "Bool" "mux2")
              [Var "c",Var "t", Var "f"]
              [Var "o0"]
	      [ [TyVar $ Var "c", B ]
	      , [TyVar $ Var "t",TyVar $ Var "f",TyVar $ Var "o0"]
	      ]
              (\ a b c -> if a then b else c)
              sC sT sF
{-
  mux3 sC sLT sEQ sGT = 
        o0 $ entity4 (Name "Bool" "mux3")
              [Var "c",Var "lt", Var "eq", Var "gt"]
              [Var "o0"]
	      [ [TyVar $ Var "c", U 2 ]
	      , [TyVar $ Var "t",TyVar $ Var "f",TyVar $ Var "o0"]
	      ]
              (\ a b c -> if a then b else c)
              sC sT sF
-}

cases :: (MUX cir) => [(Signal Bool,cir)] -> cir -> cir
cases []           def = def
cases ((b,c):rest) def = mux2 b c (cases rest def)


data O = X | Y

jk_ff :: Signal Bool -> Signal Int -> (Signal Int, Signal Bool)
jk_ff a b = (proj (Var "x") fst e,proj (Var "y") snd e)
  where
	e :: ESignal (Int,Bool)
	e = entity2 (Name "JK" "FF")
		   [Var "j",Var "k"]
		   [Var "x",Var "y"]
		   [ B : [ TyVar $ Var n | n <- words "j y" ]
		   , S 32 : [ TyVar $ Var n | n <- words "k x" ]
		   ]
		   (\ a b -> (b,a))
		   a b
		





	