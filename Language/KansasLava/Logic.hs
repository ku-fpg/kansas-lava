{-# LANGUAGE TypeFamilies, RankNTypes, ScopedTypeVariables #-}
-- | The 'Logic' module provides basic combinational circuit combinators.

module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative
import Data.Sized.Matrix as M hiding (length, zipWith)
import Data.Sized.Unsigned as U

high :: Signal Bool
high = Signal (pure True) $ Lit 1

low :: Signal Bool
low = Signal (pure False) $ Lit 0


mux4 :: (MUX a) => Signal U2 -> a -> a -> a -> a -> a
mux4 u2 a b c d = mux2 b1 (mux2 b2 a b) (mux2 b2 c d)
   where
	b1 = testABit u2 0
	b2 = testABit u2 1
{-
class BROADWAY a where
--  type LANE a
  promote :: (forall x . (BROADWAY x) => x -> x -> x) -> a -> a -> a

instance BROADWAY (Signal a) where
  promote f = f

-- mux and delay can use this.
instance (BROADWAY a,BROADWAY b) => BROADWAY (a,b) where
  promote f (a,b) (a',b') = (f a a',f b b')
-}
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

instance OpType a => MUX (Signal a) where
  mux2 sC@(~(Signal b _)) sT@(~(Signal t _)) sF@(~(Signal f _)) =
	clone (Signal (seqMux b t f) (error "bad entity for mux"))
              (o0 $ entity3 (Name "Bool" "mux2")
              [Var "c",Var "t", Var "f"]
              [Var "o0"]
	      (error "mux functionality misunderstood")
--              (\ a b c -> if a then b else c)
              sC sT sF)

  mux3 _ _ _ = error "undefined method mux3 in MUX (Signal a)"

instance MUX a => MUX [a] where
  mux2 b ts es = zipWith (mux2 b) ts es
  mux3 b a1 a2 a3 = zipWith3  (mux3 b) a1 a2 a3

instance (Size ix, OpType a) => MUX (M.Matrix ix (Signal a))  where
  mux2 b ts es = M.fromList $ zipWith (mux2 b) (M.toList ts) (M.toList es)
  mux3 b m1 m2 m3 = M.fromList $ zipWith3 (mux3 b) (M.toList m1) (M.toList m2) (M.toList m3)



-- Selects an arbitrary element of a list
-- The selector has type, [Signal Bool]
-- is a binary representation of the unsigned index to select,
-- with the leftmost (first) element most significant.
--
-- The list is indexed: 0-based, left to right 
-- 
-- If (2 ** (length selector)) < (length inputlist)
--     not all elements of the list are selectable.
-- If (2 ** (length selector)) > (length inputlist)
--     the output for selector "value" >= (length inputlist) is 
--     not defined.
anyMux :: (MUX a) => [Signal Bool] -> [a] -> a
anyMux [s] [a0] = a0
anyMux [s] [a0, a1] = mux2 s  a1 a0
anyMux sel@(s:rest) as = if (aLength <= halfRange) 
                         then anyMux sel' as
                         else if (aLength > maxRange) 
                              then anyMux sel as' 
                              else anyMux'
    where aLength = length as
          halfRange = 2 ^ ((length sel) -1)
          maxRange = 2 * halfRange
          nselbits = max 1 (ceiling (logBase 2 (fromIntegral aLength)))
          sel' = drop ((length sel) - nselbits) sel
          as' = take  maxRange as
          -- anyMux' knows that the number of selection bits matches range input choices
          anyMux' = mux2 s (anyMux topSelect top) (anyMux rest bottom)
          (bottom, top) = splitAt halfRange as
          topLen = fromIntegral $ length top
          nbits = max 1 (ceiling (logBase 2 topLen))
          topSelect = drop ((length rest) - nbits) rest


-- matrixMux allows one to select an element of the Matrix at an arbitrary index
-- The selector has type, [Signal Bool]
-- is a binary representation of the unsigned index to select,
-- with the leftmost (first) element most significant.
-- See 'anyMux' above for additional details.
matrixMux :: (MUX a, Size ix) => [Signal Bool] -> M.Matrix ix a -> a
matrixMux sel mat = anyMux sel (M.toList mat)

cases :: (MUX cir) => [(Signal Bool,cir)] -> cir -> cir
cases []           def = def
cases ((b,c):rest) def = mux2 b c (cases rest def)

-- Misc Bool signal things.
-- We do not overload Bool at Bits because
-- Bool is not a Num, and correctly so. We use U1 instead.

--	    (Sized ix) => Signal a -> ix -> Signal Bool
testABit :: (Bits a, OpType a) => Signal a -> Int -> Signal Bool
testABit x y = o0 $ entity1 (Name "Bits" "testABit") inputs [Var "o0"] (\ x' -> testBit x' y) x
	where inputs   = take 2 defaultInputs
and2 :: Signal Bool -> Signal Bool -> Signal Bool
and2 x y = o0 $ entity2 (Name "Bool" "and2") inputs [Var "o0"] (&&) x y
	where inputs   = take 2 defaultInputs
or2 :: Signal Bool -> Signal Bool -> Signal Bool
or2 x y = o0 $ entity2 (Name "Bool" "or2") inputs [Var "o0"] (||) x y
        where inputs = take 2 defaultInputs

xor2 ::  Signal Bool -> Signal Bool -> Signal Bool
xor2 x y = o0 $ entity2 (Name "Bool" "xor2") inputs [Var "o0"] (/=) x y
	where inputs   = take 2 defaultInputs
bitNot :: Signal Bool -> Signal Bool
bitNot x = o0 $ entity1 (Name "Bool" "not") inputs [Var "o0"]  not x
	where inputs   = take 1 defaultInputs
