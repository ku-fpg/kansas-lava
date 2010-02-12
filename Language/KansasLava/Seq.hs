{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Seq where

import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Type
import Language.KansasLava.Signal

import Language.KansasLava.Entity
import Language.KansasLava.Stream as S

import Control.Applicative
import Data.Sized.Unsigned as UNSIGNED
import Data.Sized.Signed as SIGNED
import Data.Sized.Sampled as SAMPLED
import Data.Sized.Arith as Arith
import Data.Sized.Ix as X

import Language.KansasLava.Comb
import Language.KansasLava.Wire

-----------------------------------------------------------------------------------------------

data Seq a = Seq (Stream (X a)) (D a)

seqValue :: Seq a -> Stream (X a)
seqValue (Seq a d) = a

seqDriver :: Seq a -> D a
seqDriver (Seq a d) = d

instance forall a . (RepWire a, Show a) => Show (Seq a) where
	show (Seq vs _)
         	= unwords [ showRepWire (undefined :: a) x ++ " :~ "
                          | x <- take 20 $ toList vs
                          ] ++ "..."

instance forall a . (Wire a, Eq a) => Eq (Seq a) where
	-- Silly question; never True; can be False.
	(Seq x _) == (Seq y _) = error "undefined: Eq over a Seq"
	
deepSeq :: D a -> Seq a
deepSeq d = Seq (error "incorrect use of shallow Seq") d

shallowSeq :: Stream (X a) -> Seq a
shallowSeq s = Seq s (error "incorrect use of deep Seq")

instance Signal Seq where
  liftS0 ~(Comb a e) = Seq (pure a) e

  liftS1 f ~(Seq a ea) = Seq (fmap f' a) eb
      where
	Comb _ eb = f (deepComb ea)
	f' a = let ~(Comb b _) = f (shallowComb a) 
	       in b

  -- We can not replace this with a version that uses packing,
  -- because *this* function is used by the pack/unpack stuff.
  liftS2 f ~(Seq a ea) ~(Seq b eb) = Seq (S.zipWith f' a b) ec
      where
	Comb _ ec = f (deepComb ea) (deepComb eb)
	f' a b = let ~(Comb c _) = f (shallowComb a) (shallowComb b) 
	         in c

----------------------------------------------------------------------------------------------------

-- Small DSL's for declaring signals

toSeq :: (Wire a) => [a] -> Seq a
toSeq xs = shallowSeq (S.fromList (map optX (map Just xs ++ repeat Nothing)))

encSeq :: (Wire a) =>  (Char -> Maybe a) -> String -> Seq a
encSeq enc xs = shallowSeq (S.fromList (map optX (map enc xs ++ repeat Nothing)))

encSeqBool :: String -> Seq Bool
encSeqBool = encSeq enc 
	where enc 'H' = return True
	      enc 'L' = return False
	      enc  _   = Nothing

