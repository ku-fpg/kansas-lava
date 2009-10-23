module MemoryTest where

import Language.KansasLava
import Language.KansasLava.Memory

import Data.Sized.Ix
import Data.Sized.Unsigned

import Control.Applicative
type Unsigned2 = Unsigned X2

test = permute p clock testvector
  where p :: [(Unsigned2,Unsigned2)]
        p = zip [0..3] ( reverse [0..3])


permute :: [(Unsigned2,Unsigned2)] -> Time -> Signal Unsigned2 -> Signal (Maybe Unsigned2)
permute p clk i = bufferRAM address
  where count = counter clk
        writing = delayN 2 clk $ toggle clk (fmap (==0) count)

        permutationROM = bram p clk
        addressMaybe = permutationROM (readMem count)
        bufferRAM = bram [] clk


        idelayed = delayN 2 clk i
        address = merge <$>  writing <*> addressMaybe <*> idelayed <*> (delayN 2 clk count)

        -- First parameter is whether we are writing
        -- Had to export R/W constructors, since we're using an a.f., and readMem/writeMem generate signals.
        merge :: Bool -> Maybe addr -> dat -> addr -> MemOp addr dat
        merge False _ _ i = R i
        merge True (Just addr) dat _ = W addr dat
        merge True Nothing _ i = R i

        counter clk = out
          where out :: Signal Unsigned2
                out = delay clk 0 out'
                out' =  1 + out

delayN n clock input = foldr ($) input (replicate n (delay clock initVal))

toggle clk i = res
  where res = xor2 (delay clk low res) i

trajectory clk l = foldr c 0 l
  where c a as = delay clk a as




testvector = trajectory clock (map fromInteger [0..10])
