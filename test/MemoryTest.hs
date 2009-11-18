{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}
module MemoryTest where

import Language.KansasLava
import Language.KansasLava.Memory

import Data.Sized.Ix
import Data.Sized.Unsigned

import Control.Applicative
type Unsigned2 = Unsigned X2

-- FIXME: Remove and pull in Ed's version from Data.Sized
instance (Enum s, Size s) => Bounded (Unsigned s) where
  minBound = 0
  maxBound = let a :: s
                 a = error "making a s"
             -- had to use -fglasgow-exts. Don't know which extension is needed.
             in fromIntegral $ 2 ^ (size a)  - 1
{-
test = permute p clock testvector
  where p :: [(Unsigned2,Unsigned2)]
        p = zip [0..3] ( reverse [0..3])





toggle clk i = res
  where res = xor2 (delay clk low res) i


testvector = trajectory clock (map fromInteger [0..10])
-}

trajectory :: (OpType a) => Time -> [Signal a] -> Signal a
trajectory clk l = foldr c initVal l
  where c a as = delay clk a as

delayN n clk input = foldr ($) input (replicate n (delay clk initVal))

--permute :: (Num a, OpType a) => [(a,a)] -> Time -> Signal Bool -> Signal Bool

permute ::
  (OpType d, Ord d, Num d, Enum d, Bounded d) =>
  [(d, d)] -> Time -> Signal Int -> Signal Int
permute permutation clk input = out
  where out = mux2 toggle_z_zz bufA bufB
        addr = counter clk
        permRead = readMem addr

        rom = bram permutation clk permRead

        writeReq = writeMem rom input_zz
        readReq = readMem addr_zz

        muxA = mux2 toggle_z readReq writeReq
        muxB = mux2 toggle_z writeReq readReq

        bufA = bram initBuf clk muxA
        bufB = bram initBuf clk muxB
        initBuf = [(i,0) | i <- [minBound..maxBound]]
        toggle = delay clk high (toggle `xor2` overflow)
           where overflow = (addr .==. 0)

        addr_zz = delayN 2 clk addr
        input_zz = delayN 2 clk input
        -- the 'toggle' has a built-in 1-cycle delay, so we only delay 1 cycle
        toggle_z = delayN 1 clk toggle
        toggle_z_zz = delayN 2 clk toggle_z



counter :: (OpType a, Num a) => Time -> Signal a
counter clk = out
  where out = delay clk 0 out'
        out' =  1 + out


p1 :: [(Unsigned X2, Unsigned X2)]
p1 = [(0,3),(1,2),(2,1),(3,0)]


