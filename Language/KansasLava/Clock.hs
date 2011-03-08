{-# LANGUAGE RankNTypes, TypeFamilies, ScopedTypeVariables #-}
-- | The 'Clock' module provides a utility function for simulating clock rate
-- downsampling.
module Language.KansasLava.Clock(rate) where

import Data.Ratio

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Ix

import Language.KansasLava.RTL
import Language.KansasLava.Utils
import Language.KansasLava.Types
import Language.KansasLava.Seq

-- | 'rate' constructs a stream of enable bits used for clock-rate
-- downsampling. For example, with a rate of n=1/2, every other value in the
-- output stream will be True. If 1/n is not a integer, then the function uses
-- http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm to approximate the
-- given rate.
rate :: forall x clk . (Clock clk, Size x) => Witness x -> Rational -> CSeq clk Bool
rate Witness n
  | step * 2 > 2^sz = error $ "bit-size " ++ show sz ++ " too small for punctuate Witness " ++ show n
  | n <= 0 = error "can not have rate less than or equal zero"
  | n > 1 = error $ "can not have rate greater than 1, requesting " ++ show n
  | otherwise = runRTL $ do
	count <- newReg (0 :: (Unsigned x))
	cut   <- newReg (0 :: (Unsigned x))
	err   <- newReg (0  :: (Signed x))
	CASE [ IF (reg count .<. (fromIntegral step + reg cut - 1)) $
		  count := reg count + 1
	     , OTHERWISE $ do
		  count := 0
		  CASE [ IF (reg err .>=. 0) $ do
		            cut := 1
			    err   := reg err + fromIntegral nerr
		        , OTHERWISE $ do
		            cut := 0
			    err   := reg err + fromIntegral perr
			]

	     ]
	return  (reg count .==. 0)

   where sz :: Integer
         sz = fromIntegral (size (error "witness" :: x))
	 num = numerator n
	 dom = denominator n
	 step = floor (1 / n)
	 perr = dom - step       * num
	 nerr = dom - (step + 1) * num


