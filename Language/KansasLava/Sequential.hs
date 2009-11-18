{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Language.KansasLava.Sequential where

import Language.KansasLava.Entity
import Language.KansasLava.IO
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Seq as Seq
import Control.Applicative
import Language.KansasLava.Applicative

--delay :: (Sig sig) => sig a -> sig a -> sig a
-- delay = error "delay!"

-- To revisit: Should this be a -> S a -> S a ??

delay :: forall a. (OpType a) => Time -> Signal a -> Signal a -> Signal a
delay ~(Time ~(Signal tm tm_w) ~(Signal r r_w)) ~(Signal d def) ~(Signal rest w)
        = Signal (shallowDelay r d rest)
        $ Port (Var "o0")
        $ E
        $ Entity (Name "Lava" "delay") [(Var "o0",aTy)]
            [(Var "clk",clkTy,tm_w), (Var "rst", rstTy,r_w), (Var "init",aTy,def),(Var "i",aTy,w)] []
  where aTy = tyRep (error "delay/aTy" ::  a)
        clkTy = tyRep (error "delay/clk" :: Clk)
        rstTy = tyRep (error "delay/rst" :: Rst)


shallowDelay :: Seq Rst -> Seq a -> Seq a -> Seq a
shallowDelay sT sInit input =
	seqMux (fmap (\ (Rst reset) -> reset) reset_delayed)
	       sInit
	       input_delayed
   where
	input_delayed = Nothing :~ input
	reset_delayed = sT -- Just (Rst False) :~ sT


data Time = Time
		(Signal Clk)		-- threading clock
		(Signal Rst)		-- threading reset

newtype Clk = Clk Integer
newtype Rst = Rst Bool

instance Show Time where
	-- show (Time t r) = show (pure (,) <*> t <*> r)
        show (Time (Signal t_s _) (Signal r_s _)) = show $ zipWith' (,) t_s r_s

instance REIFY Time where
--	capture p (a,b) = capture (p `w` 1) a ++ capture (p `w` 2) b
	create     = Time <$> create <*> create

	-- create = Time <$> named "clk" <*> named "rst"

--	capture'' (a,b) = (++) <$> capture'' a <*> capture'' b
        capture'' = error "No method nor default method for `capture''' in the instance declaration for `REIFY Time'"

instance Show Clk where
	show (Clk n) = show n

instance OpType Clk
  where op _ nm = Name "Time" nm
	bitTypeOf _ = ClkTy
	initVal = error "can not use a clock as an init value"

instance Show Rst where
	show (Rst n) = show n

instance OpType Rst
  where op _ nm = Name "Time" nm
	bitTypeOf _ = RstTy
	initVal = error "can not use a reset as an init value"



-- newtype Clk = Clk Integer	-- always running

clk :: Time -> Signal Integer
clk (Time c _) = error "Sequential.clk" -- fmap (\ (Clk n) -> n) c

rst :: Time -> Signal Bool
rst (Time _ r) = error "Sequential.rst"  -- fmap (\ (Rst r') -> r') r

{-
time :: Time -> Signal Integer		-- simluation only
time (Time t) = t
-}

-- 'clock' gives 2 cycles of bla, 1 cycle of reset, then counts from 0.
clock :: Time
clock = Time (with "global_clock" $ map Clk [0..])
	     (with "global_reset" $ map Rst (True:(repeat False)))

waitFor :: (OpType a) => Int -> Signal a -> Signal a
waitFor n ~(Signal s _) = Signal (fromList (take n (repeat Nothing) ++ toList s)) (error "bad entity")

-- waitForReset ::

{-

	Time $ Signal clock_times (Port (Var "o0") $ E $ Entity (Name "Lava" "clock") [] [] [[TyVar $ Var "o0",BaseTy T]])
  where clock_times = Nothing :~ Nothing :~ (Seq.fromList $ map Just $ (-1 : [0..]))
-}

{-
reset :: Time
reset = Time $ Signal (pure (-1)) (Port (Var "o0") $ E $ Entity (Name "Lava" "clock") [] [] [[TyVar $ Var "o0",BaseTy T]])

switch :: Int -> Signal a -> Signal a -> Signal a
switch n (Signal s1 _) (Signal s2 _)
  = Signal (pure (\ s1 s2 i -> if i < n then s1 else s2)
		<*> s1
		<*> s2
		<*> (Seq.fromList [ Just i | i <- [0..]])
	   ) undefined
-}

{-
-}

