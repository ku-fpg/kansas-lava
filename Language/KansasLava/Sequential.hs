module Language.KansasLava.Sequential where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Seq as Seq
import Control.Applicative
import Language.KansasLava.Applicative

--delay :: (Sig sig) => sig a -> sig a -> sig a
-- delay = error "delay!"

-- Should this be a -> S a -> S a ??
delay :: Signal Time -> Signal a -> Signal a -> Signal a
delay ~(Signal tm tm_w) ~(Signal d def) ~(Signal rest w) 
        = Signal (shallowDelay tm d rest)
        $ Port (Var "o")
        $ E
        $ Entity (Name "Lava" "delay") [Var "o"] [(Var "t",tm_w), (Var "init",def),(Var "i",w)]
			[[ TyVar $ Var v | v <- ["o","init","i"]]]

shallowDelay :: Seq Time -> Seq a -> Seq a -> Seq a
shallowDelay sT sInit input =
	pure (\ reset a b -> if reset then a else b)
		<*> reset_delayed
		<*> sInit
		<*> input_delayed
   where
	input_delayed = Nothing :~ input
	reset_delayed = Nothing :~ ((\ (Time t) -> t == -1) <$> sT)


data Time = Time Integer 		-- local clocks
					-- 0 == reset signal
					-- 1 == first cycle
					-- 2 == second cycle

instance Show Time where
	show (Time t) = show t


time :: Signal Time -> Signal Integer		-- simluation only
time = fmap (\ (Time t) -> t)


-- 'clock' gives 2 cycles of bla, 1 cycle of reset, then counts from 0.
clock :: Signal Time
clock = Signal clock_times (Port (Var "o0") $ E $ Entity (Name "Lava" "clock") [] [] [[TyVar $ Var "o0",BaseTy T]])
  where clock_times = Nothing :~ Nothing :~ (Seq.fromList $ map Just $ map Time (-1 : [0..]))

reset :: Signal Time 
reset = Signal (pure (Time (-1))) (Port (Var "o0") $ E $ Entity (Name "Lava" "clock") [] [] [[TyVar $ Var "o0",BaseTy T]])

switch :: Int -> Signal a -> Signal a -> Signal a
switch n (Signal s1 _) (Signal s2 _) 
  = Signal (pure (\ s1 s2 i -> if i < n then s1 else s2) 
		<*> s1
		<*> s2
		<*> (Seq.fromList [ Just i | i <- [0..]])
	   ) undefined
	


instance OpType Time    
  where op _ nm = Name "Time" nm 
	bitTypeOf _ = T
	initVal = error "can not use Signal Time as an init value"

