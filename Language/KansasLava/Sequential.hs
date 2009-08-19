module Language.KansasLava.Sequential where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Seq as Seq


--delay :: (Sig sig) => sig a -> sig a -> sig a
-- delay = error "delay!"

-- Should this be a -> S a -> S a ??
delay :: Signal Time -> Signal a -> Signal a -> Signal a
delay ~(Signal tm tm_w) ~(Signal d def) ~(Signal rest w) 
        = Signal (Seq.head d :~ rest) 
        $ Port (Var "o")
        $ E
        $ Entity (name "delay") [Var "o"] [(Var "t",tm_w), (Var "init",def),(Var "i",w)]
			[[ TyVar $ Var v | v <- ["o","init","i"]]]


data Time = Time Integer 		-- local clocks
					-- 0 == reset signal
					-- 1 == first cycle
					-- 2 == second cycle

time :: Signal Time -> Signal Integer		-- simluation only
time = fmap (\ (Time t) -> t)

clock :: Signal Time
clock = Signal (Seq.fromList $ map Just $ map Time [0..]) (Port (Var "o0") $ E $ Entity (Name "Lava" "clock") [] [] [[TyVar $ Var "o0",BaseTy T]])

instance OpType Time    
  where op _ nm = Name "Time" nm 
	bitTypeOf _ = T
	initVal = error "can not use Signal Time as an init value"

