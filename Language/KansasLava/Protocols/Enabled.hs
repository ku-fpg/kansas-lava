{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies,
    TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes,
    UndecidableInstances #-}


-- | The 'Enabled' module allows the construction of circuits that use
-- additional control logic -- an enable signal -- that externalizes whether a
-- data signal is valid.
module Language.KansasLava.Protocols.Enabled
  (Enabled,
  packEnabled,
  enabledVal, isEnabled,
  mapEnabled,
  enabledS, disabledS,
  ) where

import Language.KansasLava.Comb
import Language.KansasLava.Rep
import Language.KansasLava.Signal

-- | Enabled is a synonym for Maybe.
type Enabled a = Maybe a


-- | Create a register with a load-enable. (DEADCODE)
-- enabledRegister :: forall a clk. (Rep a, Clock clk) => CSeq clk (Enabled a) -> CSeq clk a
-- enabledRegister inp = res
--    where
-- 	(en,v) = unpack inp
-- 	res    = delay (mux2 en (v,res))

-- | Turns a list of maybe values into enabled values. (DEADCODE)
-- toEnabledSeq :: forall a . (Rep a) => [Maybe a] -> Seq (Enabled a)
-- toEnabledSeq xs = toSeqX [ optX (Just x)
-- 			 | x <- xs
-- 			 ]

-- | Convert a signal of one type 'a' into a possibly enabled signal of another
-- type. (DEADCODE)
-- fullEnabled :: forall a b sig . (Signal sig, Show a, Rep a, Show b, Rep b)
-- 	   => sig a -> (a -> Maybe b) -> sig (Enabled b)
-- fullEnabled iseq f = pack (funMap (return . isJust . f) iseq :: sig Bool,funMap f iseq :: sig b)

-- | This is lifting *Comb* because Comb is stateless, and the 'en' Bool being
-- passed on assumes no history, in the 'a -> b' function.
mapEnabled :: (Rep a, Rep b, Signal sig) => (Comb a -> Comb b) -> sig (Enabled a) -> sig (Enabled b)
mapEnabled f en = pack (en_bool,liftS1 f en_val)
   where (en_bool,en_val) = unpack en

{-
zipEnabled :: (Rep a, Rep b, Rep c, Signal sig) => (Comb a -> Comb b -> Comb c) -> sig (Enabled a) -> sig (Enabled b) -> sig (Enabled c)
zipEnabled f en1 en2 = pack (en_bool1 `phi` en_bool2,liftS2 f en_val1 en_val2)
   where (en_bool1,en_val1) = unpack en1
	 (en_bool2,en_val2) = unpack en2
-}

-- | Lift a data signal to be an Enabled signal, that's always enabled.
enabledS :: (Rep a, Signal sig) => sig a -> sig (Enabled a)
enabledS s = pack (pureS True,s)

-- | Create a signal that's never enabled.
disabledS :: (Rep a, Signal sig) => sig (Enabled a)
disabledS = pack (pureS False,undefinedS)

-- | Combine a boolean control signal and an data signal into an enabled signal.
packEnabled :: (Rep a, Signal sig) => sig Bool -> sig a -> sig (Enabled a)
packEnabled s1 s2 = pack (s1,s2)

-- | Break the representation of an Enabled signal into a Bool signal (for whether the
-- value is valid) and a signal for the data.
unpackEnabled :: (Rep a, Signal sig) => sig (Enabled a) -> (sig Bool, sig a)
unpackEnabled = unpack

-- | Drop the Enabled control from the signal. The output signal will be Rep
-- unknown if the input signal is not enabled.
enabledVal :: (Rep a, Signal sig) => sig (Enabled a) -> sig a
enabledVal = snd .  unpackEnabled

-- | Determine if the the circuit is enabled.
isEnabled :: (Rep a, Signal sig) => sig (Enabled a) -> sig Bool
isEnabled = fst .  unpackEnabled

-- | a 'safe' delay that uses 'Nothing' to give a default value. (DEADCODE)
-- delayEnabled :: (Rep a, Clock clk) => CSeq clk (Enabled a) -> CSeq clk (Enabled a)
-- delayEnabled inp = register Nothing inp

-- | Combine two Enabled signals. Output enabled if either of the inputs is
-- enabled. If both are, it biases to the first argument. (DEADCODE)
-- joinEnabled :: (Signal sig, Rep a) => sig (Enabled a) -> sig (Enabled a) -> sig (Enabled a)
-- joinEnabled = liftS2 $ \ e1 e2 ->
-- 			let (en1,v1) = unpack e1
-- 	 		    (en2,v2) = unpack e2
-- 	                in pack (en1 `or2` en2, mux2 en1 (v1,v2))

-- | A register with a load-enable. (DEADCODE)
-- latch :: forall a. (Rep a) => Seq (Enabled a) -> Seq a
-- latch inp0 = out
--   where out = mux2 (isEnabled inp0) (enabledVal inp0,delay out)

