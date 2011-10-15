{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies,
    TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes,
    UndecidableInstances #-}


-- | The 'Enabled' module allows the construction of circuits that use
-- additional control logic -- an enable signal -- that externalizes whether a
-- data signal is valid.
module Language.KansasLava.Protocols.Enabled
  (Enabled,
  packEnabled, unpackEnabled,
  enabledVal, isEnabled,
  mapEnabled,
  enabledS, disabledS,
  ) where

import Language.KansasLava.Signal
import Language.KansasLava.Rep

-- | Enabled is a synonym for Maybe.
type Enabled a = Maybe a

-- | This is lifting *Comb* because Comb is stateless, and the 'en' Bool being
-- passed on assumes no history, in the 'a -> b' function.
mapEnabled :: (Rep a, Rep b, sig ~ Signal clk) 
           => (forall clk' . Signal clk' a -> Signal clk' b) 
           -> sig (Enabled a) -> sig (Enabled b)
mapEnabled f en = pack (en_bool,f en_val)
   where (en_bool,en_val) = unpack en


-- | Lift a data signal to be an Enabled signal, that's always enabled.
enabledS :: (Rep a, sig ~ Signal clk) => sig a -> sig (Enabled a)
enabledS s = pack (pureS True,s)

-- | Create a signal that's never enabled.
disabledS :: (Rep a, sig ~ Signal clk) => sig (Enabled a)
disabledS = pack (pureS False,undefinedS)

-- | Combine a boolean control signal and an data signal into an enabled signal.
packEnabled :: (Rep a, sig ~ Signal clk) => sig Bool -> sig a -> sig (Enabled a)
packEnabled s1 s2 = pack (s1,s2)

-- | Break the representation of an Enabled signal into a Bool signal (for whether the
-- value is valid) and a signal for the data.
unpackEnabled :: (Rep a, sig ~ Signal clk) => sig (Enabled a) -> (sig Bool, sig a)
unpackEnabled = unpack

-- | Drop the Enabled control from the signal. The output signal will be Rep
-- unknown if the input signal is not enabled.
enabledVal :: (Rep a, sig ~ Signal clk) => sig (Enabled a) -> sig a
enabledVal = snd .  unpackEnabled

-- | Determine if the the circuit is enabled.
isEnabled :: (Rep a, sig ~ Signal clk) => sig (Enabled a) -> sig Bool
isEnabled = fst .  unpackEnabled


