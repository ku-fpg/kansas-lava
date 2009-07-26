module Language.KansasLava.Sequential where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq


--delay :: (Sig sig) => sig a -> sig a -> sig a
-- delay = error "delay!"

-- Should this be a -> S a -> S a ??
delay :: Signal a -> Signal a -> Signal a
delay (Signal d def) (Signal rest w) = Signal (h :~ rest) $ Wire $ Entity (name "delay") [def,w]
  where (h:_) = toList d
