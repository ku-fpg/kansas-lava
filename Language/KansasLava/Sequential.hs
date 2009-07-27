module Language.KansasLava.Sequential where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq


--delay :: (Sig sig) => sig a -> sig a -> sig a
-- delay = error "delay!"

-- Should this be a -> S a -> S a ??
delay :: Signal a -> Signal a -> Signal a
delay ~(Signal d def) ~(Signal rest w) 
        = Signal (h :~ rest) 
        $ Port (Var "o")
        $ E
        $ Entity (name "delay") [Var "o"] [(Var "init",def),(Var "i",w)]
  where (h:_) = toList d
