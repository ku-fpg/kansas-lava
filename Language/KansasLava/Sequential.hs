module Language.KansasLava.Sequential where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Seq as Seq


--delay :: (Sig sig) => sig a -> sig a -> sig a
-- delay = error "delay!"

-- Should this be a -> S a -> S a ??
delay :: Signal a -> Signal a -> Signal a
delay ~(Signal d def) ~(Signal rest w) 
        = Signal (Seq.head d :~ rest) 
        $ Port (Var "o")
        $ E
        $ Entity (name "delay") [Var "o"] [(Var "init",def),(Var "i",w)]
			[[ TyVar $ Var v | v <- ["o","init","i"]]]
