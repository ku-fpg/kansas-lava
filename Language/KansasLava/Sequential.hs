module Language.KansasLava.Sequential where

import Language.KansasLava.Entity
import Language.KansasLava.Signal

delay :: Signal a -> Signal a -> Signal a
delay (Signal def) (Signal w) = Signal $ Wire $ Entity (name "delay") [def,w]

