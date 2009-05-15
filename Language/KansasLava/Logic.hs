module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
    
and2 :: (Signal a, Signal a) -> Signal a
and2 (Signal w1,Signal w2) = Signal $ Wire $ Entity (var "and2") [w1,w2]

high :: Signal a
high = Signal $ Wire $ Pad $ var "high"

low :: Signal a
low  = Signal $ Wire $ Pad $ var "low"

mux :: Signal Bool -> (Signal a, Signal a) -> Signal a
mux (Signal s) (Signal w1,Signal w2) = Signal $ Wire $ Entity (var "mux") [s,w1,w2]
    