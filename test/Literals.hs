module Literals where

import Language.KansasLava
import Data.Sized.Unsigned


xxx :: Signal Bool -> Signal U1 -> Signal U1
xxx  a b = mux2 a b 1


yyy :: Signal Bool -> Signal U1 -> Signal U1
yyy a b = mux2 a 1 b

zzz :: Signal U1 -> Signal U1 -> Signal U1
zzz a b = mux2 high a b