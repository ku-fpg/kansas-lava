module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative

high :: Signal Bool
high = Signal (pure True) $ Wire $ Pad $ Var "high"

low :: Signal Bool
low = Signal (pure False) $ Wire $ Pad $ Var "low"

-- entity3??
mux :: Signal Bool -> Signal a -> Signal a -> Signal a
mux = entity3 (Name "Bool" "mux2") 
                (\ a b c -> if a then b else c)

{-
-- Use overloaded versions.

bitAnd :: Signal Bool -> Signal Bool -> Signal Bool
bitAnd = entity2 (Name "Bool" "&&") (&&)
bitOr :: Signal Bool -> Signal Bool -> Signal Bool
bitOr  = entity2 (Name "Bool" "||") (||)
bitXor :: Signal Bool -> Signal Bool -> Signal Bool
bitXor = entity2 (Name "Bool" "xor") (/=)

bitNot :: Signal Bool -> Signal Bool
bitNot = entity1 (Name "Bool" "not") (not)
-}
