module Language.KansasLava.Logic where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Data.Bits
import Control.Applicative

high :: Signal Bool
high = Signal (pure True) $ Pad $ Var "high"

low :: Signal Bool
low = Signal (pure False) $ Pad $ Var "low"

-- entity3??
mux :: Signal Bool -> Signal a -> Signal a -> Signal a
mux sC sT sF = 
        o0 $ entity3 (Name "Bool" "mux2")
              [Var "c",Var "t", Var "f"]
              [Var "o0"]
	      [ [TyVar $ Var "c",Ty $ Bit]
	      , [TyVar $ Var "t",TyVar $ Var "f",TyVar $ Var "o0"]
	      ]
              (\ a b c -> if a then b else c)
              sC sT sF