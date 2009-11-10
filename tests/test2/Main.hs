import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Control.Applicative



ring n clk = out
  where shifting 1 = [delay clk high o]
        shifting i = let rs@(r:_) = shifting (i-1)
                     in (delay clk low r):rs
        ~out@(o:_) = shifting n
ring' n clk = M.matrix $ ring n clk


ringNew :: (Enum ix, Size ix) => Time -> M.Matrix ix (Signal Bool)
ringNew clk = m
    where m = M.forAll $ \ i -> delay clk (if (i == minBound) then high else low)
                                      (if (i == maxBound) then m M.! minBound else m M.! (succ i))
