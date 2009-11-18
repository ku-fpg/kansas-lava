{-# LANGUAGE FlexibleInstances, ScopedTypeVariables,FlexibleContexts  #-}
module LDPC where

import Language.KansasLava
import Language.KansasLava.VHDL.Testbench

import Data.Sized.Unsigned
import Data.Sized.Ix
import qualified Data.Sized.Matrix as M

import Data.List(sortBy)

type U4 = (Unsigned X4)

-- The LDPC algorithm does iterative updating of variable nodes. The 'minSum'
-- function is supposed to do this for a single Check Node.

-- The minsum function takes a matrix (of lambda values) and returns a new
-- matrix of lambda values. It uses the minsum algorithm, with the optimization
-- that Ehsan described where you only need to calculate the minimum two values.
-- I'm not sure that I actually understand that, so it may be completely wrong.
--
-- Andy or Tristan can probably show how to use the matrix 'forAll' or other
-- matrix operation to eliminate the (fromList . toList) stuff.
minSum :: (Size ix, OpType a, Ord a, Num a) =>
          M.Matrix ix (Signal a) -> M.Matrix ix (Signal a)
minSum matrix = M.fromList (map upd lambdas) -- M.fromList and the map are statically reduced
  where lambdas = M.toList matrix -- M.toList is statically reduced.
        -- m is the minimum, m2 is the second minimum
        (m,m2) = min2 lambdas
        -- upd replaces every value (except the minimum) with minimum. The
        -- actual minimum is replaced with the second minimum.
        -- Check: Is the mux backwards? I can never remember...
        upd lam = mux2 (lam .==. m) m2 m

-- min2 calculates the minimum two values for a list. This is similar to a
-- 'tree' combinator, but it requires an additional two muxes/comparisons,
-- because we have to propogate both the minimum and the second minimum, which
-- may both be the result of the same recursive call to the same decomposed list.
-- I'm interested to hear other suggestions on how to do this...
min2 [a,b] = mux2 (a .<. b) (a,b) (b,a)
min2 [a] = (a,99999999) -- FIXME: Signal doesn't support maxbound
min2 vs = min2 [e,f]
  where (l,r) = splitAt (length vs `div` 2) vs
        (a,b) = min2 l
        (c,d) = min2 r
        (e,_) = min2 [a,d]
        (f,_) = min2 [c,b]

instance (Num a, OpType a, Enum a) => Enum (Signal a) where
  toEnum x = fromIntegral x
  fromEnum (Signal _ (Lit x)) = fromInteger x
  fromEnum _ = error "fromEnum (Signal a)"


instance (Ports b, Size ix) => Ports ((M.Matrix ix (Signal a)) -> b) where
  ports vars f = ports rem (f $ M.fromList [pad i | i <- inps])
    where (inps,rem) = splitAt (size (error "Ports(M.Matrix ix (Signal a)" :: ix)) vars
          pad i = Signal (error "pad") (Pad (Var i))

instance (OpType (M.Matrix ix a), OpType a, Size ix) => Ports (M.Matrix ix (Signal a)) where
 ports vs v = ports  vs (matrixSignalToSignalMatrix v)


minSum' :: M.Matrix X3 (Signal U4) -> M.Matrix X3 (Signal U4)
minSum' = minSum

cats :: M.Matrix X2 (Signal Int) -> M.Matrix X2 (Signal Int)
cats x = x


-- cn (min,min2) (min',min2') vi =

-- A variable node is associated with a set of check nodes.

type VariableNode = Int
-- a/a2 is the _previous_ iteration's minimum/min2.
-- b/b2 is the _current_ iteration's minimum/min2 accumulator.
data CheckNode = CN { a,a2,b,b2 :: Int }
type VAdjacent = VariableNode -> [CheckNode]

{-

The idea is that we can interleave the calculation of Check Node updates with
the calculation of Variable Node updates.

Check Nodes are updated incrementally,by accumulating variable node values. We
scan the variable nodes from left-to-right. For each variable (column), we fetch
the previous variable node value, along with the min and min2 values for each
check node associated with that variable. The check node values are combined to
calculate a new variable node value. This new variable node value is then
propogated to each of the check nodes associated with the variable node, which
then accumulate the new variable node value. Moreover, the new variable node
value is written to the variable node store.


-}


