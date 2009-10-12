module EdgeCounter where

import Language.KansasLava

-- increments an internal counter (the output value) whenever the
-- "input" changes (rising AND falling edges)
edgeCounter :: Time -> Signal Bool -> Signal Int
edgeCounter clk input = count
  where reg    = delay clk initVal input
        count  = delay clk 0 up
        inputChange  = xor2 input reg
        up     = mux2 inputChange (count + 1) count


a -: b = delay clock a b

cycles = foldr (-:) low $ take 10 (cycle [low,low,high])

