-- | The VCD module transforms a Trace into the Verilog Value Change Dump
-- format for viewing in a waveform viewer.
module Language.KansasLava.Testing.Output.VCD(toVCD) where

import Language.KansasLava.Types
import Language.KansasLava.Testing.Trace

import Data.Char
import Data.List

toVCD :: Trace -> String
toVCD (Trace Nothing _ _ _)  = error "can't turn infinite trace into vcd"
toVCD (Trace (Just n) i o p) = unlines
    [ "$timescale 10ns $end"
    , "$scope top $end"
    ]
    ++ unlines [ unwords ["$var wire", show l, ident, show k, "$end"]
               | (ident,(k,TraceStream _ strm)) <- signals
               , let RepValue vs = head strm
               , let l = length vs ]
    ++ "$enddefinitions $end\n"
    ++ values n signals

    where signals = zip vcd_ids $ foldr union [] [i,o,p]

-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
vcd_ids :: [String]
vcd_ids = res
    where chars = [(chr 33)..(chr 126)]
          ids@(_:res) = [[]]  ++ concatMap (\i -> [c:i | c <- chars]) ids

vcdVal :: RepValue -> String -> String
vcdVal r@(RepValue bs) ident | length bs == 1 = show r ++ ident
                             | otherwise      = "b" ++ show r ++ " " ++ ident

values :: Int -> [(String, (OVar, TraceStream))] -> String
values n sigs = dumpVars initials ++ eventList initials (zip [0..] rest)
    where (initials:rest) =
            transpose [ take n $ zip strm (repeat ident) | (ident, (_, TraceStream _ strm)) <- sigs ]

dumpVars :: [(RepValue, String)] -> String
dumpVars vals = "$dumpvars\n" ++ unlines (map (uncurry vcdVal) vals) ++ "$end\n"

eventList :: [(RepValue, String)] -> [(Int,[(RepValue, String)])] -> String
eventList prev rest = case next of
                        [] -> ""
                        ((clk,n):ns) -> "#" ++ show clk ++ "\n" ++ valChange prev n ++ eventList n ns
    where next = dropWhile (\(_, row) -> row == prev) rest

-- the assumption is that these lists are parallel by id
valChange :: [(RepValue, String)] -> [(RepValue, String)] -> String
valChange prev cur = unlines [ vcdVal val ident | (p,c@(val,ident)) <- zip prev cur
                                             , p /= c ]
