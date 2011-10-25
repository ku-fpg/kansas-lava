-- | The VCD module transforms a Trace into the Verilog Value Change Dump
-- format for viewing in a waveform viewer.
module Language.KansasLava.VCD (toVCD,fromVCD) where

import Language.KansasLava.Trace
import Language.KansasLava.Types

import Data.Char
import Data.Function
import Data.List
import Data.Maybe

-- | Convert a VCD file to a Trace.
-- TODO: use sig file to recover types and input/output designation
fromVCD :: String -> Signature -> Trace
fromVCD vcd sig = Trace (Just longest) [ (nm,TraceStream ty $ fromJust $ lookup nm streams) | (nm,ty) <- sigInputs sig ]
                                       [ (nm,TraceStream ty $ fromJust $ lookup nm streams) | (nm,ty) <- sigOutputs sig ]
                                       []
    where (signames, ls) = defs2map $ dropWhile (not . isPrefixOf "$var") $ lines $ trim vcd
          vals = uncurry changes . dumpvars $ ls
          streams = [ (nm, extendTo longest vs) | (i, nm) <- signames, let vs = fromJust $ lookup i vals ]
          longest = maximum . map (length . snd) $ vals

          trim = let f = reverse . dropWhile isSpace in f . f

defs2map :: [String] -> ([(String,String)],[String])
defs2map = go []
    where go m (l:ls) | head ws == "$enddefinitions" = (m,ls)
                      | head ws == "$var" = go ((ws !! 3, trimQuot $ ws !! 4):m) ls
                      | otherwise = error "defs2map: parse error!"
            where ws = words l
          go _ _ = error "defs2map: parse error, no lines!"

          trimQuot = let f = reverse . dropWhile (== '"') in f . f

dumpvars :: [String] -- ^ remaining lines of the vcd file
         -> ([(String,RepValue)],[String]) -- ^ map of vcdIds to initial values
dumpvars ("$dumpvars":ls) = go ls []
    where go ("$end":rest) m = (m,rest)
          go (line:rest)   m = let (vcdId,val) = parseVal line
                                   (m',rest')  = go rest m
                               in ((vcdId,val):m',rest')
          go [] _ = error $ "dumpvars: no $end!"
dumpvars other = error $ "dumpvars: bad parse! " ++ show other

changes :: [(String,RepValue)] -> [String] -> [(String, [RepValue])]
changes initVals ls = foldl fromEvList [ (i,[v]) | (i,v) <- initVals ]
                          $ sortBy (compare `on` snd3) $ snd $ foldl go (0,[]) ls
    where go :: (Int,[(String, Int, RepValue)]) -> String -> (Int,[(String, Int, RepValue)])
          go (_,m) ('#':time) = (read time, m)
          go (t,m) line       = (t, let (vcdId,val) =  parseVal line
                                    in (vcdId, t, val):m)

          fromEvList :: [(String,[RepValue])] -> (String, Int, RepValue) -> [(String, [RepValue])]
          fromEvList m (i,t,v) = [ (i',if i == i'
                                       then extendTo t vs ++ [v]
                                       else vs
                                   )
                                 | (i',vs) <- m ]

          snd3 (_,y,_) = y

extendTo :: Show a => Int -> [a] -> [a]
extendTo _ [] = error "extendTo: empty list"
extendTo i xs | 1 + i >= length xs = xs ++ (replicate (1 + i - length xs) (last xs))
              | otherwise = error $ "extendTo: i < xs - i: " ++ show i ++ " xs: " ++ show xs

parseVal :: String -> (String, RepValue)
parseVal = go . words
    where go [bitVal] | length bitVal > 1   = (tail bitVal, tbw2rep $ take 1 bitVal)
          go [t:vals,ident] | t `elem` "bB" = (ident      , tbw2rep vals           )
          go other                          = error $ "parseVal: can't parse! " ++ unwords other

-- | Convert a 'Trace' to a VCD representation of the trace.
toVCD :: Bool    -- ^ Whether to include the clock signal in the list of signals
      -> Integer -- ^ Timescale in nanoseconds
      -> Trace
      -> String
toVCD _       _  (Trace Nothing  _ _ _) = error "can't turn infinite trace into vcd"
toVCD _incClk ts (Trace (Just n) i o p) = unlines
    [ "$version\n   Kansas Lava\n$end"
    , "$timescale " ++ show ts ++ "ns $end"
    , "$scope top $end"
    ]
    ++ unlines [ unwords ["$var wire", show l, ident, show k, "$end"]
               | (ident,(k,TraceStream _ strm)) <- signals
               , let RepValue vs = head strm
               , let l = length vs ]
    ++ "$enddefinitions $end\n"
    ++ values n signals

    where signals = zip vcdIds $ foldr union [] [i,o,p]

-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
vcdIds :: [String]
vcdIds = map code [0..]
    where code :: Int -> String
          code i | i < 0 = ""
          code i         = [chr (33 + mod i 94)] ++ code (div i 94 - 1)

vcdVal :: RepValue -> String -> String
vcdVal r@(RepValue bs) ident | length bs == 1 = rep2tbw r ++ ident
                             | otherwise      = "b" ++ rep2tbw r ++ " " ++ ident

values :: Int -> [(String, (String, TraceStream))] -> String
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
