{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
-- | This module contains functions for generating VCD debug traces.
-- It also provides functionality for (de)serializing Traces.
module Language.KansasLava.VCD
    ( VCD(..)
    , toVCDFile
    , fromVCDFile
    , addEvent
    -- * Generate a Signature from a VCD trace
    , toSignature
    , fromSignature
    -- * Compare two VCDs
    , cmpVCD
    , ioOnly
    -- * Make a VCD trace from a Fabric and input Pads
    , mkVCD
    , mkVCDCM
    -- * Reading and Writing the Test Bench Format (.tbf)
    , readTBF
    , writeTBF
    -- * Convert Rep to Test Bench Word
    , tbw2rep
    , rep2tbw
    ) where

import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Internal
import qualified Language.KansasLava.VCD.EventList as E

import qualified Language.KansasLava.Stream as S

import Control.Monad

import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as M

----------------------------------------------------------------------------------------
-- | The VC (value change) is used for capturing traces of shallow-embedded
-- streams. It combines the bitwise representation of a stream along with the
-- type of the stream.
data VC = VC Type (E.EventList RepValue)
    deriving (Eq, Ord, Read, Show)

addVC :: VC -> Int -> RepValue -> VC
addVC (VC ty el) i v = VC ty $ E.insert (i,v) el

newVC :: forall w . (Rep w) => Witness w -> VC
newVC _ = VC (repType (Witness :: Witness w)) $ E.fromList []

-- | Convert a Pad to a Tracestream
padToVC :: Int -> Pad -> VC
padToVC c (StdLogic s) = convertVC $ take c $ S.toList $ shallowS s
padToVC c (StdLogicVector s) = convertVC $ take c $ S.toList $ shallowS s
padToVC _ other = error $ "fix padToVC for " ++ show other

-- | Convert a Stream to a VC. Note this can force evaluation.
convertVC :: forall w . (Rep w) => [X w] -> VC
convertVC l = VC ty $ E.fromList $ map toRep l
    where (VC ty _) = newVC (Witness :: Witness w)

----------------------------------------------------------------------------------------
-- | 'VCD' is a primary bit-wise record of an interactive session with some circuit
-- Map from module/name to stream.
newtype VCD = VCD [(String,VC)]
    deriving (Eq)

instance Show VCD where
    show (VCD m) = unlines $ headers : [ pr (show clk) clkwidth str | (clk,str) <- reverse $ E.unEL rows ]
        where wMaxLens :: [E.EventList (String,Int)]
              wMaxLens = [ let maxlen = max $ length h
                           in fmap (\v -> let str = show v in (str, maxlen $ length str)) el
                         | (h, VC _ el) <- m ]

              rows = fmap fst
                   $ E.mergeWith (\(s1,l1) (s2,l2) -> (pr s1 l1 s2, l1 + l2))
                                 wMaxLens

              clkwidth = max 3 $ length $ show $ E.length rows

              widths = [ l | E.EL ((_,(_,l)):_) <- wMaxLens ]
              headers = foldr (\(h,l) r -> pr h l r) "" $ zip ("clk" : map fst m) (clkwidth : widths)

              pr s1 l1 s2 = s1 ++ replicate (1 + l1 - length s1) ' ' ++ s2

addEvent :: forall w . (Rep w) => String -> Int -> (X w) -> VCD -> VCD
addEvent nm i v (VCD m) | nm `elem` map fst m = VCD [ (n,if n == nm then addVC vc i (toRep v) else vc) | (n,vc) <- m ]
                        | otherwise           = VCD $ (nm, addVC (newVC (Witness :: Witness w)) i (toRep v)) : m

-- | Generate a signature from a trace.
-- TODO: support generics in both these functions?
toSignature :: VCD -> Signature
toSignature vcd = Signature (convert $ inputs vcd) (convert $ outputs vcd) []
    where convert m = [ (dropModName nm,ty) | (nm,VC ty _) <- m ]
          dropModName = reverse . takeWhile (/= '/') . reverse

-- | Creates an (empty) trace from a signature
fromSignature :: Signature -> VCD
fromSignature (Signature inps outps _) = VCD $ convert "inputs" inps ++ convert "outputs" outps
    where convert mnm l = [ (mnm ++ "/" ++ nm, VC ty $ E.fromList [])  | (nm, ty) <- l ]

scope :: String -> VCD -> [(String,VC)]
scope s = scopes [s]

scopes :: [String] -> VCD -> [(String,VC)]
scopes s (VCD m) = [ (nm,ts) | (nm,ts) <- m
                             , s' <- s
                             , s' `isPrefixOf` nm ]

inputs :: VCD -> [(String,VC)]
inputs = scope "inputs"

outputs :: VCD -> [(String,VC)]
outputs = scope "outputs"

----------------------------------------------------------------------------------------

-- | Convert a VCD file to a VCD object.
fromVCDFile :: String -> Signature -> VCD
fromVCDFile vcd sig = VCD $ [ ("inputs/" ++ nm, VC ty s)
                            | (nm,ty) <- sigInputs sig, (snm,s) <- streams, nm == snm ]
                         ++ [ ("outputs/" ++ nm, VC ty s)
                            | (nm,ty) <- sigOutputs sig, (snm, s) <- streams, nm == snm ]
    where (signames, ls) = defs2map $ dropWhile (not . isPrefixOf "$var") $ lines $ trimWhile isSpace vcd
          vals = uncurry changes . dumpvars $ ls
          streams = [ (nm, vs) | (i, nm) <- signames, (i',vs) <- vals, i == i' ]

-- | Parse definitions section, getting map of VCDIDs to signal names.
defs2map :: [String] -> ([(VCDID,String)],[String])
defs2map = go []
    where go m (l:ls) | head ws == "$enddefinitions" = (m,ls)
                      | head ws == "$var" = go ((ws !! 3, trimWhile (== '"') $ ws !! 4):m) ls
                      | otherwise = error "defs2map: parse error!"
            where ws = words l
          go _ _ = error "defs2map: parse error, no lines!"

trimWhile :: (Char -> Bool) -> String -> String
trimWhile p = f . f
    where f = reverse . dropWhile p

-- | Parse $dumpvars section, getting initial values for each signal.
dumpvars :: [String] -- ^ remaining lines of the vcd file
         -> ([(VCDID,RepValue)],[String]) -- ^ map of vcdIds to initial values
dumpvars ("$dumpvars":ls) = go ls []
    where go ("$end":rest) m = (m,rest)
          go (line:rest)   m = let (vcdId,val) = parseVal line
                                   (m',rest')  = go rest m
                               in ((vcdId,val):m',rest')
          go [] _ = error $ "dumpvars: no $end!"
dumpvars other = error $ "dumpvars: bad parse! " ++ show other

-- | Parse list of changes into an EventList
changes :: [(VCDID,RepValue)] -> [String] -> [(String, E.EventList RepValue)]
-- changes initVals ls = foldl fromEvList [ (i,[(0,v)]) | (i,v) <- initVals ]
changes initVals ls = [ (nm, E.EL $ evs' ++ [(len,v)])
                      | (nm,E.EL evs') <- M.toList $ unMerge evs
                      , let v = case evs' of [] -> undefined; _ -> snd . last $ evs' ]
    where (_,E.EL evs) = foldl go (0,E.fromList []) ls
          len = maximum . map fst $ evs

          go :: (Int,E.EventList (String, RepValue)) -> String -> (Int,E.EventList (String, RepValue))
          go (_,el) ('#':time) = (read time, el)
          go (t,el) line       = (t, E.insert (t, parseVal line) el)

          unMerge :: [(Int,(String,RepValue))] -> M.Map String (E.EventList RepValue)
          unMerge = foldr f $ M.fromList [ (i,E.fromList [v]) | (i,v) <- initVals ]
            where f (i,(nm,v)) m | M.member nm m = M.adjust (E.insert (i,v)) nm m
                                 | otherwise     = M.insert nm (E.EL [(i,v)]) m

parseVal :: String -> (String, RepValue)
parseVal = go . words
    where go [bitVal] | length bitVal > 1   = (tail bitVal, tbw2rep $ take 1 bitVal)
          go [t:vals,ident] | t `elem` "bB" = (ident      , tbw2rep vals           )
          go other                          = error $ "parseVal: can't parse! " ++ unwords other

----------------------------------------------------------------------------------------

-- | Convert a 'VCD' to a VCD file.
toVCDFile :: Bool    -- ^ Whether to include the clock signal in the list of signals
          -> Integer -- ^ Timescale in nanoseconds
          -> VCD
          -> String
toVCDFile _incClk ts (VCD m) = unlines
    [ "$version\n   Kansas Lava\n$end"
    , "$timescale " ++ show ts ++ "ns $end"
    , "$scope top $end"
    ]
    ++ unlines [ unwords ["$var wire", show $ typeWidth ty, ident, show k, "$end"]
               | (ident,(k,VC ty _)) <- signals ]
    ++ "$enddefinitions $end\n"
    ++ values [ (i',strm) | (i',(_,VC _ strm)) <- signals ]

    where signals = zip vcdIds m

type VCDID = String
-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
vcdIds :: [VCDID]
vcdIds = map code [0..]
    where code :: Int -> VCDID
          code i | i < 0 = ""
          code i         = chr (33 + mod i 94) : code (div i 94 - 1)

values :: [(VCDID, E.EventList RepValue)] -> String
values sigs = dumpVars initials ++ eventList rest
    where (initials,rest) = unzip [ ((i, v), (i, E.EL xs)) | (i, E.EL ((_,v):xs)) <- sigs ]

dumpVars :: [(VCDID, RepValue)] -> String
dumpVars vals = "$dumpvars\n" ++ unlines (map (uncurry vcdVal) vals) ++ "$end\n"

eventList :: [(VCDID, E.EventList RepValue)] -> String
eventList strms = unlines [ "#" ++ show i ++ "\n" ++ ls | (i,ls) <- evs ]
    where (E.EL evs) = E.mergeWith (\s1 s2 -> s1 ++ ('\n':s2))
                                 [ fmap (vcdVal ident) elist | (ident,elist) <- strms ]

vcdVal :: VCDID -> RepValue -> String
vcdVal i r@(RepValue bs) | length bs == 1 = rep2tbw r ++ i
                         | otherwise      = "b" ++ rep2tbw r ++ " " ++ i

----------------------------------------------------------------------------------------

-- | Compare two trace objects. First argument is the golden value. See notes for cmpRepValue
cmpVCD :: VCD -> VCD -> Bool
cmpVCD (VCD m1) (VCD m2) =
    and [ k1 == k2 && cmpVC (tslen s1) s1 s2
        | ((k1,s1),(k2,s2)) <- zip (sorted m1) (sorted m2)
        ]
    where tslen (VC _ el) = E.length el
          sorted = sortBy ((compare) `on` fst)

ioOnly :: VCD -> VCD
ioOnly = VCD . scopes ["inputs","outputs"]

-- | 'cmpVC' compares two traces to determine equivalence. Note this
-- uses 'cmpRepValue' under the hood, so the first argument is considered the
-- golden trace.
cmpVC :: Int -> VC -> VC -> Bool
cmpVC count (VC t1 s1) (VC t2 s2) = t1 == t2 && countLTs1 && s1LTs2 && eql
    where countLTs1 = count <= E.length s1
          s1LTs2 = E.length s1 <= E.length s2
          eql = and $ map snd . E.unEL $ E.zipWith cmpRepValue (E.take count s1) (E.take count s2)

-- | Make a 'VCD' from a 'Fabric' and its input.
mkVCD :: Int            -- ^ number of cycles to capture
      -> Fabric ()      -- ^ The Fabric we are tracing
      -> [(String,Pad)] -- ^ Inputs to the Fabric
      -> IO VCD
mkVCD c fabric input = do
    (trace, _) <- mkVCDCM c fabric input (return)
    return trace

-- | Version of 'mkVCD' that accepts arbitrary circuit mods.
mkVCDCM :: Int               -- ^ number of cycles to capture
        -> Fabric ()         -- ^ Fabric we are tracing
        -> [(String, Pad)]   -- ^ Inputs to the Fabric
        -> (KLEG -> IO KLEG) -- ^ KLEG Mod
        -> IO (VCD, KLEG)
mkVCDCM c fabric input circuitMod = do
    rc <- (reifyFabric >=> circuitMod) fabric

    let (_,output) = runFabric fabric input
        tr = VCD $ [ ("inputs/" ++ nm, padToVC c p)
                   | (nm,_) <- theSrcs rc
                   , (nm',p) <- input
                   , nm == nm' ]
                 ++ [ ("outputs/" ++ nm, padToVC c p)
                    | (nm,_,_) <- theSinks rc
                    , (nm',p) <- output
                    , nm == nm' ]

    return (tr, rc)

----------------------------------------------------------------------------------------

-- | Convert the inputs and outputs of a VCD to the textual format expected
-- by a testbench.
writeTBF :: String -> VCD -> IO ()
writeTBF filename = writeFile filename . unlines . mergeWith (++) . asciiStrings

-- | Inverse of showTBF, needs a signature for the shape of the desired VCD.
-- Creates a VCD from testbench signal files.
readTBF :: [String] -> Signature -> VCD
readTBF ilines sig = VCD $ ins ++ outs
    where et = fromSignature sig
          widths = [ typeWidth ty
                   | (_,VC ty _) <- inputs et ++ outputs et
                   ]
          (inSigs, outSigs) = splitAt (length $ inputs et) $ splitLists ilines widths
          addToMap sigs m = [ (k,VC ty $ E.fromList $ map tbw2rep strm)
                            | (strm,(k,VC ty _)) <- zip sigs m
                            ]
          (ins, outs) = (addToMap inSigs $ inputs et, addToMap outSigs $ outputs et)

-- | Convert a VCD into a list of lists of Strings, each String is a value,
-- each list of Strings is a signal.
asciiStrings :: VCD -> [[String]]
asciiStrings vcd = [ E.toList $ fmap rep2tbw s | VC _ s <- insOuts ]
    where insOuts = [ ts | (_,ts) <- inputs vcd ++ outputs vcd ]

-- | Convert string representation used in testbench files to a RepValue
-- Note the reverse here is crucial due to way vhdl indexes stuff
tbw2rep :: String -> RepValue
tbw2rep vals = RepValue [ case v of
                            'X' -> Nothing
                            '1' -> Just True
                            '0' -> Just False
                            'U' -> Nothing
                            other -> error $ "tbw2rep: bad character! " ++ [other]
                        | v <- reverse vals ]

-- | Convert a RepValue to the string representation used in testbench files
rep2tbw :: RepValue -> String
rep2tbw (RepValue vals) = [ case v of
                              Nothing   -> 'X'
                              Just True  -> '1'
                              Just False -> '0'
                          | v <- reverse vals ]

