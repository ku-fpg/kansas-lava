{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
-- | This module contains functions for manipulating (extending, querying, modifying) debugging Traces. It also provides functionality for (de)serializing Traces.
module Language.KansasLava.VCD
    ( VCD(..)
    , toVCD
    , fromVCD
    , toSignature
    , fromSignature
    , cmpVCD
    , cmpVCDIO
    , mkVCD
    , mkVCDCM
    -- * Reading and Writing the Test Bench Format (.tfb)
    , readTBF
    , writeTBF
    , tbw2rep
    , rep2tbw
    ) where

import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Internal

import qualified Language.KansasLava.Stream as S

import Control.Monad

import Data.Char
import Data.Function
import Data.List
import Data.Maybe

-- | 'VCD' is a primary bit-wise record of an interactive session with some circuit
-- Map from module/name to stream.
newtype VCD = VCD [(String,TraceStream)]
    deriving (Eq)

-- | The TraceStream is used for capturing traces of shallow-embedded
-- streams. It combines the bitwise representation of a stream along with the
-- type of the stream.
data TraceStream = TraceStream Type (EventList RepValue) -- to recover type, eventually clock too?
    deriving (Eq, Ord, Read, Show)

-- instances for VCD
instance Show VCD where
    show (VCD m) = pr Nothing sorted
        where sorted = [ (mnm,nm,ts) | (fnm,ts) <- sortBy ((compare) `on` fst) m
                                     , let (mnm,nm) = modnames fnm ]
              pr :: Maybe String -> [(String,String,TraceStream)] -> String
              pr _       [] = ""
              pr Nothing r@((mnm,_,_):_) = pr (Just $ '_' : mnm) r
              pr (Just p) ((mnm,nm,ts):r) = (if p /= mnm
                                             then mnm ++ "\n"
                                             else "") ++ nm ++ ": " ++ show ts ++ "\n" ++ pr (Just mnm) r

              modnames :: String -> (String,String)
              modnames = (\(nm,mn) -> (reverse nm, reverse $ tail mn)) . span (/= '/') . reverse

{-
instance Read VCD where
    readsPrec _ str = [(VCD { inputs = ins, outputs = outs, probes = ps },unlines rest)]
        where ("INPUTS":ls) = lines str
              (ins,"OUTPUTS":r1) = readMap ls
              (outs,"PROBES":r2) = readMap r1
              (ps,"END":rest) = readMap r2
-}

-- | Convert a VCD file to a VCD object.
fromVCD :: String -> Signature -> VCD
fromVCD vcd sig = VCD $ [ ("inputs/" ++ nm,TraceStream ty $ fromList $ fromJust $ lookup nm streams) | (nm,ty) <- sigInputs sig ]
                      ++ [ ("outputs/" ++ nm,TraceStream ty $ fromList $ fromJust $ lookup nm streams) | (nm,ty) <- sigOutputs sig ]
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
         -> ([(VCDID,RepValue)],[String]) -- ^ map of vcdIds to initial values
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

-- | Convert a 'VCD' to a VCD file.
toVCD :: Bool    -- ^ Whether to include the clock signal in the list of signals
      -> Integer -- ^ Timescale in nanoseconds
      -> VCD
      -> String
toVCD _incClk ts (VCD m) = unlines
    [ "$version\n   Kansas Lava\n$end"
    , "$timescale " ++ show ts ++ "ns $end"
    , "$scope top $end"
    ]
    ++ unlines [ unwords ["$var wire", show $ typeWidth ty, ident, show k, "$end"]
               | (ident,(k,TraceStream ty _)) <- signals ]
    ++ "$enddefinitions $end\n"
    ++ values [ (i',strm) | (i',(_,TraceStream _ strm)) <- signals ]

    where signals = zip vcdIds m

type VCDID = String
-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
vcdIds :: [VCDID]
vcdIds = map code [0..]
    where code :: Int -> VCDID
          code i | i < 0 = ""
          code i         = chr (33 + mod i 94) : code (div i 94 - 1)

values :: [(VCDID, EventList RepValue)] -> String
values sigs = dumpVars initials ++ eventList rest
    where (initials,rest) = unzip [ ((i, v), (i, EL xs)) | (i, EL ((_,v):xs)) <- sigs ]
--          rest = [ (i, EL xs') | (i, EL xs) <- sigs
--                               , let xs' = dropWhile ((== 0) . fst) xs ]

dumpVars :: [(VCDID, RepValue)] -> String
dumpVars vals = "$dumpvars\n" ++ unlines (map (uncurry vcdVal) vals) ++ "$end\n"

eventList :: [(VCDID, EventList RepValue)] -> String
eventList strms = unlines [ "#" ++ show i ++ "\n" ++ ls | (i,ls) <- evs ]
    where (EL evs) = mergeWithEL (\s1 s2 -> s1 ++ ('\n':s2))
                                 [ fmap (vcdVal ident) elist | (ident,elist) <- strms ]

vcdVal :: VCDID -> RepValue -> String
vcdVal i r@(RepValue bs) | length bs == 1 = rep2tbw r ++ i
                         | otherwise      = "b" ++ rep2tbw r ++ " " ++ i

-- | A list of changes, indexed from 0
newtype EventList a = EL { unEL :: [(Int,a)] }
    deriving (Eq,Show,Read)

instance (Ord a) => Ord (EventList a) where
    compare exs eys = compare (toList exs) (toList eys)

instance Functor EventList where
    fmap f (EL evs) = EL [ (i,f v) | (i,v) <- evs ]

{-
instance (Show a) => Show (EventList a) where
    show = show . toList

instance (Eq a, Read a) => Read (EventList a) where
    readsPrec p str = [ (fromList l,r) | (l,r) <- readsPrec p str ]
-}

toList :: EventList a -> [a]
toList = toList' $ error "toList: no initial value!"

-- | Like toList, but accepts initial value for case that
-- first event is not at timestep 0
toList' :: a -> EventList a -> [a]
toList' iv (EL xs) = go (0,iv) xs
    where go _      []          = []
          go (p,px) ((i,x):xs') = replicate (i-p) px ++ go (i,x) xs'

fromList :: (Eq a) => [a] -> EventList a
fromList xs = EL (go (0,undefined) xs)
    where go :: (Eq a) => (Int,a) -> [a] -> [(Int,a)]
          go (0,_) [] = []
          go (i,p) [] = [(i,p)] -- this tracks length, value is thrown away by toList
          go (i,p) (x:xs') | checkpoint i || p /= x = (i,x) : go (i+1,x) xs'
                           | otherwise              =         go (i+1,x) xs'

          checkpoint = (== 0) . (`mod` 1000)

lengthEL :: EventList a -> Int
lengthEL (EL []) = 0
lengthEL (EL xs) = fst $ last xs

takeEL :: Int -> EventList a -> EventList a
takeEL i (EL evs) = EL $ evs' ++ final
    where evs' = takeWhile ((<= i) . fst) evs
          final = [(i, case evs' of [] -> undefined; _ -> snd $ last evs')]

zipWithEL :: (Eq c) => (a -> b -> c) -> EventList a -> EventList b -> EventList c
-- zipWithEL f xs ys = fromList $ zipWith f (toList xs) (toList ys)
zipWithEL f xs ys = EL $ go (ea,eb) (unEL $ takeEL l xs) (unEL $ takeEL l ys)
    where l = min (lengthEL xs) (lengthEL ys)
          trunc = takeWhile ((<= l) . fst)
          ea = error "zipWithEL: no initial value in a-list"
          eb = error "zipWithEL: no initial value in b-list"
          go (pa,_) [] bs = [ (i,f pa b) | (i,b) <- trunc bs ]
          go (_,pb) as [] = [ (i,f a pb) | (i,a) <- trunc as ]
          go (pa,pb) ((i,a):as) ((i',b):bs) | i < i'    = (i ,f a  pb) : go (a,pb) as         ((i',b):bs)
                                            | i == i'   = (i ,f a  b ) : go (a,b ) as         bs
                                            | otherwise = (i',f pa b ) : go (pa,b) ((i,a):as) bs

mergeWithEL :: (Eq a) => (a -> a -> a) -> [EventList a] -> EventList a
-- mergeWithEL f = fromList . mergeWith f . map toList
mergeWithEL _ [] = fromList []
mergeWithEL f ls = foldr1 (zipWithEL f) ls

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
        truncTS (TraceStream ty (EL s)) = TraceStream ty $ takeEL c $ EL s
        tr = VCD $ [ ("inputs/" ++ nm, truncTS $ padToTraceStream p)
                   | (nm,_) <- theSrcs rc
                   , (nm',p) <- input
                   , nm == nm' ]
                 ++ [ ("outputs/" ++ nm, truncTS $ padToTraceStream p)
                    | (nm,_,_) <- theSinks rc
                    , (nm',p) <- output
                    , nm == nm' ]

    return (tr, rc)

padToTraceStream :: Pad -> TraceStream
padToTraceStream (StdLogic s) = toTraceStream $ shallowS s
padToTraceStream (StdLogicVector s) = toTraceStream $ shallowS s
padToTraceStream other = error $ "fix padToTraceStream for " ++ show other

-- basic conversion to trace representation
-- | Convert a Stream to a TraceStream.
toTraceStream :: forall w . (Rep w) => S.Stream (X w) -> TraceStream
toTraceStream stream = TraceStream (repType (Witness :: Witness w)) $ fromList $ map toRep $ S.toList stream

-- | 'cmpTraceStream' compares two traces to determine equivalence. Note this
-- uses 'cmpRepValue' under the hood, so the first argument is considered the
-- golden trace.
cmpTraceStream :: Int -> TraceStream -> TraceStream -> Bool
cmpTraceStream count (TraceStream t1 s1) (TraceStream t2 s2) = t1 == t2 && countLTs1 && s1LTs2 && eql
    where countLTs1 = count <= lengthEL s1
          s1LTs2 = lengthEL s1 <= lengthEL s2
          eql = and $ map snd . unEL $ zipWithEL cmpRepValue (takeEL count s1) (takeEL count s2)

scope :: VCD -> String -> [(String,TraceStream)]
scope vcd s = scopes vcd [s]

scopes :: VCD -> [String] -> [(String,TraceStream)]
scopes (VCD m) s = [ (nm,ts) | (nm,ts) <- m
                             , s' <- s
                             , s' `isPrefixOf` nm ]

inputs :: VCD -> [(String,TraceStream)]
inputs = flip scope "inputs"

outputs :: VCD -> [(String,TraceStream)]
outputs = flip scope "outputs"

-- | Generate a signature from a trace.
-- TODO: support generics in both these functions?
toSignature :: VCD -> Signature
toSignature vcd = Signature (convert $ inputs vcd) (convert $ outputs vcd) []
    where convert m = [ (dropModName nm,ty) | (nm,TraceStream ty _) <- m ]
          dropModName = reverse . takeWhile (/= '/') . reverse

-- | Creates an (empty) trace from a signature
fromSignature :: Signature -> VCD
fromSignature (Signature inps outps _) = VCD $ convert "inputs" inps ++ convert "outputs" outps
    where convert mnm l = [ (mnm ++ "/" ++ nm, TraceStream ty $ fromList [])  | (nm, ty) <- l ]

-- | Compare two trace objects. First argument is the golden value. See notes for cmpRepValue
cmpVCD :: VCD -> VCD -> Bool
cmpVCD (VCD m1) (VCD m2) =
    and [ k1 == k2 && cmpTraceStream (tslen s1) s1 s2
        | ((k1,s1),(k2,s2)) <- zip (sorted m1) (sorted m2)
        ]
    where tslen (TraceStream _ el) = lengthEL el
          sorted = sortBy ((compare) `on` fst)

-- | Like cmpVCD but only compares inputs and outputs.
cmpVCDIO :: VCD -> VCD -> Bool
cmpVCDIO vcd1 vcd2 = cmpVCD (VCD $ inputs vcd1 ++ outputs vcd1)
                            (VCD $ inputs vcd2 ++ outputs vcd2)

-- Functions below are not exported.
{-
readMap :: [String] -> ([(String,TraceStream)], [String])
readMap ls = (go thismap, rest)
    where cond = (not . (flip elem) ["INPUTS","OUTPUTS","PROBES","END"])
          (thismap, rest) = span cond ls
          tabsplit l = let (k,'\t':r1) = span (/= '\t') l
                           (ty,'\t':r) = span (/= '\t') r1
                       in (k,ty,r)
          go :: [String] -> [(String,TraceStream)]
          go = map (\l -> let (k,ty,strm) = tabsplit l
                          in (k,TraceStream (read ty) $ fromList [read v | v <- words strm])
                   )
-}

--------------------------------------------

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
                   | (_,TraceStream ty _) <- inputs et ++ outputs et
                   ]
          (inSigs, outSigs) = splitAt (length $ inputs et) $ splitLists ilines widths
          addToMap sigs m = [ (k,TraceStream ty $ fromList $ map tbw2rep strm)
                            | (strm,(k,TraceStream ty _)) <- zip sigs m
                            ]
          (ins, outs) = (addToMap inSigs $ inputs et, addToMap outSigs $ outputs et)

-- | Convert a VCD into a list of lists of Strings, each String is a value,
-- each list of Strings is a signal.
asciiStrings :: VCD -> [[String]]
asciiStrings vcd = [ toList $ fmap rep2tbw s | TraceStream _ s <- insOuts ]
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
