{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
-- | This module contains functions for manipulating (extending, querying, modifying) debugging Traces. It also provides functionality for (de)serializing Traces.
module Language.KansasLava.Trace
    ( VCD(..)
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

import Data.Function
import Data.List
-- import Data.Maybe

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
        truncTS (TraceStream ty s) = TraceStream ty $ take c s
        tr = VCD { inputs = [ (nm, truncTS $ padToTraceStream p)
                            | (nm,_) <- theSrcs rc
                            , (nm',p) <- input
                            , nm == nm'
                            ]
                 , outputs = [ (nm, truncTS $ padToTraceStream p)
                             | (nm,_,_) <- theSinks rc
                             , (nm',p) <- output
                             , nm == nm'
                             ]
                 , probes = []
                 }

    return (tr, rc)

-- | 'VCD' is a primary bit-wise record of an interactive session with some circuit
-- The inputs and outputs are in the order of the parent KLEG.
data VCD = VCD { inputs :: [(String,TraceStream)]
               , outputs :: [(String,TraceStream)]
               , probes :: [(String,TraceStream)]
               }

-- instances for VCD
instance Show VCD where
    show (VCD ins outs ps) = unlines
                           $ ["INPUTS"]
                          ++ boxIn (showMap ins)
                          ++ ["OUTPUTS"]
                          ++ boxIn (showMap outs)
                          ++ ["PROBES"]
                          ++ boxIn (showMap ps)
                          ++ ["END"]
        where showMap :: [(String,TraceStream)] -> [String]
              showMap m = [intercalate "\t" [k, show ty, showStrm strm] | (k,TraceStream ty strm) <- m]
              showStrm s = unwords [concatMap ((showRep) . XBool) $ val | RepValue val <- s]

              boxIn = take 20 . map (take 75)

instance Read VCD where
    readsPrec _ str = [(VCD { inputs = ins, outputs = outs, probes = ps },unlines rest)]
        where ("INPUTS":ls) = lines str
              (ins,"OUTPUTS":r1) = readMap ls
              (outs,"PROBES":r2) = readMap r1
              (ps,"END":rest) = readMap r2

-- | Two traces are equal if they have the same length and all the streams are equal over that length
instance Eq VCD where
    (==) (VCD i1 o1 p1) (VCD i2 o2 p2) = insEqual && outEqual && probesEqual
        where sorted = sortBy ((compare) `on` fst)
              insEqual = sorted i1 == sorted i2
              outEqual = sorted o1 == sorted o2
              probesEqual = sorted p1 == sorted p2

-- | 'cmpTraceStream' compares two traces to determine equivalence. Note this
-- uses 'cmpRepValue' under the hood, so the first argument is considered the
-- golden trace.
cmpTraceStream :: Int -> TraceStream -> TraceStream -> Bool
cmpTraceStream count (TraceStream t1 s1) (TraceStream t2 s2) = t1 == t2 && countLTs1 && s1LTs2 && eql
    where countLTs1 = count <= (length $ take count s1)
          s1LTs2 = (length $ take count s1) <= (length $ take count s2)
          eql = and $ take count $ zipWith cmpRepValue s1 s2

-- | Generate a signature from a trace.
-- TODO: support generics in both these functions?
toSignature :: VCD -> Signature
toSignature (VCD ins outs _) = Signature (convert ins) (convert outs) []
    where convert m = [ (nm,ty) | (nm,TraceStream ty _) <- m ]

-- | Creates an (empty) trace from a signature
fromSignature :: Signature -> VCD
fromSignature (Signature inps outps _) = VCD (convert inps) (convert outps) []
    where convert l = [ (nm, TraceStream ty [])  | (nm, ty) <- l ]

-- | Compare two trace objects. First argument is the golden value. See notes for cmpRepValue
cmpVCD :: VCD -> VCD -> Bool
cmpVCD (VCD i1 o1 p1) (VCD i2 o2 p2) =
    and [ k1 == k2 && cmpTraceStream (tslen s1) s1 s2
        | (m1, m2) <- zip [i1,o1,p1] [i2,o2,p2]
        , ((k1,s1),(k2,s2)) <- zip m1 m2
        ]

    where tslen (TraceStream _ s) = length s

-- | Like cmpVCD but only compares inputs and outputs.
cmpVCDIO :: VCD -> VCD -> Bool
cmpVCDIO (VCD i1 o1 _) (VCD i2 o2 _) = cmpVCD (VCD i1 o1 []) (VCD i2 o2 [])

-- Functions below are not exported.

readMap :: [String] -> ([(String,TraceStream)], [String])
readMap ls = (go thismap, rest)
    where cond = (not . (flip elem) ["INPUTS","OUTPUTS","PROBES","END"])
          (thismap, rest) = span cond ls
          tabsplit l = let (k,'\t':r1) = span (/= '\t') l
                           (ty,'\t':r) = span (/= '\t') r1
                       in (k,ty,r)
          go :: [String] -> [(String,TraceStream)]
          go = map (\l -> let (k,ty,strm) = tabsplit l
                          in (k,TraceStream (read ty) [read v | v <- words strm])
                   )

padToTraceStream :: Pad -> TraceStream
padToTraceStream (StdLogic s) = toTraceStream $ shallowS s
padToTraceStream (StdLogicVector s) = toTraceStream $ shallowS s
padToTraceStream other = error $ "fix padToTraceStream for " ++ show other

-- basic conversion to trace representation
-- | Convert a Stream to a TraceStream.
toTraceStream :: forall w . (Rep w) => S.Stream (X w) -> TraceStream
toTraceStream stream = TraceStream (repType (Witness :: Witness w)) [toRep xVal | xVal <- S.toList stream ]

--------------------------------------------

-- | Convert the inputs and outputs of a VCD to the textual format expected
-- by a testbench.
writeTBF :: String -> VCD -> IO ()
writeTBF filename = writeFile filename . unlines . mergeWith (++) . asciiStrings

-- | Inverse of showTBF, needs a signature for the shape of the desired VCD.
-- Creates a VCD from testbench signal files.
readTBF :: [String] -> Signature -> VCD
readTBF ilines sig = et { inputs = ins, outputs = outs }
    where et = fromSignature sig
          widths = [ typeWidth ty
                   | (_,TraceStream ty _) <- inputs et ++ outputs et
                   ]
          (inSigs, outSigs) = splitAt (length $ inputs et) $ splitLists ilines widths
          addToMap sigs m = [ (k,TraceStream ty $ map tbw2rep strm)
                            | (strm,(k,TraceStream ty _)) <- zip sigs m
                            ]
          (ins, outs) = (addToMap inSigs $ inputs et, addToMap outSigs $ outputs et)

-- | Convert a VCD into a list of lists of Strings, each String is a value,
-- each list of Strings is a signal.
asciiStrings :: VCD -> [[String]]
asciiStrings (VCD ins outs _) = [ map rep2tbw s
                                | (_,TraceStream _ s) <- ins ++ outs ]

-- | Convert string representation used in testbench files to a RepValue
-- Note the reverse here is crucial due to way vhdl indexes stuff
tbw2rep :: String -> RepValue
tbw2rep vals = RepValue [ case v of
                            'X' -> Nothing
                            '1' -> Just True
                            '0' -> Just False
                            _   -> error "tbw2rep: bad character!"
                        | v <- reverse vals ]

-- | Convert a RepValue to the string representation used in testbench files
rep2tbw :: RepValue -> String
rep2tbw (RepValue vals) = [ case v of
                              Nothing   -> 'X'
                              Just True  -> '1'
                              Just False -> '0'
                          | v <- reverse vals ]
