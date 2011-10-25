{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
-- | This module contains functions for manipulating (extending, querying, modifying) debugging Traces. It also provides functionality for (de)serializing Traces.
module Language.KansasLava.Trace
    ( Trace(..)
    , toSignature
    , fromSignature
    , cmpTrace
    , cmpTraceIO
    , diff
    , emptyTrace
    , serialize
    , deserialize
    , writeToFile
    , readFromFile
    , mkTrace
    , mkTraceCM
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

import Data.List
-- import Data.Maybe

-- | Make a 'Trace' from a 'Fabric' and its input.
mkTrace :: Maybe Int      -- ^ Nothing means infinite trace, Just x sets trace length to x cycles.
        -> Fabric ()      -- ^ The Fabric we are tracing
        -> [(String,Pad)] -- ^ Inputs to the Fabric
        -> IO Trace
mkTrace c fabric input = do
    (trace, _) <- mkTraceCM c fabric input (return)
    return trace

-- | Version of 'mkTrace' that accepts arbitrary circuit mods.
mkTraceCM :: Maybe Int               -- ^ Nothing means infinite trace, Just x sets trace length to x cycles.
          -> Fabric ()               -- ^ Fabric we are tracing
          -> [(String, Pad)]         -- ^ Inputs to the Fabric
          -> (KLEG -> IO KLEG) -- KLEG Mod
          -> IO (Trace, KLEG)
mkTraceCM c fabric input circuitMod = do
    rc <- (reifyFabric >=> circuitMod) fabric

    let (_,output) = runFabric fabric input
        tr = Trace { len = c
                   , inputs = [ (nm, padToTraceStream p)
                              | (nm,_) <- theSrcs rc
                              , (nm',p) <- input
                              , nm == nm'
                              ]
                   , outputs = [ (nm, padToTraceStream p)
                               | (nm,_,_) <- theSinks rc
                               , (nm',p) <- output
                               , nm == nm'
                               ]
                   , probes = []
                   }

    return (addProbes rc tr, rc)

-- | 'Trace' is a primary bit-wise record of an interactive session with some circuit
-- The inputs and outputs are in the order of the parent KLEG.
data Trace = Trace { len :: Maybe Int
                   , inputs :: [(String,TraceStream)]
                   , outputs :: [(String,TraceStream)]
                   , probes :: [(String,TraceStream)]
                   }

-- instances for Trace
instance Show Trace where
    show = serialize

instance Read Trace where
    readsPrec _ = deserialize

-- | Two traces are equal if they have the same length and all the streams are equal over that length
instance Eq Trace where
    (==) (Trace c1 i1 o1 p1) (Trace c2 i2 o2 p2) = (c1 /= Nothing || c2 /= Nothing) && (c1 == c2) && insEqual && outEqual && probesEqual
        where sorted m = [(k,TraceStream ty $ takeMaybe c1 s) | (k,TraceStream ty s) <- m]
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

-- | To turn a TraceStream back into a shallow KL signal.
class Traceable a where
    getSignal :: TraceStream -> a

instance (Clock c, Rep a) => Traceable (Signal c a) where
    getSignal ts = mkShallowS $ fromTraceStream ts

-- instance Functor TraceStream where -- can we do this with proper types?

-- | Generate a signature from a trace.
-- TODO: support generics in both these functions?
toSignature :: Trace -> Signature
toSignature (Trace _ ins outs _) = Signature (convert ins) (convert outs) []
    where convert m = [ (nm,ty) | (nm,TraceStream ty _) <- m ]

-- | Creates an (empty) trace from a signature
fromSignature :: Signature -> Trace
fromSignature (Signature inps outps _) = Trace Nothing (convert inps) (convert outps) []
    where convert l = [ (nm, TraceStream ty [])  | (nm, ty) <- l ]

-- | Compare two trace objects. First argument is the golden value. See notes for cmpRepValue
cmpTrace :: Trace -> Trace -> Bool
cmpTrace (Trace Nothing _ _ _)     _                           = False
cmpTrace (Trace c1 _ _ _)          (Trace c2 _ _ _) | c1 /= c2 = False
cmpTrace (Trace (Just c) i1 o1 p1) (Trace _ i2 o2 p2)          =
    and [ k1 == k2 && cmpTraceStream c s1 s2
        | (m1, m2) <- zip [i1,o1,p1] [i2,o2,p2]
        , ((k1,s1),(k2,s2)) <- zip m1 m2
        ]

-- | Like cmpTrace but only compares inputs and outputs.
cmpTraceIO :: Trace -> Trace -> Bool
cmpTraceIO (Trace c1 i1 o1 _) (Trace c2 i2 o2 _) = cmpTrace (Trace c1 i1 o1 []) (Trace c2 i2 o2 [])

-- something more intelligent someday?
-- | Determine if two traces are equal.
diff :: Trace -> Trace -> Bool
diff t1 t2 = t1 == t2

-- | A default, empty Trace.
emptyTrace :: Trace
emptyTrace = Trace { len = Nothing, inputs = [], outputs = [], probes = [] }

-- | Convert a trace to a textual form.
serialize :: Trace -> String
serialize (Trace c ins outs ps) = unlines
                                $ [show c, "INPUTS"]
                               ++ boxIn (showMap ins)
                               ++ ["OUTPUTS"]
                               ++ boxIn (showMap outs)
                               ++ ["PROBES"]
                               ++ boxIn (showMap ps)
                               ++ ["END"]
    where showMap :: [(String,TraceStream)] -> [String]
          showMap m = [intercalate "\t" [k, show ty, showStrm strm] | (k,TraceStream ty strm) <- m]
          showStrm s = unwords [concatMap ((showRep) . XBool) $ val | RepValue val <- takeMaybe c s]

          boxIn = take 20 . map (take 75)



-- | Parse a textual representation of a Trace. Return the Trace and the remainder of the unparsed output.
deserialize :: String -> [(Trace,String)]
deserialize str = [(Trace { len = read cstr, inputs = ins, outputs = outs, probes = ps },unlines rest)]
    where (cstr:"INPUTS":ls) = lines str
          (ins,"OUTPUTS":r1) = readMap ls
          (outs,"PROBES":r2) = readMap r1
          (ps,"END":rest) = readMap r2

-- | Serialize a Trace to a file.
writeToFile :: FilePath -> Trace -> IO ()
writeToFile fp t = writeFile fp $ serialize t

-- | Deserialize a Trace from a file.
readFromFile :: FilePath -> IO Trace
readFromFile fp = do
    str <- readFile fp
    return $ fst $ head $ deserialize str

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

-- | Used by 'mkTraceCM' to add internal probes to the Trace.
addProbes :: KLEG -> Trace -> Trace
addProbes rc t = t { probes = ps }
    where pdata = [ (nid,k,v) | (nid,Entity (TraceVal ks v) _ _) <- theCircuit rc, k <- ks ]
          ps = [ (show nid ++ nm, strm) | (nid, nm, strm) <- pdata ]

-- basic conversion to trace representation
-- | Convert a Stream to a TraceStream.
toTraceStream :: forall w . (Rep w) => S.Stream (X w) -> TraceStream
toTraceStream stream = TraceStream (repType (Witness :: Witness w)) [toRep xVal | xVal <- S.toList stream ]

-- | Convert a TraceStream to a Stream.
fromTraceStream :: (Rep w) => TraceStream -> S.Stream (X w)
fromTraceStream (TraceStream _ list) = S.fromList [fromRep val | val <- list]

--------------------------------------------

-- | Convert the inputs and outputs of a Trace to the textual format expected
-- by a testbench.
writeTBF :: String -> Trace -> IO ()
writeTBF filename = writeFile filename . unlines . mergeWith (++) . asciiStrings

-- | Inverse of showTBF, needs a signature for the shape of the desired Trace.
-- Creates a Trace from testbench signal files.
readTBF :: [String] -> Signature -> Trace
readTBF ilines sig = et { inputs = ins, outputs = outs }
    where et = (fromSignature sig) { len = Just (length ilines) }
          widths = [ typeWidth ty
                   | (_,TraceStream ty _) <- inputs et ++ outputs et
                   ]
          (inSigs, outSigs) = splitAt (length $ inputs et) $ splitLists ilines widths
          addToMap sigs m = [ (k,TraceStream ty $ map tbw2rep strm)
                            | (strm,(k,TraceStream ty _)) <- zip sigs m
                            ]
          (ins, outs) = (addToMap inSigs $ inputs et, addToMap outSigs $ outputs et)

-- | Convert a Trace into a list of lists of Strings, each String is a value,
-- each list of Strings is a signal.
asciiStrings :: Trace -> [[String]]
asciiStrings (Trace c ins outs _) = [ map rep2tbw $ takeMaybe c s
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
