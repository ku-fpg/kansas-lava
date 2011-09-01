{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
-- | This module contains functions for manipulating (extending, querying, modifying) debugging Traces. It also provides functionality for (de)serializing Traces.
module Language.KansasLava.Trace
    ( Trace(..)
    , toSignature
    , fromSignature
    , setCycles
    , addInput
    , getInput
    , remInput
    , addOutput
    , getOutput
    , remOutput
    , addProbe
    , getProbe
    , remProbe
    , cmpTrace
    , cmpTraceIO
    , diff
    , emptyTrace
    , takeTrace
    , dropTrace
    , serialize
    , deserialize
    , writeToFile
    , readFromFile
    , mkTrace
    , mkTraceCM
    ) where

import Language.KansasLava.Comb
import Language.KansasLava.Fabric
import Language.KansasLava.Probes
import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Types
import Language.KansasLava.Utils

import qualified Language.KansasLava.Stream as S

import Control.Monad

import Data.List
import Data.Maybe

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
    rc <- (reifyFabric >=> mergeProbesIO >=> circuitMod) fabric

    let (_,output) = runFabric fabric input
        tr = Trace { len = c
                   , inputs = [ (OVar 0 nm, padToTraceStream p)
                              | (OVar _ nm,_) <- theSrcs rc
                              , (nm',p) <- input
                              , nm == nm'
                              ]
                   , outputs = [ (OVar 0 nm, padToTraceStream p)
                               | (OVar _ nm,_,_) <- theSinks rc
                               , (nm',p) <- output
                               , nm == nm'
                               ]
                   , probes = []
                   }

    return (addProbes rc tr, rc)

-- | 'Trace' is a primary bit-wise record of an interactive session with some circuit
-- The inputs and outputs are in the order of the parent KLEG.
data Trace = Trace { len :: Maybe Int
                   , inputs :: [(OVar,TraceStream)]
                   , outputs :: [(OVar,TraceStream)]
                   , probes :: [(OVar,TraceStream)]
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

instance Rep a => Traceable (CSeq c a) where
    getSignal ts = shallowSeq $ fromTrace ts

instance Rep a => Traceable (Comb a) where
    getSignal ts = shallowComb $ S.head $ fromTrace ts

-- instance Functor TraceStream where -- can we do this with proper types?

-- | Generate a signature from a trace.
-- TODO: support generics in both these functions?
toSignature :: Trace -> Signature
toSignature (Trace _ ins outs _) = Signature (convert ins) (convert outs) []
    where convert m = [ (ovar,ty) | (ovar,TraceStream ty _) <- m ]

-- | Creates an (empty) trace from a signature
fromSignature :: Signature -> Trace
fromSignature (Signature inps outps _) = Trace Nothing (convert inps) (convert outps) []
    where convert l = [ (ovar, TraceStream ty [])  | (ovar, ty) <- l ]

-- Combinators to change a trace
-- | Set the length of the trace, in cycles.
setCycles :: Int -> Trace -> Trace
setCycles i t = t { len = Just i }

-- | Add a named input to a Trace.
addInput :: forall a. (Rep a) => OVar -> Seq a -> Trace -> Trace
addInput key iseq t@(Trace _ ins _ _) = t { inputs = addSeq key iseq ins }

-- | Get a named input from a Trace.
getInput :: (Rep w) => OVar -> Trace -> Seq w
getInput key trace = getSignal $ fromJust $ lookup key (inputs trace)

-- | Remove a named input from a Trace.
remInput :: OVar -> Trace -> Trace
remInput key t@(Trace _ ins _ _) = t { inputs = filter ((== key) . fst) ins }

-- | Add a named output to a Trace.
addOutput :: forall a. (Rep a) => OVar -> Seq a -> Trace -> Trace
addOutput key iseq t@(Trace _ _ outs _) = t { outputs = addSeq key iseq outs }

-- | Get a named output from a Trace
getOutput :: (Rep w) => OVar -> Trace -> Seq w
getOutput key trace = getSignal $ fromJust $ lookup key (outputs trace)

-- | Remove a named output from a Trace.
remOutput :: OVar -> Trace -> Trace
remOutput key t@(Trace _ _ outs _) = t { outputs = filter ((== key) . fst) outs }

-- | Add a named internal probe to a Trace.
addProbe :: forall a. (Rep a) => OVar -> Seq a -> Trace -> Trace
addProbe key iseq t@(Trace _ _ _ ps) = t { probes = addSeq key iseq ps }

-- | Get a named internal probe from a Trace.
getProbe :: (Rep w) => OVar -> Trace -> Seq w
getProbe key trace = getSignal $ fromJust $ lookup key (probes trace)

-- | Remove a named internal probe from a Trace.
remProbe :: OVar -> Trace -> Trace
remProbe key t@(Trace _ _ _ ps) = t { probes = filter ((== key) . fst) ps }

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

-- | Get the first i elements of a Trace.
takeTrace :: Int -> Trace -> Trace
takeTrace i t = t { len = Just newLen }
    where newLen = case len t of
                    Just x -> min i x
                    Nothing -> i

-- | Drop the first i elements of a Trace.
dropTrace :: Int -> Trace -> Trace
dropTrace i t@(Trace c ins outs ps)
    | newLen > 0 = t { len = Just newLen
                     , inputs = dropStream ins
                     , outputs = dropStream outs
                     , probes = dropStream ps }
    | otherwise = emptyTrace
    where dropStream m = [ (k,TraceStream ty (drop i s)) | (k,TraceStream ty s) <- m ]
          newLen = maybe i (\x -> x - i) c

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
    where showMap :: [(OVar,TraceStream)] -> [String]
          showMap m = [intercalate "\t" [show k, show ty, showStrm strm] | (k,TraceStream ty strm) <- m]
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

readMap :: [String] -> ([(OVar,TraceStream)], [String])
readMap ls = (go thismap, rest)
    where cond = (not . (flip elem) ["INPUTS","OUTPUTS","PROBES","END"])
          (thismap, rest) = span cond ls
          tabsplit l = let (k,'\t':r1) = span (/= '\t') l
                           (ty,'\t':r) = span (/= '\t') r1
                       in (k,ty,r)
          go :: [String] -> [(OVar,TraceStream)]
          go = map (\l -> let (k,ty,strm) = tabsplit l
                          in (read k,TraceStream (read ty) [read v | v <- words strm])
                   )

addStream :: forall w. (Rep w) => OVar -> [(OVar,TraceStream)] -> S.Stream (X w) -> [(OVar,TraceStream)]
addStream key m stream = m ++ [(key,toTrace stream)]

addSeq :: forall w. (Rep w) => OVar -> Seq w -> [(OVar,TraceStream)] -> [(OVar,TraceStream)]
addSeq key iseq m = addStream key m (seqValue iseq :: S.Stream (X w))

padToTraceStream :: Pad -> TraceStream
padToTraceStream (StdLogic s) = toTrace $ seqValue s
padToTraceStream (StdLogicVector s) = toTrace $ seqValue s
padToTraceStream other = error $ "fix padToTraceStream for " ++ show other

-- | Used by 'mkTraceCM' to add internal probes to the Trace.
addProbes :: KLEG -> Trace -> Trace
addProbes rc t = t { probes = ps }
    where pdata = [ (nid,k,v) | (nid,Entity (TraceVal ks v) _ _) <- theCircuit rc, k <- ks ]
          ps = [ (OVar nid nm, strm) | (nid, OVar _ nm, strm) <- pdata ]
