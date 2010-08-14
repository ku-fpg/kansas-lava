{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
module Language.KansasLava.Trace where

import Language.KansasLava
import qualified Language.KansasLava.Testing.Compare as C -- remove me someday!

import qualified Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned

import qualified Data.Map as M
import Data.Maybe

type TraceMap a = M.Map a (BaseTy,[[X Bool]]) -- to recover type, eventually clock too?
type TraceStream a = Stream (Maybe a)

data Trace = Trace { cycles :: Int
                   , inputs :: TraceMap PadVar
                   , outputs :: TraceMap PadVar
                   , probes :: TraceMap PadVar
--                   , circuit :: ReifiedCircuit
--                   , opts :: DebugOpts -- can see a case for this eventually
                   -- what else? keep the vhdl here?
                   }

-- Some combinators to get stuff in and out of the map
streamToSeq :: (Wire a) => TraceStream a -> Seq a
streamToSeq s = shallowSeq $ fmap optX s

seqToStream :: (Wire a) => Seq a -> TraceStream a
seqToStream s = fmap unX $ seqValue s

fromXStream :: forall w. (RepWire w) => w -> Stream (X w) -> [[X Bool]]
fromXStream witness stream = [Matrix.toList $ fromWireXRep witness xVal | xVal <- toList stream ]

toXStream :: forall w. (RepWire w) => w -> [[X Bool]] -> Stream (X w)
toXStream witness list = fromList [toWireXRep witness $ Matrix.fromList val | val <- list]

getStream :: forall a w. (Ord a, RepWire w) => a -> TraceMap a -> w -> Stream (X w)
getStream name m witness = case M.lookup name m of
                        Just (ty,rep) -> toXStream witness rep
--                        Just (ty,strReps) -> fromList $ map (readRepWire ty) strReps

getSeq :: (Ord a, RepWire w) => a -> TraceMap a -> w -> Seq w
getSeq key m witness = shallowSeq $ getStream key m witness

addStream :: forall a w. (Ord a, RepWire w) => a -> TraceMap a -> w -> Stream (X w) -> TraceMap a
addStream key m witness stream = M.insert key (ty,rep) m
    where ty = wireType witness
          rep = fromXStream witness stream
--          strRep = [showRepWire witness (optX x :: X b) | x <- toList stream]

addSeq :: forall a b. (Ord a, RepWire b) => a -> Seq b -> TraceMap a -> TraceMap a
addSeq key seq m = addStream key m witness (seqValue seq :: Stream (X b))
    where witness = error "addSeq" :: b

-- instances for Trace
instance Show Trace where
    show (Trace c i o p) = unlines $ concat [[show c,"inputs"], printer i, ["outputs"], printer o, ["probes"], printer p]
        where printer strm = map show $ zip (M.keys strm) [(ty,take 10 val) | (ty,val) <- M.elems strm]

instance Eq Trace where
    (==) (Trace c1 i1 o1 p1) (Trace c2 i2 o2 p2) = c1 == c2

{- combinators for working with traces
-- should be easy to build using probes... especially once probes store data in same way
mkTrace :: (Ports a, Probe a, Ports b) => Int -> a -> (a -> b) -> IO Trace
mkTrace cycles circuit apply = do
    rc <- reifyCircuit [] $ apply circuit -- this is essentially what probeCircuit does
                                          -- but we want the unique node id's too

-- make use of probeCircuit
mkTraceProbes :: (..) => [(String,Annotation)] -> Trace

-- assuming ReifiedCircuit has probe data in it
mkTraceRC :: (..) => ReifiedCircuit -> Trace

-- run a circuit with a trace, get a new trace (same inputs, possibly diff outputs) back
runTrace :: (..) => a -> Trace -> Trace

-- this is just Eq, but maybe instead of Bool some kind of detailed diff
diff :: Trace -> Trace -> Bool
takeTrace :: Int -> Trace -> Trace -- truncate a trace
dropTrace :: Int -> Trace -> Trace -- skip into a trace?
unionTrace :: Trace -> Trace -> Trace
remove :: PadVar -> Trace -> Trace

-- serialization
writeTrace :: (..) => FilePath -> Trace -> IO ()
readTrace :: (..) => FilePath -> IO Trace

-- testing?

-- take a trace and a path, compare trace to other serialized traces residing in that path
unitTest :: FilePath -> Trace -> IO Bool -- or some rich exit code
unitTest' :: (..) => FilePath -> a -> (a -> b) -> IO (Bool, b) -- same
thing, but builds trace internally

-- create tarball for modelsim, much like mkTestbench
genSim :: FilePath -> Trace -> IO ()

-- create tarball for use on togo, including the SimpleWrapper
genWrapper :: FilePath -> Trace -> IO ()

-- output formats
latexWaveform :: Trace -> String
truthTable :: Trace -> String -- or some truth table structure
vcd :: Trace -> String -- or maybe FilePath -> Trace -> IO ()
vhdl :: Trace -> String
-}

main = do
    let pv1 = PadVar 0 "blah"
        pv2 = PadVar 1 "bleh"
        ov1 = PadVar 0 "out1"
        ov2 = PadVar 99 "out2"

        -- make some Seqs
        seq1 = toSeq $ cycle [True, False]
        seq2 = toSeq' $ cycle [Nothing, Just True, Just False]
        seq3 :: Seq U4
        seq3 = toSeq $ cycle [0..15]

        -- now stick them in the maps
        inmap = addSeq pv1 seq1 $ addSeq pv2 seq2 $ addSeq (PadVar 3 "ints") seq3 $ M.empty
        outmap = addSeq ov1 seq1 $ addSeq ov2 seq2 M.empty

        -- and create a whole trace
        trace = Trace { cycles = 100, inputs = inmap, outputs = outmap, probes = M.empty } -- , circuit = ReifiedCircuit [] [] [] }

        witness = error "witness" :: Bool
        witness2 = error "witness" :: U4

    print trace

    -- now pull a stream back out of the trace and use it as input to delay
    print $ delay shallowEnv $ (getSeq pv2 (inputs trace) witness :: Seq Bool)
    print $ (getSeq (PadVar 3 "ints") (inputs trace) witness2 :: Seq U4)
