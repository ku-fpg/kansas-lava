{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
module Language.KansasLava.Trace where

import Language.KansasLava
import Language.KansasLava.Testing.Probes

import qualified Data.Sized.Matrix as Matrix

import Data.List
import qualified Data.Map as M
import Data.Maybe

type TraceMap a = M.Map a (BaseTy,[[X Bool]]) -- to recover type, eventually clock too?

data Trace = Trace { cycles :: Int
                   , inputs :: TraceMap PadVar
                   , outputs :: TraceMap PadVar
                   , probes :: TraceMap PadVar
--                   , circuit :: ReifiedCircuit
--                   , opts :: DebugOpts -- can see a case for this eventually
                   -- what else? keep the vhdl here?
                   }

-- Some combinators to get stuff in and out of the map
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

-- obviously need to figure out the PadVar business at some point
addProbe :: Annotation -> TraceMap PadVar -> TraceMap PadVar
addProbe (ProbeValue key strm) m = M.insert key strm m

-- instances for Trace
instance Show Trace where
    show (Trace c i o p) = unlines $ concat [[show c,"inputs"], printer i, ["outputs"], printer o, ["probes"], printer p]
        where printer strm = map show $ zip (M.keys strm) [(ty,take 20 val) | (ty,val) <- M.elems strm]

-- two traces are equal if they have the same length and all the streams are equal over that length
instance Eq Trace where
    (==) (Trace c1 i1 o1 p1) (Trace c2 i2 o2 p2) = (c1 == c2) && insEqual && outsEqual && probesEqual
        where sorted m = sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ [(k,(ty,take c1 s)) | (k,(ty,s)) <- M.toList m]
              insEqual = (sorted i1) == (sorted i2)
              outsEqual = (sorted o1) == (sorted o2)
              probesEqual = (sorted p1) == (sorted p2)

-- something more intelligent someday?
diff :: Trace -> Trace -> Bool
diff t1 t2 = t1 == t2

emptyTrace :: Trace
emptyTrace = Trace { cycles = 0, inputs = M.empty, outputs = M.empty, probes = M.empty }

takeTrace :: Int -> Trace -> Trace
takeTrace i t = t { cycles = i }

dropTrace :: Int -> Trace -> Trace
dropTrace i t@(Trace c ins outs ps) | i <= c = t { cycles = c - i
                                                 , inputs = ins'
                                                 , outputs = outs'
                                                 , probes = ps' }
                                    | otherwise = emptyTrace
    where dropStream m = M.fromList [ (k,(ty,drop i s)) | (k,(ty,s)) <- M.toList m ]
          ins' = dropStream ins
          outs' = dropStream outs
          ps' = dropStream ps

serialize :: Trace -> String
serialize (Trace c ins outs ps) = concat $ unlines [(show c), "INPUTS"] : showMap ins ++ ["OUTPUTS\n"] ++ showMap outs ++ ["PROBES\n"] ++ showMap ps
    where showMap m = [unlines [show k, show ty, showStrm strm] | (k,(ty,strm)) <- M.toList m]
          showStrm s = unwords [concatMap (showRepWire (error "serialize witness" :: Bool)) val | val <- take c s]

deserialize :: String -> Trace
deserialize str = Trace { cycles = c, inputs = ins, outputs = outs, probes = ps }
    where (cstr:"INPUTS":ls) = lines str
          c = read cstr :: Int
          (ins,"OUTPUTS":r1) = readMap ls
          (outs,"PROBES":r2) = readMap r1
          (ps,_) = readMap r2

readMap ls = (go $ takeWhile cond ls, rest)
    where cond = (not . (flip elem) ["INPUTS","OUTPUTS","PROBES"])
          rest = dropWhile cond ls
          go (k:ty:strm:r) = M.union (M.singleton (read k) (read ty,[map toXBool w | w <- words strm])) $ go r
          go _             = M.empty
          toXBool :: Char -> X Bool
          toXBool 'T' = return True
          toXBool 'F' = return False
          toXBool _   = fail "unknown"

writeToFile :: FilePath -> Trace -> IO ()
writeToFile fp t = writeFile fp $ serialize t

readFromFile :: FilePath -> IO Trace
readFromFile fp = do
    str <- readFile fp
    return $ deserialize str

{- combinators for working with traces
-- should be easy to build using probes... especially once probes store data in same way
mkTrace :: (Ports a, Probe a, Ports b) => Int -> a -> (a -> b) -> IO Trace
mkTrace cycles circuit apply = do
    rc <- reifyCircuit [] $ apply circuit -- this is essentially what probeCircuit does
                                          -- but we want the unique node id's too
    let evts = [(n,pv) | (_,Entity _ _ _ attrs) <- theCircuit rc
                       , pv@(ProbeValue n v) <- attrs]
    return evts

-- make use of probeCircuit
mkTraceProbes :: (..) => [(String,Annotation)] -> Trace

-- assuming ReifiedCircuit has probe data in it
mkTraceRC :: (..) => ReifiedCircuit -> Trace

-- run a circuit with a trace, get a new trace (same inputs, possibly diff outputs) back
runTrace :: (..) => a -> Trace -> Trace

-- this is just Eq, but maybe instead of Bool some kind of detailed diff
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
