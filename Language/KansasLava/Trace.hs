{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Language.KansasLava.Trace where

import Language.KansasLava.Circuit
import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Stream hiding (head,zipWith)
import Language.KansasLava.Type
import Language.KansasLava.Utils
import Language.KansasLava.Wire

import qualified Data.Sized.Matrix as Matrix

import qualified Data.Graph.Inductive as G

import qualified Data.Reify.Graph as DRG

import Data.List
import qualified Data.Map as M
import Data.Maybe

type TraceMap k = M.Map k TraceStream
type TraceStream = (BaseTy,[[X Bool]]) -- to recover type, eventually clock too?

-- cycles -> Maybe Int?
data Trace = Trace { cycles :: Int
                   , inputs :: TraceMap PadVar
                   , outputs :: TraceStream
                   , probes :: TraceMap PadVar
--                   , circuit :: ReifiedCircuit
--                   , opts :: DebugOpts -- can see a case for this eventually
                   -- what else? keep the vhdl here?
                   }

-- Some combinators to get stuff in and out of the map
fromXStream :: forall w. (RepWire w) => w -> Stream (X w) -> [[X Bool]]
fromXStream witness stream = [Matrix.toList $ fromWireXRep witness xVal | xVal <- toList stream ]

toXStream :: forall w. (RepWire w) => w -> [[X Bool]] -> Stream (X w)
toXStream witness list = fromList [toWireXRep witness $ Matrix.fromList val | val <- list]

getStream :: forall a w. (Ord a, RepWire w) => a -> TraceMap a -> w -> Stream (X w)
getStream name m witness = case M.lookup name m of
                        Just (ty,rep) -> toXStream witness rep

getSeq :: (Ord a, RepWire w) => a -> TraceMap a -> w -> Seq w
getSeq key m witness = shallowSeq $ getStream key m witness

addStream :: forall a w. (Ord a, RepWire w) => a -> TraceMap a -> w -> Stream (X w) -> TraceMap a
addStream key m witness stream = M.insert key (ty,rep) m
    where ty = wireType witness
          rep = fromXStream witness stream

addSeq :: forall a b. (Ord a, RepWire b) => a -> Seq b -> TraceMap a -> TraceMap a
addSeq key seq m = addStream key m (witness :: b) (seqValue seq :: Stream (X b))

-- Combinators to change a trace
setCycles :: Int -> Trace -> Trace
setCycles i t = t { cycles = i }

addInput :: forall a. (RepWire a) => PadVar -> Seq a -> Trace -> Trace
addInput key seq t@(Trace _ ins _ _) = t { inputs = addSeq key seq ins }

remInput :: PadVar -> Trace -> Trace
remInput key t@(Trace _ ins _ _) = t { inputs = M.delete key ins }

setOutput :: forall a. (RepWire a) => Seq a -> Trace -> Trace
setOutput (Seq s _) t = t { outputs = (wireType (witness :: a), fromXStream (witness :: a) s) }

addProbe :: forall a. (RepWire a) => PadVar -> Seq a -> Trace -> Trace
addProbe key seq t@(Trace _ _ _ ps) = t { probes = addSeq key seq ps }

remProbe :: PadVar -> Trace -> Trace
remProbe key t@(Trace _ _ _ ps) = t { probes = M.delete key ps }

-- instances for Trace
instance Show Trace where
    show (Trace c i (oty,os) p) = unlines $ concat [[show c,"inputs"], printer i, ["outputs", show (oty,take c os), "probes"], printer p]
        where printer m = [show (k,(ty,take c val)) | (k,(ty,val)) <- M.toList m]

-- two traces are equal if they have the same length and all the streams are equal over that length
instance Eq Trace where
    (==) (Trace c1 i1 (oty1,os1) p1) (Trace c2 i2 (oty2,os2) p2) = (c1 == c2) && insEqual && outEqual && probesEqual
        where sorted m = sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ [(k,(ty,take c1 s)) | (k,(ty,s)) <- M.toList m]
              insEqual = (sorted i1) == (sorted i2)
              outEqual = (oty1 == oty2) && (take c1 os1 == take c2 os2)
              probesEqual = (sorted p1) == (sorted p2)

-- something more intelligent someday?
diff :: Trace -> Trace -> Bool
diff t1 t2 = t1 == t2

emptyTrace :: Trace
emptyTrace = Trace { cycles = 0, inputs = M.empty, outputs = (B,[]), probes = M.empty }

takeTrace :: Int -> Trace -> Trace
takeTrace i t = t { cycles = i }

dropTrace :: Int -> Trace -> Trace
dropTrace i t@(Trace c ins (oty,os) ps) | i <= c = t { cycles = c - i
                                                , inputs = dropStream ins
                                                , outputs = (oty, drop i os)
                                                , probes = dropStream ps }
                                    | otherwise = emptyTrace
    where dropStream m = M.fromList [ (k,(ty,drop i s)) | (k,(ty,s)) <- M.toList m ]

-- need to change format to be vertical
serialize :: Trace -> String
serialize (Trace c ins (oty,os) ps) = concat $ unlines [(show c), "INPUTS"] : showMap ins ++ [unlines ["OUTPUT", show $ PadVar 0 "placeholder", show oty, showStrm os, "PROBES"]] ++ showMap ps
    where showMap m = [unlines [show k, show ty, showStrm strm] | (k,(ty,strm)) <- M.toList m]
          showStrm s = unwords [concatMap (showRepWire (witness :: Bool)) val | val <- take c s]

deserialize :: String -> Trace
deserialize str = Trace { cycles = c, inputs = ins, outputs = out, probes = ps }
    where (cstr:"INPUTS":ls) = lines str
          c = read cstr :: Int
          (ins,"OUTPUT":r1) = readMap ls
          (out,"PROBES":r2) = readStrm r1
          (ps,_) = readMap r2

readStrm :: [String] -> (TraceStream, [String])
readStrm ls = (strm,rest)
    where (m,rest) = readMap ls
          [(_,strm)] = M.toList (m :: TraceMap PadVar)

readMap :: (Ord k, Read k) => [String] -> (TraceMap k, [String])
readMap ls = (go $ takeWhile cond ls, rest)
    where cond = (not . (flip elem) ["INPUTS","OUTPUT","PROBES"])
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

rcToGraph :: ReifiedCircuit -> G.Gr (MuE DRG.Unique) ()
rcToGraph rc = G.mkGraph (theCircuit rc) [ (n1,n2,())
                                         | (n1,Entity _ _ ins _) <- theCircuit rc
                                         , (_,_,Port _ n2) <- ins ]

-- return true if running circuit with trace gives same outputs as that contained by the trace
test :: (Run a) => a -> Trace -> Bool
test circuit trace = trace == (execute circuit trace)

execute :: (Run a) => a -> Trace -> Trace
execute circuit trace = trace { outputs = run circuit trace }

class Run a where
    run :: a -> Trace -> TraceStream

instance (RepWire a) => Run (CSeq c a) where
    run (Seq s _) (Trace c _ _ _) = (wireType (witness :: a), take c $ fromXStream (witness :: a) s)

{- eventually
instance (RepWire a) => Run (Comb a) where
    run (Comb s _) (Trace c _ _ _) = (wireType witness, take c $ fromXStream witness (fromList $ repeat s))
        where witness = (error "run trace" :: a)
-}

instance (Run a, Run b) => Run (a,b) where
    -- note order of zip matters! must be consistent with fromWireXRep
    run (x,y) t = (TupleTy [ty1,ty2], zipWith (++) strm1 strm2)
        where (ty1,strm1) = run x t
              (ty2,strm2) = run y t

instance (Run a, Run b, Run c) => Run (a,b,c) where
    -- note order of zip matters! must be consistent with fromWireXRep
    run (x,y,z) t = (TupleTy [ty1,ty2,ty3], zipWith (++) strm1 $ zipWith (++) strm2 strm3)
        where (ty1,strm1) = run x t
              (ty2,strm2) = run y t
              (ty3,strm3) = run z t

instance (RepWire a, Run b) => Run (Seq a -> b) where
    run fn t@(Trace c ins _ _) = run (fn input) $ t { inputs = M.delete key ins }
        where key = head $ sort $ M.keys ins
              input = getSeq key ins (witness :: a)

{- combinators for working with traces
-- assuming ReifiedCircuit has probe data in it
mkTraceRC :: (..) => ReifiedCircuit -> Trace

-- this is just Eq, but maybe instead of Bool some kind of detailed diff
unionTrace :: Trace -> Trace -> Trace
remove :: PadVar -> Trace -> Trace

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
