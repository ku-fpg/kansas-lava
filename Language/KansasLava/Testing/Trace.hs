{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Language.KansasLava.Testing.Trace (Trace(..), traceSignature, setCycles
                                         ,addInput, getInput, remInput
                                         ,addOutput, getOutput, remOutput
                                         ,addProbe, getProbe, remProbe
                                         ,seqAll, toVCD
                                         ,diff, emptyTrace, takeTrace, dropTrace
                                         ,serialize, deserialize, genShallow, genInfo
                                         ,writeToFile, readFromFile, checkExpected, execute) where

import Language.KansasLava.Types
import Language.KansasLava.Shallow
import Language.KansasLava.Utils
import Language.KansasLava.Seq
import Language.KansasLava.Comb
import Language.KansasLava.Reify
import Language.KansasLava.Signal
import Language.KansasLava.StdLogicVector

import qualified Language.KansasLava.Stream as Stream
import Language.KansasLava.Stream (Stream)
import Language.KansasLava.Testing.Utils

import qualified Data.Sized.Matrix as Matrix

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Debug.Trace

-- instance Functor TraceStream where -- can we do this with proper types?

traceSignature :: Trace -> Signature
traceSignature (Trace _ ins outs _) = Signature inps outps []
    where inps = [ (k,ty) | (k,TraceStream ty _) <- M.toList ins ]
          outps = [ (k,ty) | (k,TraceStream ty _) <- M.toList outs ]

-- Combinators to change a trace
setCycles :: Int -> Trace -> Trace
setCycles i t = t { len = Just i }

addInput :: forall a. (Rep a) => OVar -> Seq a -> Trace -> Trace
addInput key seq t@(Trace _ ins _ _) = t { inputs = addSeq key seq ins }

getInput :: (Rep w) => OVar -> Trace -> Seq w
getInput key trace = getSignal $ (inputs trace) M.! key

remInput :: OVar -> Trace -> Trace
remInput key t@(Trace _ ins _ _) = t { inputs = M.delete key ins }

addOutput :: forall a. (Rep a) => OVar -> Seq a -> Trace -> Trace
addOutput key seq t@(Trace _ _ outs _) = t { outputs = addSeq key seq outs }

getOutput :: (Rep w) => OVar -> Trace -> Seq w
getOutput key trace = getSignal $ (outputs trace) M.! key

remOutput :: OVar -> Trace -> Trace
remOutput key t@(Trace _ _ outs _) = t { outputs = M.delete key outs }

addProbe :: forall a. (Rep a) => OVar -> Seq a -> Trace -> Trace
addProbe key seq t@(Trace _ _ _ ps) = t { probes = addSeq key seq ps }

getProbe :: (Rep w) => OVar -> Trace -> Seq w
getProbe key trace = getSignal $ (probes trace) M.! key

remProbe :: OVar -> Trace -> Trace
remProbe key t@(Trace _ _ _ ps) = t { probes = M.delete key ps }

-- instances for Trace
instance Show Trace where
    show (Trace c i o p) = unlines $ concat [[show c,"inputs"], printer i, ["outputs"], printer o, ["probes"], printer p]
        where printer m = [show (k,TraceStream ty $ takeMaybe c val) | (k,TraceStream ty val) <- M.toList m]

-- two traces are equal if they have the same length and all the streams are equal over that length
instance Eq Trace where
    (==) (Trace c1 i1 o1 p1) (Trace c2 i2 o2 p2) = (c1 /= Nothing || c2 /= Nothing) && (c1 == c2) && insEqual && outEqual && probesEqual
        where sorted m = [(k,TraceStream ty $ takeMaybe c1 s) | (k,TraceStream ty s) <- M.assocs m]
              insEqual = (sorted i1) == (sorted i2)
              outEqual = (sorted o1) == (sorted o2)
              probesEqual = (sorted p1) == (sorted p2)

-- something more intelligent someday?
diff :: Trace -> Trace -> Bool
diff t1 t2 = t1 == t2

emptyTrace :: Trace
emptyTrace = Trace { len = Nothing, inputs = M.empty, outputs = M.empty, probes = M.empty }

takeTrace :: Int -> Trace -> Trace
takeTrace i t = t { len = Just newLen }
    where newLen = case len t of
                    Just x -> min i x
                    Nothing -> i

dropTrace :: Int -> Trace -> Trace
dropTrace i t@(Trace c ins outs ps)
    | newLen > 0 = t { len = Just newLen
                     , inputs = dropStream ins
                     , outputs = dropStream outs
                     , probes = dropStream ps }
    | otherwise = emptyTrace
    where dropStream m = M.fromList [ (k,TraceStream ty (drop i s)) | (k,TraceStream ty s) <- M.toList m ]
          newLen = maybe i (\x -> x - i) c

-- need to change format to be vertical
serialize :: Trace -> String
serialize (Trace c ins outs ps) = unlines
                                $ [show c, "INPUTS"]
                               ++ showMap ins
                               ++ ["OUTPUTS"]
                               ++ showMap outs
                               ++ ["PROBES"]
                               ++ showMap ps
    where showMap :: TraceMap -> [String]
          showMap m = concat [[show k, show ty, showStrm strm] | (k,TraceStream ty strm) <- M.toList m]
          showStrm s = unwords [concatMap ((showRep (Witness :: Witness Bool)) . XBool) $ val | RepValue val <- takeMaybe c s]

deserialize :: String -> Trace
deserialize str = Trace { len = c, inputs = ins, outputs = outs, probes = ps }
    where (cstr:"INPUTS":ls) = lines str
          c = read cstr :: Maybe Int
          (ins,"OUTPUTS":r1) = readMap ls
          (outs,"PROBES":r2) = readMap r1
          (ps,_) = readMap r2

genShallow :: Trace -> [String]
genShallow (Trace c ins outs _) = mergeWith (++) [ showTraceStream c v | v <- alldata ]
    where alldata = (M.elems ins) ++ (M.elems outs)

genInfo :: Trace -> [String]
genInfo (Trace c ins outs _) = [ "(" ++ show i ++ ") " ++ l | (i,l) <- zip [1..] lines ]
    where alldata = (M.elems ins) ++ (M.elems outs)
          lines = mergeWith (\ x y -> x ++ " -> " ++ y) [ showTraceStream c v | v <- alldata ]

writeToFile :: FilePath -> Trace -> IO ()
writeToFile fp t = writeFile fp $ serialize t

readFromFile :: FilePath -> IO Trace
readFromFile fp = do
    str <- readFile fp
    return $ deserialize str

-- return true if running circuit with trace gives same outputs as that contained by the trace
checkExpected :: (Ports a) => a -> Trace -> (Bool, Trace)
checkExpected circuit trace = (trace == result, result)
    where result = execute circuit trace

execute :: (Ports a) => a -> Trace -> Trace
execute circuit t@(Trace _ _ outs _) = t { outputs = M.adjust (\_ -> run circuit t) k outs }
    where k = head $ M.keys outs

-- Functions below are not exported.

toXBit :: Maybe Bool -> Char
toXBit = maybe 'X' (\b -> if b then '1' else '0')

-- note the reverse here is crucial due to way vhdl indexes stuff
showTraceStream :: Maybe Int -> TraceStream -> [String]
showTraceStream c (TraceStream _ s) = [map (toXBit . unX . XBool) $ reverse val | RepValue val <- takeMaybe c s]

readStrm :: [String] -> (TraceStream, [String])
readStrm ls = (strm,rest)
    where (m,rest) = readMap ls
          [(_,strm)] = M.toList (m :: TraceMap)

readMap :: [String] -> (TraceMap, [String])
readMap ls = (go $ takeWhile cond ls, rest)
    where cond = (not . (flip elem) ["INPUTS","OUTPUTS","PROBES"])
          rest = dropWhile cond ls
          go (k:ty:strm:r) = M.union (M.singleton (read k) (TraceStream (read ty) ([RepValue $ map toWireVal w | w <- words strm]))) $ go r
          go _             = M.empty
          toWireVal :: Char -> WireVal Bool
          toWireVal '1' = return True
          toWireVal '0' = return False
          toWireVal _   = fail "unknown"

addStream :: forall w. (Rep w) => OVar -> TraceMap -> Stream (X w) -> TraceMap
addStream key m stream = M.insert key (toTrace stream) m

addSeq :: forall w. (Rep w) => OVar -> Seq w -> TraceMap -> TraceMap
addSeq key seq m = addStream key m (seqValue seq :: Stream (X w))

toVCD :: Trace -> String
toVCD (Trace Nothing _ _ _)  = error "can't turn infinite trace into vcd"
toVCD (Trace (Just n) i o p) = unlines
    [ "$timescale 10ns $end"
    , "$scope module logic $end"
    ]
    ++ unlines [ unwords ["$var wire", show l, id, show k, "$end"]
               | (id,(k,TraceStream ty strm)) <- signals
               , let RepValue vs = head strm
               , let l = length vs ]
    ++ "$enddefinitions $end\n"
    ++ values n signals

    where signals = zip vcd_ids $ M.toList $ M.unions [i,o,p]

-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
vcd_ids :: [String]
vcd_ids = res
    where chars = [(chr 33)..(chr 126)]
          ids@(_:res) = [[]]  ++ concatMap (\i -> [c:i | c <- chars]) ids

vcdVal :: RepValue -> String -> String
vcdVal r@(RepValue bs) id | length bs == 1 = show r ++ id
                          | otherwise      = "b" ++ show r ++ " " ++ id

values :: Int -> [(String, (OVar, TraceStream))] -> String
values n sigs = dumpVars inits ++ eventList inits (zip [0..] rest)
    where (inits:rest) = transpose [ take n $ zip strm (repeat id) | (id, (_, TraceStream _ strm)) <- sigs ]

dumpVars :: [(RepValue, String)] -> String
dumpVars vals = "$dumpvars\n" ++ unlines (map (uncurry vcdVal) vals) ++ "$end\n"

eventList :: [(RepValue, String)] -> [(Int,[(RepValue, String)])] -> String
eventList last rest = case next of
                        [] -> ""
                        ((clk,n):ns) -> "#" ++ show clk ++ "\n" ++ valChange last n ++ eventList n ns
    where next = dropWhile (\(clock, row) -> row == last) rest

-- the assumption is that these lists are parallel by id
valChange :: [(RepValue, String)] -> [(RepValue, String)] -> String
valChange prev cur = unlines [ vcdVal val id | (p,c@(val,id)) <- zip prev cur
                                             , p /= c ]
