{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Language.KansasLava.Testing.Trace (Trace(..), traceSignature, setCycles
                                         ,addInput, getInput, remInput
                                         ,addOutput, getOutput, remOutput
                                         ,addProbe, getProbe, remProbe
                                         ,cmpTrace, cmpTraceIO, diff, emptyTrace, takeTrace, dropTrace
                                         ,serialize, deserialize, genShallow, genInfo, asciiToTrace
                                         ,writeToFile, readFromFile{-, checkExpected, execute-}) where

import Language.KansasLava.Types
import Language.KansasLava.Shallow
import Language.KansasLava.Utils
import Language.KansasLava.Seq
import Language.KansasLava.Reify

import Language.KansasLava.Stream (Stream)
import Language.KansasLava.Testing.Utils

import Data.List
import qualified Data.Map as M

-- instance Functor TraceStream where -- can we do this with proper types?

-- generate a signature from a trace
-- TODO: support generics in both these functions?
traceSignature :: Trace -> Signature
traceSignature (Trace _ ins outs _) = Signature (convert ins) (convert outs) []
    where convert m = [ (OVar i (show wc),ty) | (wc@(WholeCircuit _ i _),TraceStream ty _) <- M.toList m ]

-- creates an (obviously empty) trace from a signature
signatureTrace :: Signature -> Trace
signatureTrace (Signature inps outps _) = Trace Nothing (convert inps) (convert outps) M.empty
    where convert l = M.fromList [ (read nm, TraceStream ty [])  | (OVar _ nm, ty) <- l ]

-- Combinators to change a trace
setCycles :: Int -> Trace -> Trace
setCycles i t = t { len = Just i }

addInput :: forall a. (Rep a) => ProbeName -> Seq a -> Trace -> Trace
addInput key iseq t@(Trace _ ins _ _) = t { inputs = addSeq key iseq ins }

getInput :: (Rep w) => ProbeName -> Trace -> Seq w
getInput key trace = getSignal $ (inputs trace) M.! key

remInput :: ProbeName -> Trace -> Trace
remInput key t@(Trace _ ins _ _) = t { inputs = M.delete key ins }

addOutput :: forall a. (Rep a) => ProbeName -> Seq a -> Trace -> Trace
addOutput key iseq t@(Trace _ _ outs _) = t { outputs = addSeq key iseq outs }

getOutput :: (Rep w) => ProbeName -> Trace -> Seq w
getOutput key trace = getSignal $ (outputs trace) M.! key

remOutput :: ProbeName -> Trace -> Trace
remOutput key t@(Trace _ _ outs _) = t { outputs = M.delete key outs }

addProbe :: forall a. (Rep a) => ProbeName -> Seq a -> Trace -> Trace
addProbe key iseq t@(Trace _ _ _ ps) = t { probes = addSeq key iseq ps }

getProbe :: (Rep w) => ProbeName -> Trace -> Seq w
getProbe key trace = getSignal $ (probes trace) M.! key

remProbe :: ProbeName -> Trace -> Trace
remProbe key t@(Trace _ _ _ ps) = t { probes = M.delete key ps }

-- instances for Trace
instance Show Trace where
    show = serialize

instance Read Trace where
    readsPrec _ = deserialize

-- two traces are equal if they have the same length and all the streams are equal over that length
instance Eq Trace where
    (==) (Trace c1 i1 o1 p1) (Trace c2 i2 o2 p2) = (c1 /= Nothing || c2 /= Nothing) && (c1 == c2) && insEqual && outEqual && probesEqual
        where sorted m = [(k,TraceStream ty $ takeMaybe c1 s) | (k,TraceStream ty s) <- M.assocs m]
              insEqual = (sorted i1) == (sorted i2)
              outEqual = (sorted o1) == (sorted o2)
              probesEqual = (sorted p1) == (sorted p2)

-- | Compare two trace objects. First argument is the golden value. See notes for cmpRepValue
cmpTrace :: Trace -> Trace -> Bool
cmpTrace (Trace Nothing _ _ _)     _                           = False
cmpTrace (Trace c1 _ _ _)          (Trace c2 _ _ _) | c1 /= c2 = False
cmpTrace (Trace (Just c) i1 o1 p1) (Trace _ i2 o2 p2)          =
    and [ k1 == k2 && cmpTraceStream c s1 s2
        | (m1, m2) <- zip [i1,o1,p1] [i2,o2,p2]
        , ((k1,s1),(k2,s2)) <- zip (M.assocs m1) (M.assocs m2)
        ]

-- | Like cmpTrace but only compares inputs and outputs.
cmpTraceIO :: Trace -> Trace -> Bool
cmpTraceIO (Trace c1 i1 o1 _) (Trace c2 i2 o2 _) = cmpTrace (Trace c1 i1 o1 M.empty) (Trace c2 i2 o2 M.empty)

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
                               ++ ["END"]
    where showMap :: TraceMap -> [String]
          showMap m = [intercalate "\t" [show k, show ty, showStrm strm] | (k,TraceStream ty strm) <- M.toList m]
          showStrm s = unwords [concatMap ((showRep (Witness :: Witness Bool)) . XBool) $ val | RepValue val <- takeMaybe c s]

deserialize :: String -> [(Trace,String)]
deserialize str = [(Trace { len = read cstr, inputs = ins, outputs = outs, probes = ps },unlines rest)]
    where (cstr:"INPUTS":ls) = lines str
          (ins,"OUTPUTS":r1) = readMap ls
          (outs,"PROBES":r2) = readMap r1
          (ps,"END":rest) = readMap r2

genShallow :: Trace -> [String]
genShallow (Trace c ins outs _) = mergeWith (++) [ showTraceStream c v | v <- alldata ]
    where alldata = (M.elems ins) ++ (M.elems outs)

-- inverse of genShallow
asciiToTrace :: [String] -> Signature -> Trace
asciiToTrace ilines sig = et { inputs = ins, outputs = outs }
    where et = setCycles (length ilines) $ signatureTrace sig
          widths = [ typeWidth ty
                   | (_,TraceStream ty _) <- M.assocs (inputs et) ++ M.assocs (outputs et)
                   ]
          (inSigs, outSigs) = splitAt (M.size $ inputs et) $ splitLists ilines widths
          addToMap sigs m = M.fromList [ (k,TraceStream ty (map (RepValue . reverse . (map fromXBit)) strm))
                                       | (strm,(k,TraceStream ty _)) <- zip sigs $ M.assocs m
                                       ]
          (ins, outs) = (addToMap inSigs $ inputs et, addToMap outSigs $ outputs et)

splitLists :: [[a]] -> [Int] -> [[[a]]]
splitLists xs (i:is) = map (take i) xs : splitLists (map (drop i) xs) is
splitLists _  []     = [[]]

genInfo :: Trace -> [String]
genInfo (Trace c ins outs _) = [ "(" ++ show i ++ ") " ++ l | (i::Int,l) <- zip [1..] lines' ]
    where alldata = (M.elems ins) ++ (M.elems outs)
          lines' = mergeWith (\ x y -> x ++ " -> " ++ y) [ showTraceStream c v | v <- alldata ]

writeToFile :: FilePath -> Trace -> IO ()
writeToFile fp t = writeFile fp $ serialize t

readFromFile :: FilePath -> IO Trace
readFromFile fp = do
    str <- readFile fp
    return $ fst $ head $ deserialize str

-- return true if running circuit with trace gives same outputs as that contained by the trace
{-
checkExpected :: (Ports a) => a -> Trace -> (Bool, Trace)
checkExpected circuit trace = (trace == result, result)
    where result = execute circuit trace

execute :: (Ports a) => a -> Trace -> Trace
execute circuit t@(Trace _ _ outs _) = t { outputs = newOutputs }
    where res@(TraceStream ty _) = run circuit t
          sig = getSignal res -- need type ascription here and can't get it
          newOutputs = foldr (\(k,v) m -> M.adjust (\_ -> v) k m) outs $ zip ks vs
          ks = take (length ty) $ M.keys outs
          vs = case unpack of
                (s1, s2) -> [toTrace s1, toTrace s2]
                (s1, s2, s3) -> [toTrace s1, toTrace s2, toTrace s3]
                other -> toTrace other
-}

-- Functions below are not exported.

toXBit :: WireVal Bool -> Char
toXBit WireUnknown = 'X'
toXBit (WireVal True) = '1'
toXBit (WireVal False) = '0'

fromXBit :: Char -> WireVal Bool
fromXBit 'X' = WireUnknown
fromXBit 'U' = WireUnknown -- is this really the case?
fromXBit '1' = WireVal True
fromXBit '0' = WireVal False

-- note the reverse here is crucial due to way vhdl indexes stuff
showTraceStream :: Maybe Int -> TraceStream -> [String]
showTraceStream c (TraceStream _ s) = [map toXBit $ reverse val | RepValue val <- takeMaybe c s]

-- readStrm :: [String] -> (TraceStream, [String])
-- readStrm ls = (strm,rest)
--     where (m,rest) = readMap ls
--           [(_,strm)] = M.toList (m :: TraceMap)

readMap :: [String] -> (TraceMap, [String])
readMap ls = (go thismap, rest)
    where cond = (not . (flip elem) ["INPUTS","OUTPUTS","PROBES","END"])
          (thismap, rest) = span cond ls
          tabsplit l = let (k,'\t':r1) = span (/= '\t') l
                           (ty,'\t':r) = span (/= '\t') r1
                       in (k,ty,r)
          go :: [String] -> TraceMap
          go = foldr (\l m -> let (k,ty,strm) = tabsplit l
                              in M.union (M.singleton (read k) (TraceStream (read ty) [read v | v <- words strm]))
                                         m
                     )
                     M.empty

addStream :: forall w. (Rep w) => ProbeName -> TraceMap -> Stream (X w) -> TraceMap
addStream key m stream = M.insert key (toTrace stream) m

addSeq :: forall w. (Rep w) => ProbeName -> Seq w -> TraceMap -> TraceMap
addSeq key iseq m = addStream key m (seqValue iseq :: Stream (X w))
