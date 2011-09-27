{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes, ExistentialQuantification, ScopedTypeVariables, UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | Probes log the shallow-embedding signals of a Lava circuit in the
-- | deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Probes (
 Probe(..), probe, probeCircuit, probeNames, probeValue, probeData,
 remProbes, mergeProbes, mergeProbesIO, exposeProbes, exposeProbesIO,
 toGraph, toTrace, fromTrace, printProbes, printProbeTable
 ) where

import qualified Data.Reify.Graph as DRG

import Data.List(nub,sort,isPrefixOf,transpose)
-- import Control.Monad
import Control.Applicative
import qualified Data.Graph.Inductive as G

import qualified Data.Sized.Matrix as M

import Language.KansasLava.Comb
import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Seq
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types

import System.Environment
import System.IO.Unsafe

import Debug.Trace

-- basic conversion to trace representation
-- | Convert a Stream to a TraceStream.
toTrace :: forall w . (Rep w) => S.Stream (X w) -> TraceStream
toTrace stream = TraceStream (repType (Witness :: Witness w)) [toRep xVal | xVal <- S.toList stream ]

-- | Convert a TraceStream to a Stream.
fromTrace :: (Rep w) => TraceStream -> S.Stream (X w)
fromTrace (TraceStream _ list) = S.fromList [fromRep val | val <- list]

-- this is the public facing method for probing
-- | Add a named probe to a circuit
probe :: (Probe a) => String -> a -> a
probe name = probe' probeState [ show (i::Int) ++ name | i <- [0..] ]

-- | Probe all of the inputs/outputs for the given Fabric. The parameter 'n'
-- indicates the sequence length to capture.
probeCircuit :: Int -> Fabric () -> IO [(String, TraceStream)]
probeCircuit n fabric = do
    -- TODO: figure out why mergeProbes is broken
    -- rc <- (reifyFabric >=> mergeProbesIO) fabric
    rc <- reifyFabric fabric

    return [ (nm,TraceStream ty $ take n strm)
           | (_,Entity (TraceVal nms (TraceStream ty strm)) _ _) <- theCircuit rc
           , nm <- nms ]

-- | Print the output of 'probeCircuit' nicely on stdout, one stream per line
printProbes :: [(String, TraceStream)] -> IO ()
printProbes strms = do
    let maxlen = maximum $ map (length . show . fst) strms
    sequence_ [ putStrLn $ replicate p ' ' ++ nm ++ ": " ++ show strm
              | (nm,strm) <- strms, let p = maxlen - length nm ]

-- | Print the output of 'probeCircuit' in a tabular format on stdout, one stream per column
printProbeTable :: [(String, TraceStream)] -> IO ()
printProbeTable strms = do
    let (headers, strms') = unzip strms
        strms'' = [map show s | TraceStream _ s <- strms']
        (ticks, _) = unzip $ zip (map show [(0::Int)..]) $ case strms'' of [] -> [""]; (x:_) -> x
        table = ("clk" : headers) : transpose (ticks : strms'')
        clkwidth = 1 + max 3 (maximum $ map length ticks)
        widths = clkwidth : [1 + max hl sl | (h,TraceStream _ (v:_)) <- strms
                                      , let hl = length (show h)
                                      , let sl = length (unRepValue v)]
        pr :: [Int] -> [String] -> IO ()
        pr ws ss = do
            sequence_ [putStr $ s ++ replicate p ' ' | (w,s) <- zip ws ss, let p = w - length s]
            putStr "\n"
    mapM_ (pr widths) table

-- | Get all of the named probes for a 'KLEG' node.
probeNames :: DRG.Unique -> KLEG -> [String]
probeNames n c = maybe [] fst $ probeData n c

-- | Get all of the prove values for a 'KLEG' node.
probeValue :: DRG.Unique -> KLEG -> Maybe TraceStream
probeValue n c = snd <$> probeData n c

-- | Capture the shallow embedding probe value to the deep embedding.
insertProbe :: String -> TraceStream -> Driver E -> Driver E
insertProbe n s@(TraceStream ty _) = mergeNested
    where mergeNested :: Driver E -> Driver E
          mergeNested (Port nm (E (Entity (TraceVal names strm) outs ins)))
                        = Port nm (E (Entity (TraceVal (n:names) strm) outs ins))
          mergeNested d = Port "o0" (E (Entity (TraceVal [n] s) [("o0",ty)] [("i0",ty,d)]))

-- | Get the probe names and trace from a 'KLEG' graph.
probeData :: DRG.Unique -> KLEG -> Maybe ([String], TraceStream)
probeData n circuit = case lookup n $ theCircuit circuit of
                        Just (Entity (TraceVal nms strm) _ _) -> Just (nms, strm)
                        _ -> Nothing

-- | The 'Probe' class is used for adding probes to all inputs/outputs of a Lava
-- circuit.
class Probe a where
    -- | Add probes (using the input list of strings as a name supply) to Lava
    -- circuit.
    probe' :: ProbeState -> [String] -> a -> a

instance (Clock c, Rep a) => Probe (CSeq c a) where
    probe' NoProbe _ sq = sq
    probe' TraceProbe (n:_) (Seq s (D d)) = Seq (obs s) (D d)
        where obs = foldr (\ (i,x) xs -> trace (show n ++ "(" ++ show i ++ ")" ++ showRep x) $ S.Cons x xs) (error "never done")
                  . zip [(0::Int)..]
                  . S.toList
    probe' CaptureProbe (n:_) (Seq s (D d)) = Seq s (D (insertProbe n strm d))
        where strm = toTrace s
    probe' _ [] _ = error "Can't add probe: no name supply available (Seq)"

instance Rep a => Probe (Comb a) where
    probe' NoProbe _ sq = sq
    probe' TraceProbe _ sq = sq
    probe' CaptureProbe (n:_) (Comb s (D d)) = Comb s (D (insertProbe n strm d))
        where strm = toTrace $ S.fromList $ repeat s
    probe' _ [] _ = error "Can't add probe: no name supply available (Comb)"

instance (Probe a, Probe b) => Probe (a,b) where
    probe' m names (x,y) = (probe' m (addSuffixToProbes names "-fst") x,
                            probe' m (addSuffixToProbes names "-snd") y)


instance (Probe a, Probe b, Probe c) => Probe (a,b,c) where
    probe' m names (x,y,z) = (probe' m (addSuffixToProbes names "-fst") x,
                              probe' m (addSuffixToProbes names "-snd") y,
                              probe' m (addSuffixToProbes names "-thd") z)

instance (Probe a, M.Size x) => Probe (M.Matrix x a) where
    probe' _ _ _ = error "Probe(probe') not defined for Matrix"

instance (Probe a, Probe b) => Probe (a -> b) where
    probe' m (n:ns) f x = probe' m ns $ f (probe' m [n] x)
    probe' _ [] _ _ = error "Can't add probe: no name supply available (a -> b)"

addSuffixToProbes :: [String] -> String -> [String]
addSuffixToProbes pns suf = map (++ suf) pns

-- | Convert a 'KLEG' to a fgl graph.
toGraph :: KLEG -> G.Gr (Entity DRG.Unique) ()
toGraph rc = G.mkGraph (theCircuit rc) [ (n1,n2,())
                                       | (n1,Entity _ _ ins) <- theCircuit rc
                                       , (_,_,Port _ n2) <- ins ]

-- Gives probes their node ids. This is used by mergeProbes and should not be exposed.
addProbeIds :: KLEG -> KLEG
addProbeIds circuit = circuit { theCircuit = newCircuit }
    where newCircuit = [ addId entity | entity <- theCircuit circuit ]
          addId (nid, Entity (TraceVal nms strm) outs ins) = (nid, Entity (TraceVal (map (++ show nid) nms) strm) outs ins)
          addId other = other


-- | Rewrites the circuit graph and commons up probes that have the same stream value.
mergeProbes :: KLEG -> KLEG
mergeProbes circuit = addProbeIds $ go (probeList circuit) circuit
    where go ((pid,Entity (TraceVal pnames strm) outs ins@[(_,_,d)]):pl) rc =
                         let others = probesOnAL d pl
                             otherIds = [ k | (k,_) <- others, k /= pid ]
                             newNames = nub $ pnames ++ concatMap snd others
                             updatedNames = updateAL pid (Entity (TraceVal newNames strm) outs ins) $ theCircuit rc
                         in go pl $ replaceWith (f pid) otherIds $ rc { theCircuit = updatedNames }
          go [] rc = rc
          go other _ = error $ "mergeProbes: " ++ show other
          f pid (Port s _) = Port s pid
          f _ p = p

-- | Lift the pure 'mergeProbes' function into the 'IO' monad.
mergeProbesIO :: KLEG -> IO KLEG
mergeProbesIO = return . mergeProbes

-- | Removes all probe nodes from the circuit.
remProbes :: KLEG -> KLEG
remProbes circuit = go (probeList circuit) circuit
    where go ((pid,Entity _ _ [(_,_,d)]):pl) rc =
                         let probes = pid : [ ident | (ident,_) <- probesOnAL d pl ]
                         in go pl $ replaceWith (\_ -> d) probes rc
          go [] rc = rc
          go other _ = error $ "remProbes: " ++ show other

-- | The 'exposeProbes' function lifted into the 'IO' monad.
exposeProbesIO :: [String] -> KLEG -> IO KLEG
exposeProbesIO names = return . exposeProbes names

-- | Takes a list of prefixes and exposes any probe whose name
-- contains that prefix as an output pad.
exposeProbes :: [String] -> KLEG -> KLEG
exposeProbes names rc = rc { theSinks = oldSinks ++ newSinks }
    where oldSinks = theSinks rc
--          n = succ $ head $ sortBy (flip compare) [ read $ takeWhile isDigit nm | (nm, _, _) <- oldSinks ]
          allProbes = sort [ (pname, nm, outs)
                           | (nm, Entity (TraceVal pnames _) outs _) <- theCircuit rc
                           , pname <- pnames ]
          newSinks = nub [ (pname, oty, Port onm nm)
                         | (pname, nm, outs) <- allProbes
                         , or [ name `isPrefixOf` pname | name <- names ]
                         , (onm,oty) <- outs ]

--          newSinks = [ (pname ++ "_" ++ show i, ty, d) | (i,(pname, ty,d@(Port _ _))) <- zip [n..] exposed ]

-- Below is not exported.

-- Surely this exists somewhere!
updateAL :: (Eq k) => k -> v -> [(k,v)] -> [(k,v)]
updateAL key val list = [ (k,if k == key then val else v) | (k,v) <- list ]

replaceWith :: (Driver DRG.Unique -> Driver DRG.Unique) -> [DRG.Unique] -> KLEG -> KLEG
replaceWith _ [] rc = rc
replaceWith y xs rc = rc { theCircuit = newCircuit, theSinks = newSinks }
    where -- newCircuit :: [(DRG.Unique, Entity DRG.Unique)]
          newCircuit = [ (ident,Entity n o (map change ins))
                       | (ident,Entity n o ins) <- theCircuit rc
                       , ident `notElem` xs ]
          newSinks ::[(String, Type, Driver DRG.Unique)]
          newSinks = map change $ theSinks rc

          change :: (a, Type, Driver DRG.Unique) ->
                    (a, Type, Driver DRG.Unique)
          change (nm,ty,p@(Port _ i)) | i `elem` xs = (nm,ty,y p)
          change other = other

probeList :: KLEG -> [(DRG.Unique, Entity DRG.Unique)]
probeList rc = [ (n,e) | (n,e@(Entity (TraceVal _ _) _ _)) <- theCircuit rc ]

-- probesOn :: Driver DRG.Unique -> KLEG -> [(DRG.Unique,[ProbeName])]
-- probesOn x rc = probesOnAL x $ theCircuit rc

probesOnAL :: Driver DRG.Unique -> [(DRG.Unique, Entity DRG.Unique)] -> [(DRG.Unique,[String])]
probesOnAL x al = [ (ident,nms) | (ident, Entity (TraceVal nms _) _ ins) <- al
                                , (_,_,d) <- ins
                                , d == x ]



data ProbeState = NoProbe | TraceProbe | CaptureProbe

{-# NOINLINE probeState #-}
probeState :: ProbeState
probeState = unsafePerformIO $ do
        nm <- getEnv "KANSAS_LAVA_PROBE" `catch` (\ _ -> return "")
        return $ case nm of
          "none"    -> NoProbe
          "trace"   -> TraceProbe
          "capture" -> CaptureProbe
          _         -> NoProbe

