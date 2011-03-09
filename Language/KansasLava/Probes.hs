{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,ExistentialQuantification,ScopedTypeVariables,UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | Probes log the shallow-embedding signals of a Lava circuit in the
-- | deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Probes (Probe(..),probe,probeWholeCircuit,probeCircuit,probeNames,probeValue) where

import qualified Data.Reify.Graph as DRG

import Control.Monad
import Control.Applicative

import qualified Data.Sized.Matrix as M

import Language.KansasLava.Circuit
import Language.KansasLava.Comb
import Language.KansasLava.Fabric
import Language.KansasLava.HandShake
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Shallow
import Language.KansasLava.Stream as S
import Language.KansasLava.Types

-- this is the public facing method for probing
probe :: (Probe a) => String -> a -> a
probe name = probe' [ Probe name i 0 | i <- [0..] ]

-- used to make traces
probeWholeCircuit :: (Probe a) => a -> a
probeWholeCircuit = probe' [ WholeCircuit "" i 0 | i <- [0..] ]

insertProbe :: ProbeName -> TraceStream -> Driver E -> Driver E
insertProbe n s@(TraceStream ty _) = mergeNested
    where mergeNested :: Driver E -> Driver E
          mergeNested (Port nm (E (Entity (TraceVal names strm) outs ins)))
                        = Port nm (E (Entity (TraceVal (n:names) strm) outs ins))
          mergeNested d = Port "o0" (E (Entity (TraceVal [n] s) [("o0",ty)] [("i0",ty,d)]))

addSuffixToProbeNames :: [ProbeName] -> String -> [ProbeName]
addSuffixToProbeNames pns suf = [ case pn of
                                    Probe name a i -> Probe (name ++ suf) a i
                                    WholeCircuit s a i -> WholeCircuit (s ++ suf) a i
                                | pn <- pns ]

class Probe a where
    probe' :: [ProbeName] -> a -> a

instance (Clock c, Rep a) => Probe (CSeq c a) where
    probe' (n:_) (Seq s (D d)) = Seq s (D (insertProbe n strm d))
        where strm = toTrace s
    probe' [] (Seq _ _) = error "probe'2"

instance Rep a => Probe (Comb a) where
    probe' (n:_) (Comb s (D d)) = Comb s (D (insertProbe n strm d))
        where strm = toTrace $ S.fromList $ repeat s
    probe' [] _ = error "Can't add probe: no name supply available"

instance (Probe a, Probe b) => Probe (a,b) where
    probe' names (x,y) = (probe' (addSuffixToProbeNames names "-fst") x,
                          probe' (addSuffixToProbeNames names "-snd") y)

instance (Clock clk, Probe a) => Probe (HandShaken clk a) where
    probe' names (HandShaken f) = HandShaken $ \ ready ->
                        let ready' = probe' (addSuffixToProbeNames names "-arg") ready
                        in probe' (addSuffixToProbeNames names "-res") (f ready')

instance (Probe a, Probe b, Probe c) => Probe (a,b,c) where
    probe' names (x,y,z) = (probe' (addSuffixToProbeNames names "-fst") x,
                            probe' (addSuffixToProbeNames names "-snd") y,
                            probe' (addSuffixToProbeNames names "-thd") z)

instance (Probe a, M.Size x) => Probe (M.Matrix x a) where
    probe' _ _ = error "Probe(probe') not defined for Matrix"

instance (Probe a, Probe b) => Probe (a -> b) where
    probe' (n:ns) f x = probe' ns $ f (probe' [n] x)
    probe' [] _ _ = error "Can't add probe: no name supply available."

probeCircuit :: Int -> Fabric () -> IO [(ProbeName, TraceStream)]
probeCircuit n fabric = do
    rc <- (reifyFabric >=> mergeProbesIO) fabric

    return [(nm,TraceStream ty $ take n strm) | (_,Entity (TraceVal nms (TraceStream ty strm)) _ _) <- theCircuit rc
                      , nm <- nms ]

probeData :: DRG.Unique -> Circuit -> Maybe ([ProbeName], TraceStream)
probeData n circuit = case lookup n $ theCircuit circuit of
                        Just (Entity (TraceVal nms strm) _ _) -> Just (nms, strm)
                        _ -> Nothing

probeNames :: DRG.Unique -> Circuit -> [ProbeName]
probeNames n c = maybe [] fst $ probeData n c

probeValue :: DRG.Unique -> Circuit -> Maybe TraceStream
probeValue n c = snd <$> probeData n c
