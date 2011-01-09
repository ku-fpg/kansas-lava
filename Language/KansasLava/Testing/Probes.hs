{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,ExistentialQuantification,ScopedTypeVariables,UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | Probes log the shallow-embedding signals of a Lava circuit in the
-- | deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Testing.Probes (probeCircuit,probeNames,probeValue) where

import qualified Data.Reify.Graph as DRG

import Control.Monad
import Control.Applicative

import Data.Sized.Arith(X1_,X0_)
import Data.Sized.Ix
import Data.Sized.Signed
import Data.Sized.Unsigned
import qualified Data.Sized.Matrix as Matrix

import Data.Char
import Data.Bits
import Data.List

import Language.KansasLava
import Language.KansasLava.Internals

import Language.KansasLava.Testing.Utils

probeCircuit :: (Ports b) => Int -> b -> IO [(ProbeName, TraceStream)]
probeCircuit n applied = do
    rc <- (reifyCircuit >=> mergeProbesIO) applied

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
