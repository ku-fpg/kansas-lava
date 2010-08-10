{-# LANGUAGE FlexibleInstances, StandaloneDeriving, ScopedTypeVariables, FlexibleContexts, Rank2Types, ExistentialQuantification, TypeFamilies #-}
module Language.KansasLava.Testing.TruthTable where

import System.IO.Unsafe

import Control.Concurrent
import Control.Concurrent.Chan

import Data.Bits
import Data.List as L
import Data.Maybe

import Data.Sized.Arith as A
import Data.Sized.Matrix as M
import Data.Sized.Sampled as S

import Data.Sized.Signed as S
import Data.Sized.Unsigned as U

import Debug.Trace

import Language.KansasLava.Circuit
import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Netlist
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Testing.Bench
import Language.KansasLava.Testing.Output.Dot
import Language.KansasLava.Testing.Output.VCD
import Language.KansasLava.Testing.Probes
import Language.KansasLava.Testing.Utils
import Language.KansasLava.Utils
import Language.KansasLava.Wire

import Language.Netlist.GenVHDL

import System.Cmd
import System.Directory
import System.Environment
import qualified System.FilePath.Posix as FP
import System.IO

import qualified System.Posix.Env as Posix

import Text.PrettyPrint(render)

probeTT :: (Probe a, Ports b) => a -> (a -> b) -> IO [[String]]
probeTT f apply = do
        plist <- probeCircuit $ apply $ probe "tt" f
        return [valsXStream xs | (_, (ProbeValue _ xs)) <- probesFor "tt" plist]

asciiTT :: String -> Int -> [[String]] -> IO ()
asciiTT name entries tt = do
    putStrLn $ "Truth table for " ++ name
    putStrLn $ "======="
    putStrLn $ unlines
             $ take entries
             $ mergeWith (\x y -> x ++ " | " ++ y) tt

probesTT probes = unlines
                $ take 100
                $ mergeWith (\x y -> x ++ " | " ++ y)
                $ map (\(_, (ProbeValue _ xs)) -> valsXStream xs) probes

muxWaveform = do
    let expand n = concatMap (replicate n)
        allBools = [Just False, Just True, Nothing]
    tt <- probeTT (mux2 :: Seq Bool -> (Seq Bool, Seq Bool) -> Seq Bool)
                  (\f -> f (toSeq' $ cycle $ expand 9 allBools)
                           (toSeq' $ cycle $ expand 3 allBools,
                            toSeq' $ cycle allBools))

    asciiTT "mux2" 27 tt

showTruthTable :: (TruthTable a) => String -> a -> IO ()
showTruthTable name fn = asciiTT name (L.length $ head tt) tt
    where tt = truthTable fn

truthTable :: (TruthTable a) => a -> [[String]]
truthTable fn = L.transpose [ l | ls <- genTT fn, l <- asciiTTV ls ]

asciiTTV :: TTV -> [[String]]
asciiTTV (ArgValue nm rest) = [ nm : rs | rss <- map asciiTTV rest, rs <- rss ]
asciiTTV (ResValue str)      = [[ str ]]

class TruthTable a where
    genTT :: a -> [TTV]

data TTV = ArgValue String [TTV]
         | ResValue String
    deriving Show

instance (RepWire a) => TruthTable (Comb a) where
    genTT c = [ ResValue $ showRepWire (undefined :: a) (combValue c) ]

instance (Enum (WIDTH w), Size (WIDTH w), RepWire w, TruthTable b) => TruthTable (Comb w -> b) where
    genTT fn = [ ArgValue (showRepWire (undefined :: w) a)
                           (genTT (fn (shallowComb a)))
               | a <- map optX $ args ++ [Nothing] ]
        where
            args :: [Maybe w]
            args = [ toWireRep rep | rep <- allWireReps ]

