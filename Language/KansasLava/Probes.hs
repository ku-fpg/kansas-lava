{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes, ExistentialQuantification, ScopedTypeVariables, UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | Probes log the shallow-embedding signals of a Lava circuit in the
-- | deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Probes (
      -- * Probes
      probeS, unpackedProbe,

      -- * Setting up the debugging mode for probes
      setProbesAsTrace, setShallowProbes, setProbes
 ) where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import qualified Language.KansasLava.Stream as S
-- import Language.KansasLava.Trace
import Language.KansasLava.Types

import System.IO.Unsafe
import Data.IORef


{-# NOINLINE probeS #-}
-- | 'probeS' adds a named probe to the front of a signal.
probeS :: (Clock c, Rep a) => String -> Signal c a -> Signal c a
probeS str sig = unsafePerformIO $ do
        (ProbeFn fn) <- readIORef probeFn
        return (fn str sig)

-- | 'unpackedProbe' is an unpacked version of 'probeS'.
unpackedProbe :: forall c a p . (Clock c, Rep a, Pack c a, p ~ Unpacked c a) => String -> p -> p
unpackedProbe nm a = unpack (probeS nm (pack a) :: Signal c a)

data ProbeFn = ProbeFn (forall a i . (Rep a, Clock i) => String -> Signal i a -> Signal i a)

{-# NOINLINE probeFn #-}
probeFn :: IORef ProbeFn
probeFn = unsafePerformIO $ newIORef $ ProbeFn $ \ _ s -> s

-- | Used internally for initializing debugging hooks, replaces all future calls to probe
-- with the given function.
setProbes :: (forall a i . (Rep a, Clock i) => String -> Signal i a -> Signal i a) -> IO ()
setProbes = writeIORef probeFn . ProbeFn

-- | The callback is called for every element of every probed value, in evaluation order.
-- The arguments are fully evaluted (so printing them will not cause any side-effects of evaluation.
setShallowProbes :: (forall a . (Rep a) => String -> Integer -> X a -> X a) -> IO ()
setShallowProbes write = setProbes $ \ nm sig -> shallowMapS (probe_shallow nm) sig
  where
        probe_shallow :: forall a . (Rep a) => String -> S.Stream (X a) -> S.Stream (X a)
        probe_shallow nm = id
                      . S.fromList
                      . map (\ (i,a) -> write nm i a)
                      . zip [1..]
                      . S.toList

-- | A simplified API, where each internal probe event is represented
-- as a newline-terminated String, and can be printed, or appended to a file.
--
-- To append to a debugging file, use
--
-- >ghci> setProbesAsTrace $ appendFile "DEBUG.out"
--
-- To write to the screen, use
--
-- >ghci> setProbesAsTrace $ putStr
--
-- You will need to re-execute your program after calling any probe function,
-- so typically this done on the command line, or by puting setProbeAsTrace inside main.
setProbesAsTrace :: (String -> IO ()) -> IO ()
setProbesAsTrace write = setShallowProbes $ \ nm i a -> unsafePerformIO $ do
    write $ nm ++ "(" ++ show i ++ ")" ++ showRep a ++ "\n"
    return a

-- setProbesAsVCD :: VCD -> IO ()
-- setProbesAsVCD vcd = setShallowProbes $ \ nm i a -> do

