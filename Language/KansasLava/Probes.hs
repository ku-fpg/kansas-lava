{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes, ExistentialQuantification, ScopedTypeVariables, UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | Probes log the shallow-embedding signals of a Lava circuit in the
-- | deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Probes (
      -- * inserting probes
      probeS, unpackedProbe,

      -- * setting up the debugging mode
      setProbesAsTrace, setShallowProbes, setProbes
 ) where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types

import System.IO.Unsafe
import Data.IORef


{-# NOINLINE probeS #-}
-- | 'probeS' adds a named probe to the front of a signal.
probeS :: (Clock c, Rep a) => String -> Signal c a -> Signal c a
probeS str sig = unsafePerformIO $ do
        (ProbeFn fn) <- readIORef probeFn
        fn str sig

-- | 'unpackedProbe' is an unpacked version of 'probeS'.
unpackedProbe :: forall c a . (Clock c, Rep a, Pack c a) => String -> Unpacked c a -> Unpacked c a
unpackedProbe nm a = unpack (probeS nm (pack a) :: Signal c a)

data ProbeFn = ProbeFn (forall a i . (Rep a, Clock i) => String -> Signal i a -> IO (Signal i a))

{-# NOINLINE probeFn #-}
probeFn :: IORef ProbeFn
probeFn = unsafePerformIO $ newIORef $ ProbeFn $ \ _ s -> return s

-- | unsafe function; used internally for initializing debugging hooks.
setProbes :: (forall a i . (Rep a, Clock i) => String -> Signal i a -> IO (Signal i a)) -> IO ()
setProbes = writeIORef probeFn . ProbeFn

-- | The callback is called for every value of every probed value, in evaluation order.
setShallowProbes :: (forall a . (Rep a) => String -> Integer -> X a -> IO ()) -> IO ()
setShallowProbes write = setProbes $ \ nm sig -> 
        return $ dual (mkShallowS (probe_shallow nm (shallowS sig))) sig
  where
        probe_shallow :: forall a . (Rep a) => String -> S.Stream (X a) -> S.Stream (X a)
        probe_shallow nm = id
                      . S.fromList
                      . map (\ (i,a) -> unsafePerformIO $ do
                                                write nm i a
                                                return a)
                      . zip [1..]
                      . S.toList

setProbesAsTrace :: (String -> IO ()) -> IO ()
setProbesAsTrace write = setShallowProbes $ \ nm i x -> 
        write $ nm ++ "(" ++ show i ++ ")" ++ showRep x ++ "\n"

--setVCDProbes :: IO VCD
--setVCDProbes = undefined


