{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes, ExistentialQuantification, ScopedTypeVariables, UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | Probes log the shallow-embedding signals of a Lava circuit in the
-- | deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Probes (
      -- * inserting probes
      probeS, unpackedProbe,

      -- * setting up the debugging mode
      setProbesAsTrace, setProbes
 ) where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types

import System.IO.Unsafe
import Data.IORef


{-# NOINLINE probeS #-}
-- | probeS adds a named probe to the front of a signal.
probeS :: (Clock c, Rep a) => String -> Signal c a -> Signal c a
probeS str sig = unsafePerformIO $ do
        (ProbeFn fn) <- readIORef probeFn
        fn str sig

unpackedProbe :: forall c a . (Clock c, Rep a, Pack c a) => String -> Unpacked c a -> Unpacked c a
unpackedProbe nm a = unpack (probeS nm (pack a) :: Signal c a)

data ProbeFn = ProbeFn (forall a i . (Rep a, Clock i) => String -> Signal i a -> IO (Signal i a))

{-# NOINLINE probeFn #-}
probeFn :: IORef ProbeFn
probeFn = unsafePerformIO $ newIORef $ ProbeFn $ \ _ s -> return s

-- | unsafe function; used internally for initializing debugging hooks.
setProbes :: (forall a i . (Rep a, Clock i) => String -> Signal i a -> IO (Signal i a)) -> IO ()
setProbes = writeIORef probeFn . ProbeFn
        
setProbesAsTrace :: (String -> IO ()) -> IO ()
setProbesAsTrace write = setProbes $ \ nm (Signal s d) -> return $
        Signal (S.zipWith (\ i x -> unsafePerformIO $ do
                                        write' (nm ++ "(" ++ showRep i ++ ")" ++ showRep x ++ "\n")
                                        return x)
                          (S.fromList $ map pureX [1..] :: S.Stream (X Int))
                          s
               ) d
   where
           write' str | isEvaluated str = write str
                      | otherwise       = error "strictness error"
           isEvaluated [] = True
           isEvaluated (x:xs) = x `seq` isEvaluated xs

--setVCDProbes :: IO VCD
--setVCDProbes = undefined


