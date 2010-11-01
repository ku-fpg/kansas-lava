{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,ExistentialQuantification,ScopedTypeVariables,UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | Probes log the shallow-embedding signals of a Lava circuit in the
-- | deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Testing.Probes (probeCircuit,probeNames,probeValue) where

import qualified Data.Reify.Graph as DRG

import Control.Monad

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

probeCircuit :: (Ports b) => Int -> b -> IO [(OVar, TraceStream)]
probeCircuit n applied = do
    rc <- (reifyCircuit >=> mergeProbesIO) applied

    return [(nm,TraceStream ty $ take n strm) | (_,Entity (TraceVal nms (TraceStream ty strm)) _ _ _) <- theCircuit rc
                      , nm <- nms ]

probeNames :: DRG.Unique -> Circuit -> [OVar]
probeNames n circuit = case lookup n $ theCircuit circuit of
                        Just (Entity (TraceVal nms _) _ _ _) -> nms
                        _ -> []

probeValue :: DRG.Unique -> Circuit -> TraceStream
probeValue n circuit = case lookup n $ theCircuit circuit of
                        Just (Entity (TraceVal _ strm) _ _ _) -> strm
                        _ -> Empty

{-
-- | 'probe' indicates a Lava shallowly-embedded value should be logged with the given name.
class Probe a where
    -- this is the public facing method
    probe :: String -> a -> a
    probe = attach 0

    -- this method is used internally to track order
    attach :: Int -> String -> a -> a

    -- probe' is used internally for a name supply.
    probe' :: String -> [Int] -> a -> a
    probe' name (i:_) s = attach i name s

    run :: a -> Trace -> TraceStream

instance (Rep a) => Probe (CSeq c a) where
    attach i name (Seq s (D d)) = Seq s (D (insertProbe n strm d))
        where n = OVar i name
              strm = toTrace s

    run (Seq s _) (Trace c _ _ _) = TraceStream ty $ takeMaybe c strm
        where TraceStream ty strm = toTrace s

instance (Rep a) => Probe (Comb a) where
    attach i name c@(Comb s (D d)) = Comb s (D (insertProbe n strm d))
        where n = OVar i name
              strm = toTrace $ fromList $ repeat s

    run (Comb s _) (Trace c _ _ _) = TraceStream ty $ takeMaybe c strm
        where TraceStream ty strm = toTrace $ fromList $ repeat s

-- TODO: consider, especially with seperate clocks
--instance Probe (Clock c) where
--    probe probeName c@(Clock s _) = Clock s (D $ Lit 0)	-- TODO: fix hack by having a deep "NULL" (not a call to error)

-- ACF: Packing these together perserves the idea of one trace in the tracemap
--      for each parameter to the function... but I don't like it.
instance Probe (Env c) where
    attach i name (Env clk rst clk_en) = Env clk rst' clk_en'
        where (rst',clk_en') = unpack $ attach i name $ (pack (rst, clk_en) :: CSeq c (Bool, Bool))

-- ACF: TODO: As you can see with tuples, we have name supply issues to solve.
--instance (Rep a, Rep b, Probe (f a), Probe (f b)) => Probe (f a, f b) where
instance (Probe a, Probe b) => Probe (a, b) where
    attach i name (x,y) = (attach i (name ++ "_1") x,
                           attach i (name ++ "_0") y)

    -- note order of zip matters! must be consistent with fromWireXRep
    run (x,y) t = TraceStream (TupleTy [ty1,ty2]) $ zipWith appendRepValue strm1 strm2
        where TraceStream ty1 strm1 = run x t
              TraceStream ty2 strm2 = run y t

instance (Probe a, Probe b, Probe c) => Probe (a, b, c) where
    attach i name (x,y,z) = (attach i (name ++ "_2") x,
                             attach i (name ++ "_1") y,
                             attach i (name ++ "_0") z)

    -- note order of zip matters! must be consistent with fromWireXRep
    run (x,y,z) t = TraceStream (TupleTy [ty1,ty2,ty3]) (zipWith appendRepValue strm1 $ zipWith appendRepValue strm2 strm3)
        where TraceStream ty1 strm1 = run x t
              TraceStream ty2 strm2 = run y t
              TraceStream ty3 strm3 = run z t

instance (Input a, Probe a, Probe b) => Probe (a -> b) where
    -- this shouldn't happen (maybe a higher order KL function?),
    -- but if it does, discard int and generate fresh order
    attach _ = probe

    -- The default behavior for probing functions is to generate fresh ordering
    probe name f =  probe' name [0..] f

    probe' name (i:is) f x = probe' name is $ f (attach i name x)

    run fn t@(Trace c ins _ _) = run fn' $ t { inputs = ins' }
        where (ins', fn') = apply ins fn

insertProbe :: OVar -> TraceStream -> Driver E -> Driver E
insertProbe n s@(TraceStream ty _) = mergeNested
    where mergeNested :: Driver E -> Driver E
          mergeNested (Port nm (E (Entity (TraceVal names strm) outs ins attrs)))
                        = Port nm (E (Entity (TraceVal (n:names) strm) outs ins attrs))
          mergeNested d = Port "o0" (E (Entity (TraceVal [n] s) [("o0",ty)] [("i0",ty,d)] []))

-}
