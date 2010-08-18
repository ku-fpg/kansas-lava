{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,ExistentialQuantification,ScopedTypeVariables,StandaloneDeriving, DeriveDataTypeable, UndecidableInstances #-}
-- | The VCD module logs the shallow-embedding signals of a Lava circuit in the
--   Verilog (yes, it shouldn't be in the VHDL hierarchy) format for viewing in
--   a waveform viewer.
module Language.KansasLava.Testing.Output.VCD(vcdCircuit) where

import Language.KansasLava
import Language.KansasLava.Testing.Probes

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Ix
import Data.Sized.Arith(X1_,X0_)

import Data.Char
import Data.Bits
import Data.Maybe
import Data.Char

vcdCircuit = error "vcdCircuit: TBD"
{-
 - AJG: I'm breaking VCD, because its the only place we use Eq over Wires.
 - We should actually check the underlying form/bits anyway.

-- | 'vcdCircuit' simulates a circuit and logs the probes to a file. The
--   function takes a name parameter for the circuit and generates a file
--   @name.vcd@ in the cwd.  The circuit type must implement the 'Ports' class
--   so that we can use observable sharing but in reality the circuit must not
--   be a function type.
vcdCircuit :: (Ports a) =>
              String   -- ^ The name of theoutput file
           -> Int  -- ^ The maximum number of clock ticks
           -> a        -- ^ The Lava circuit.
           -> IO ()
vcdCircuit circuitName end circuit = do
    evts <- probeCircuit circuit

    putStrLn $ "There are " ++ show (length evts) ++ " probes"
    let vcd = format end evts
    writeFile (circuitName ++ ".vcd") vcd
    return ()

-- Below this is all implementation.

type VCDVal = String

type TimeTag = Int
type TaggedEvents = [(TimeTag,VCDVal)]

data XVal a = XVal (X a)
xstrmHead :: XStream a -> XVal a
xstrmHead (XStream (a :~ _)) = XVal a

xstrmTail :: XStream a -> XStream a
xstrmTail (XStream (_ :~ as)) = XStream as




-- | taggedEvent takes a Seq and reduces it to a tagged (time,value) stream, eliminating
--   no-change times.
taggedEvent :: forall a. ( Ord a, Num a) => a -> Annotation -> [(a, VCDVal)]
taggedEvent maxTime (ProbeValue _ (ty,vals)) = error "FIX ME: taggedEvent in Testing/Output/VCD.hs"
{-
taggedEvent :: forall a t. ( Ord a, Num a, RepWire t) => a -> XStream t -> [(a, VCDVal)]
taggedEvent maxTime (XStream (h :~ tl)) = (0,showWire h):[] -- :(taggedEvent' 1 (unX h) (XStream tl))
  where taggedEvent' :: a -> Maybe t -> XStream t -> [(a,VCDVal)]
        taggedEvent' time old strm
                     | time >= maxTime = []
                     | old == new = taggedEvent' (time + 1) old as
                     | otherwise  = (time,showWire xnew):(taggedEvent' (time + 1) new as)
          where new :: Maybe t
                new = unX xnew
                hd :: XVal t
                hd = xstrmHead strm
                XVal xnew = hd

                as :: XStream t
                as = xstrmTail strm
        showWire = showRepWire (error "taggedEvent:showWire" :: t)
-}

-- | taggedEvents takes a collection of Sequences, and converts it to a list of
-- | tagged events, where each time stamp may have multiple value assignments.
taggedEvents :: (Eq a) => [a] -> [TaggedEvents] -> [(Int, [(a, VCDVal)])]
taggedEvents identifiers tags = collated labeled
  where labeled = zip identifiers tags
        -- collated :: [(Int,TaggedEvents)] -> [(Time,[(Int,VCDVal )])]
        collated [] = []
        collated labEvents =
                     let minTime = minimum $ map (maxHead . snd) labEvents
                         maxHead :: TaggedEvents -> Int
                         maxHead [] = maxBound
                         maxHead ((t,_):_) = t
                         -- next :: (Int,TaggedEvents) -> (Maybe (Int,VCDVal), (Int,TaggedEvents))
                         next (_,[]) = (Nothing,Nothing)
                         next (l,es@((t,v):vs))
                           | t == minTime = (Just (l,v), Just (l,vs))
                           | otherwise  = (Nothing,Just (l,es))
                         (this,rest) = unzip $ map next labEvents
                      in case catMaybes this of
                           [] -> collated (catMaybes rest)
                           hd -> (minTime,hd):(collated (catMaybes rest))



-- format creates the actual VCD formatted dump.
format :: TimeTag -> [(String,Annotation)] -> String
format maxTime seqs = unlines $ [
          "$timescale 10ns $end", -- Timescale section
          "$scope module logic $end"] -- scope section
         ++
         -- declarations section
         zipWith decl seqs identifier_code ++
         ["$upscope $end",
          "$enddefinitions $end"] ++
         -- timestamp section
         concatMap fmtTimeStep evts
  where decl (n,ProbeValue _ (ty,_)) ident =
          "$var wire " ++ show (baseTypeLength ty) ++ " " ++ ident ++ " " ++ n ++ " $end"
        fmtTimeStep (t,vals) =
          ["#" ++ show t] ++
          [v++ident | (ident,v) <- vals] -- no space between value and identifier
        tes = [taggedEvent maxTime pval | (_,pval) <- seqs]
        evts = taggedEvents identifier_code tes

-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
identifier_code :: [String]
identifier_code = res
  where chars = [(chr 33)..(chr 126)]
        ids@(_:res) = [[]]  ++ concatMap (\i -> [c:i | c <- chars]) ids


-}
