{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,ExistentialQuantification,ScopedTypeVariables,StandaloneDeriving, DeriveDataTypeable, UndecidableInstances #-}
-- | The VCD module logs the shallow-embedding signals of a Lava circuit in the
--   Verilog (yes, it shouldn't be in the VHDL hierarchy) format for viewing in
--   a waveform viewer.
module Language.KansasLava.VHDL.VCD(vcdCircuit,probeCircuit,probe,getProbe,ProbeValue(..),VCDFmt, VCDValue(..)) where

import Language.KansasLava
import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Ix
import Data.Sized.Arith(X1_,X0_)

import Data.Char
import Data.Bits
import Data.Dynamic
import Data.Maybe
import Data.Char



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
    rc <- reifyCircuit [] circuit
    let evts = [(n,pv) | (_,Entity _ _ _ attrs) <- theCircuit rc,
                Just val <- [lookup "simValue" attrs],
                Just pv@(ProbeValue n _) <- [fromDynamic val]]

    putStrLn $ "There are " ++ show (length evts) ++ " probes"
    let vcd = format end evts
    writeFile (circuitName ++ ".vcd") vcd
    return ()

-- | 'probeCircuit' takes a something that can be reified and
-- | generates an association list of the values for the probes in
-- | that circuit.
probeCircuit :: (Ports a) =>
           a        -- ^ The Lava circuit.
           -> IO [(String,Dynamic)]
probeCircuit circuit = do
    rc <- reifyCircuit [] circuit
    let evts = [(n,toDyn v) | (_,Entity _ _ _ attrs) <- theCircuit rc,
                Just val <- [lookup "simValue" attrs],
                Just pv@(ProbeValue n v) <- [fromDynamic val]]
    return evts

-- | 'getProbe' takes a association list of probe values and a probe
-- | name, and returns the trace (as a list) from the probe.
getProbe :: forall a . Typeable a => [(String,Dynamic)] -> String ->  [Maybe a]
getProbe ps nm = case lookup nm ps of
                   Just val -> toList (fromDyn val (error "getProbe" :: Stream (Maybe a)))
                   Nothing -> []

-- | 'probe' indicates a Lava 'Signal' should be logged to VCD format, with the given name.
probe :: forall t. (Typeable (X t), VCDSize (X t), VCDValue (X t), Wire (Stream t), VCDFmt t) =>
         String -> Seq t -> Seq t
probe probeName (Seq s (D d)) = Seq s (D (addAttr d))
  where addAttr (Port v (E (Entity n outs ins _))) =
            Port v (E (Entity n outs ins [("simValue", (toDyn (ProbeValue probeName s)))]))
        addAttr d@(Pad (Var v)) =
            (Port (Var "o0")
             (E (Entity (Name "probe" v) [(Var "o0", ty)] [(Var "i0", ty,d)]  [("simValue", (toDyn (ProbeValue probeName s)))])))
          where ty = wireType (error "probe/oTy" :: Stream t)
        addAttr $ driver = error "Can't probe " ++ driver

-- Below this is all implementation.

type TimeTag = Int
type TaggedEvents = [(TimeTag,VCDVal)]

-- | taggedEvent takes a Seq and reduces it to a tagged (time,value) stream, eliminating
--   no-change times.
taggedEvent :: (VCDFmt t, Ord a, Num a) =>
               a -> Stream t -> [(a, VCDVal)]
taggedEvent maxTime (h :~ tl) = (0,VCDVal h):(taggedEvent' 1 h tl)
  where taggedEvent' time old (new :~ as)
                     | time >= maxTime = []
                     | old == new = taggedEvent' (time + 1) old as
                     | otherwise  = (time,VCDVal new):(taggedEvent' (time + 1) new as)

-- | taggedEvents takes a collection of Sequences, and converts it to a list of
-- | tagged events, where each time stamp may have multiple value assignments.
taggedEvents :: [a] -> [TaggedEvents] -> [(Int, [(a, VCDVal)])]
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
format :: TimeTag -> [(String,ProbeValue)] -> String
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
  where decl (n,val) ident =
          "$var wire " ++ show (vcdSize val) ++ " " ++ ident ++ " " ++ n ++ " $end"
        fmtTimeStep (t,vals) =
          ["#" ++ show t] ++
          [vcdFmt v++ident | (ident,v) <- vals] -- no space between value and identifier
        tes = [taggedEvent maxTime a | (_,ProbeValue _ a) <- seqs]
        evts = taggedEvents identifier_code tes



-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
identifier_code :: [String]
identifier_code = res
  where chars = [(chr 33)..(chr 126)]
        ids@(_:res) = [[]]  ++ concatMap (\i -> [c:i | c <- chars]) ids

-- The support classes for holding values
class Eq a => VCDValue a where
  vcdFmt :: a -> String

class VCDSize a where
  vcdSize :: a -> Int

class (Typeable a, VCDValue a, VCDSize a) => VCDFmt a

instance VCDFmt a => VCDValue (Maybe a) where
  vcdFmt Nothing = 'b':(replicate (vcdSize (undefined :: a)) 'X')
  vcdFmt (Just a) = vcdFmt a

instance VCDSize a => VCDSize (Maybe a) where
  vcdSize _ = vcdSize (undefined :: a)


instance VCDValue Bool where
  vcdFmt True = "1"
  vcdFmt False = "0"

instance VCDSize Bool where
  vcdSize _ = 1

instance (Typeable a,VCDSize a, VCDValue a) => VCDFmt a
instance Show VCDVal where
  show (VCDVal x) = vcdFmt x

instance Eq VCDVal where
  (VCDVal _) == (VCDVal _) = False

instance VCDValue VCDVal where
  vcdFmt (VCDVal v) = vcdFmt v


data VCDVal = forall a. VCDFmt a => VCDVal a

data ProbeValue = forall a. VCDFmt a => ProbeValue String (Stream a) deriving Typeable

instance  VCDSize ProbeValue where
  vcdSize (ProbeValue _ v) = vcdSize v

instance VCDSize a => VCDSize (Stream a) where
  vcdSize _ = vcdSize (undefined :: a)

instance Size ix => VCDSize (Unsigned ix) where
  vcdSize _ = size (undefined :: ix)

instance (Size ix, Enum ix, Integral ix) =>VCDValue (Unsigned ix) where
  vcdFmt v = 'b':(concatMap vcdFmt [testBit v i | i <- reverse [0..width-1]])
    where width = size (undefined :: ix)


instance Size ix => VCDSize (Signed ix) where
  vcdSize _ = size (undefined :: ix)

instance (Integral ix, Size ix, Enum ix) =>VCDValue (Signed ix) where
  vcdFmt v = 'b':(concatMap vcdFmt [testBit v i | i <- reverse [0..width-1]])
    where width = size (undefined :: ix)


-- This was necessary to satisfy Data.Dynamic
deriving instance Typeable1 Stream


{-
-- Test Circuits
halfAdder :: (Signal Bool,Signal Bool) -> (Signal Bool,Signal Bool)
halfAdder (a,b) = (sum,carry)
  where sum = probe "sum" $ a `xor2` b
        carry = probe "carry" $ a `and2` b

fullAdder :: Signal Bool -> (Signal Bool, Signal Bool) -> (Signal Bool, Signal Bool)
fullAdder c (a,b) = (s2,c2 `xor2` c1)
  where (s1,c1) = halfAdder (a,b)
	(s2,c2) = halfAdder (s1,c)


trajectory :: (OpType a) => Time -> [Signal a] -> Signal a
trajectory clk l = foldr c initVal l
  where c a as = delay clk a as


a = trajectory clock (take 100 $ cycle [low,high,low])
b = trajectory clock (take 100 $ cycle [high,low])

ua = probe "ua" $ trajectory clock ((map fromInteger [0..100]) :: [Signal (Unsigned X8)])

plus :: Signal (Unsigned X8) -> Signal (Unsigned X8) -> Signal (Unsigned X8)
plus x y = x' + y'
  where x' = probe "x" x
        y' = probe "y" y

-}


deriving instance Typeable X0
deriving instance Typeable1 X1_
deriving instance Typeable1 X0_
deriving instance Typeable1 Signed


