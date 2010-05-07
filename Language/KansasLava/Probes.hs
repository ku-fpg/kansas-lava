{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,ExistentialQuantification,ScopedTypeVariables,StandaloneDeriving, DeriveDataTypeable, UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | The VCD module logs the shallow-embedding signals of a Lava circuit in the
-- deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Probes (Probe,ProbeValue(..),XStream(..),probeCircuit,probe,getProbe,probesFor,valsXStream,bitsXStream,showXStream,showXStreamBits) where

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Ix
import Data.Sized.Arith(X1_,X0_)
import qualified Data.Sized.Matrix as M

-- so we can derive Typeable2 instances
import Data.Sized.Matrix (Matrix)
import Data.Sized.Sampled (Sampled)

import Data.Char
import Data.Bits
import Data.Dynamic
import Data.List

import Language.KansasLava.Circuit
import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Stream
import Language.KansasLava.Wire

-- | 'probeCircuit' takes a something that can be reified and
-- | generates an association list of the values for the probes in
-- | that circuit.
probeCircuit :: (Ports a) =>
           a        -- ^ The Lava circuit.
           -> IO [(String,ProbeValue)]
probeCircuit circuit = do
    rc <- reifyCircuit [] circuit
    let evts = [(n,pv) | (_,Entity _ _ _ attrs) <- theCircuit rc,
                ("simValue", val) <- attrs,
                Just pv@(ProbeValue n v) <- [fromDynamic val]]
    return evts

-- | 'getProbe' takes an association list of probe values and a probe
-- | name, and returns the trace (wrapped in a ProbeValue) from the probe.
getProbe :: [(String,ProbeValue)] -> String ->  Maybe ProbeValue
getProbe ps nm = lookup nm ps

-- | 'probesFor' takes an association list of probe values and a probe
-- | name, and returns an association list containing only those probes
-- | related to the probed function, in argument order.
probesFor :: String -> [(String,ProbeValue)] -> [(String,ProbeValue)]
probesFor name plist =
    sortBy (\(n1, _) (n2, _) -> compare n1 n2) $
    filter (\(n, _) -> name `isPrefixOf` n) plist

-- | 'probe' indicates a Lava shallowly-embedded value should be logged with the given name.
class  Probe a where
  probe :: String -> a -> a
  -- probe' is used for a name supply.
  probe' :: String -> [Var] -> a -> a

instance (Show a, RepWire a, Typeable a) => Probe (Seq a) where
  probe probeName (Seq s (D d)) = Seq s (D (addAttr probeName strm d))
   where strm = XStream s :: XStream a
  probe' probeName ((Var v):_) s = probe (probeName ++ "_" ++ v) s

instance (Show a, RepWire a, Typeable a) => Probe (Comb a) where
   probe probeName c@(Comb s (D d)) = Comb s (D (addAttr probeName strm d))
     where strm :: XStream a
           strm = XStream $ fromList $ repeat s
   probe' probeName ((Var v):_) s = probe (probeName ++ "_" ++ v) s

instance (Show a, Show b,
          RepWire a, RepWire b,
          Typeable a, Typeable b,
          Size (ADD (WIDTH a) (WIDTH b)),
          Enum (ADD (WIDTH a) (WIDTH b)),
          Probe (f (a,b)),
          Pack f (a,b)) => Probe (f a, f b) where
   probe probeName c = val
      where packed :: f (a,b)
            packed =  probe probeName $ pack c
            val :: (f a, f b)
            val = unpack packed
   probe' probeName ((Var v):_) s = probe (probeName ++ "_" ++ v) s


instance (Show a, Probe a, Probe b) => Probe (a -> b) where
  -- The default behavior for probing functions is to generate fresh names.
  probe probeName f =  probe' probeName vars f
   where vars = [Var $ show i | i <- [0..]]

  probe' probeName ((Var v):vs) f x = probe' probeName vs $ f (probe (probeName ++ "_" ++ v) x)

addAttr :: forall a . (Show a, RepWire a, Typeable a) => String -> XStream a -> Driver E -> Driver E
addAttr probeName value (Port v (E (Entity n outs ins attrs))) =
            Port v (E (Entity n outs ins $ attrs ++ [("simValue", (toDyn (ProbeValue probeName value)))]))
-- TODO: Above is a hack for multiple probes on single node. Idealy want to just store this once with
-- multiple names, since each probe will always observe the same sequence.
addAttr probeName value d@(Pad (Var v)) =
  (Port (Var "o0")
          (E (Entity (Name "probe" v) [(Var "o0", ty)] [(Var "i0", ty,d)]
                       [("simValue", (toDyn (ProbeValue probeName value)))])))
             where ty = wireType (error "probe/oTy" :: a)
addAttr probeName value d@(Lit x) =
            (Port (Var "o0")
             (E (Entity (Name "probe" "lit") [(Var "o0", ty)] [(Var "i0", ty,d)]
                 [("simValue", (toDyn (ProbeValue probeName value)))])))
            where ty = wireType (error "probe/oTy" :: a)
addAttr _ _ driver = error $ "Can't probe " ++ show driver



data ProbeValue = forall a. (Show a, RepWire a, Typeable a) => ProbeValue String (XStream  a) deriving Typeable

instance Show ProbeValue where
    show (ProbeValue n v) = "ProbeValue " ++ show n ++ " " ++ show v

data XStream a = XStream (Stream (X a)) deriving Typeable

-- This was necessary to satisfy Data.Dynamic
deriving instance Typeable X0
deriving instance Typeable1 X1_
deriving instance Typeable1 X0_
deriving instance Typeable1 Signed
deriving instance Typeable1 Unsigned
deriving instance Typeable2 Matrix
deriving instance Typeable2 Sampled

deriving instance Typeable1 WireVal
deriving instance Eq a => Eq (WireVal a)

-- showXStream is a utility function for printing out stream representations.
instance RepWire a => Show (XStream a) where
    show xs = show $ foldr (\i r -> i ++ ", " ++ r) "..." $ take 30 $ valsXStream xs

showXStream :: forall a. RepWire a => XStream a -> Stream String
showXStream (XStream strm) = fmap (showRepWire (undefined :: a)) strm

-- bitsXStream creates a list of binary representations of the values in the stream.
bitsXStream :: forall a. RepWire a => XStream a -> [String]
bitsXStream (XStream strm) = showSeqBits ((shallowSeq strm) :: Seq a)

-- valsXStream creates a list of string representations of the values in the stream.
valsXStream :: forall a. RepWire a => XStream a -> [String]
valsXStream (XStream strm) = showSeqVals ((shallowSeq strm) :: Seq a)

showXStreamBits :: forall a . (RepWire a) => XStream a -> Stream String
showXStreamBits (XStream ss) =
    fmap (\i -> (map showX $ reverse $ M.toList $ (fromWireXRep witness (i :: X a)))) ss
       where showX b = case unX b of
			Nothing -> 'X'
			Just True -> '1'
			Just False -> '0'
             witness = error "witness" :: a

-- A test circuit
-- f :: Comb U8 -> Comb U8 -> Comb U8
-- f x y = x + y

