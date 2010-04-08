{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,ExistentialQuantification,ScopedTypeVariables,StandaloneDeriving, DeriveDataTypeable, UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | The VCD module logs the shallow-embedding signals of a Lava circuit in the
-- deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Probes(ProbeValue(..),XStream(..),probeCircuit,probe,getProbe) where

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



-- | 'probeCircuit' takes a something that can be reified and
-- | generates an association list of the values for the probes in
-- | that circuit.
probeCircuit :: (Ports a) =>
           a        -- ^ The Lava circuit.
           -> IO [(String,ProbeValue)]
probeCircuit circuit = do
    rc <- reifyCircuit [] circuit
    let evts = [(n,pv) | (_,Entity _ _ _ attrs) <- theCircuit rc,
                Just val <- [lookup "simValue" attrs],
                Just pv@(ProbeValue n v) <- [fromDynamic val]]
    return evts

-- | 'getProbe' takes a association list of probe values and a probe
-- | name, and returns the trace (wrapped in a ProbeValue) from the probe.
getProbe :: [(String,ProbeValue)] -> String ->  Maybe ProbeValue
getProbe ps nm = lookup nm ps

-- | 'probe' indicates a Lava shallowly-embedded value should be logged with the given name.
class  Probe a where
  probe :: String -> a -> a
  -- probe' is used for a name supply.
  probe' :: String -> [Var] -> a -> a

instance (RepWire a, Typeable a) => Probe (Seq a) where
  probe probeName (Seq s (D d)) = Seq s (D (addAttr probeName strm d))
   where strm :: XStream a
         strm = XStream s
  probe' probeName ((Var v):_) s = probe (probeName ++ "_" ++ v) s

instance (RepWire a, Typeable a) => Probe (Comb a) where
   probe probeName c@(Comb s (D d)) = Comb s (D (addAttr probeName strm d))
     where strm :: XStream a
           strm = XStream $ fromList $ repeat s
   probe' probeName ((Var v):_) s = probe (probeName ++ "_" ++ v) s

instance (Probe a, Probe b) => Probe (a -> b) where
  -- The default behavior for probing functions is to generate fresh names.
  probe probeName f =  probe' probeName vars f
   where vars = [Var $ show i | i <- [0..]]

  probe' probeName ((Var v):vs) f x = probe' probeName vs $ f (probe (probeName ++ "_" ++ v) x)

addAttr :: forall a . (RepWire a, Typeable a) => String -> XStream a -> Driver E -> Driver E
addAttr probeName value (Port v (E (Entity n outs ins _))) =
            Port v (E (Entity n outs ins [("simValue", (toDyn (ProbeValue probeName value)))]))
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



data ProbeValue = forall a. (RepWire a, Typeable a) => ProbeValue String (XStream  a) deriving Typeable


data XStream a = XStream (Stream (X a)) deriving Typeable

-- This was necessary to satisfy Data.Dynamic
deriving instance Typeable X0
deriving instance Typeable1 X1_
deriving instance Typeable1 X0_
deriving instance Typeable1 Signed
deriving instance Typeable1 Unsigned

deriving instance Typeable1 WireVal
deriving instance Eq a => Eq (WireVal a)

-- showXStream is a utility function for printing out stream representations.
showXStream :: forall a. RepWire a => XStream a -> Stream String
showXStream (XStream strm) = fmap (showRepWire (undefined :: a)) strm

-- A test circuit
f :: Comb U8 -> Comb U8 -> Comb U8
f x y = x + y

