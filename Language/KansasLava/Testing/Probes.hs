{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,ExistentialQuantification,ScopedTypeVariables,UndecidableInstances, TypeSynonymInstances, TypeFamilies, GADTs #-}
-- | The VCD module logs the shallow-embedding signals of a Lava circuit in the
-- deep embedding, so that the results can be observed post-mortem.
module Language.KansasLava.Testing.Probes where -- (Probe,fromXStream,toXStream,mkTrace,run,probeCircuit,probe,getProbe,probesFor) where

import Data.Sized.Arith(X1_,X0_)
import Data.Sized.Ix
import Data.Sized.Signed
import Data.Sized.Unsigned
import qualified Data.Sized.Matrix as Matrix

import Data.Char
import Data.Bits
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Language.KansasLava.Circuit
import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Stream hiding (head,zipWith)
import Language.KansasLava.Type
import Language.KansasLava.Utils
import Language.KansasLava.Wire

import Language.KansasLava.Trace

import qualified Data.Graph.Inductive as G

import qualified Data.Reify.Graph as DRG

data Thunk b = forall a. (Ports a, Probe a) => Thunk a (a -> b)
mkThunk a b = Thunk a b

runT :: Thunk b -> b
runT (Thunk circuit fn) = fn circuit

mkTrace' :: (Ports a, Probe a) => Int -> Thunk a -> IO Trace
mkTrace' i (Thunk circuit fn) = mkTrace i circuit fn

mkTrace :: (Ports a, Probe a, Ports b) => Int -> a -> (a -> b) -> IO Trace
mkTrace c circuit apply = do
    let probed = probe "wholeCircuit" circuit

    rc <- reifyCircuit [] $ probed
    rc' <- reifyCircuit [] $ apply $ probed -- this is essentially what probeCircuit does

    let pdata = M.fromList [(k,v) | (_,Entity _ _ _ attrs) <- theCircuit rc'
                                  , ProbeValue k v <- attrs ]
        entities = [(id,e) | (id,e@(Entity _ _ _ attrs)) <- theCircuit rc
                           , ProbeValue n v <- attrs]
        pnodes = map fst entities
        ins = M.fromList [ (k,fromJust $ M.lookup name pdata)
                         | (_,Entity _ _ [(_,_,Pad k)] attrs) <- entities
                         , ProbeValue name _ <- attrs]
        out = fromJust $ M.lookup
                         (head [k | let order = G.bfsn [id | (_,_,Port _ id) <- theSinks rc] graph
                                  , let sink = head $ intersect order pnodes
                                  , Just (Entity _ _ _ attrs) <- [lookup sink $ theCircuit rc]
                                  , ProbeValue k _ <- attrs ])
                         pdata
        graph :: G.Gr (MuE DRG.Unique) ()
        graph = rcToGraph rc

    return $ Trace { cycles = c, inputs = ins, outputs = out, probes = pdata }

-- | 'probeCircuit' takes a something that can be reified and
-- | generates an association list of the values for the probes in
-- | that circuit.
probeCircuit :: (Ports a) =>
           a        -- ^ The Lava circuit.
           -> IO [(String,Annotation)]
probeCircuit circuit = do
    rc <- reifyCircuit [] circuit
    let evts = [(n ++ "_" ++ show i,pv) | (_,Entity _ _ _ attrs) <- theCircuit rc
                       , pv@(ProbeValue (PadVar i n) v) <- attrs]
    return evts

-- | 'getProbe' takes an association list of probe values and a probe
-- | name, and returns the trace (wrapped in a ProbeValue) from the probe.
getProbe :: [(String,Annotation)] -> String ->  Maybe Annotation
getProbe ps nm = lookup nm ps

-- | 'probesFor' takes an association list of probe values and a probe
-- | name, and returns an association list containing only those probes
-- | related to the probed function, in argument order.
probesFor :: String -> [(String,Annotation)] -> [(String,Annotation)]
probesFor name plist =
    sortBy (\(n1, _) (n2, _) -> compare n1 n2) $
    filter (\(n, _) -> name `isPrefixOf` n) plist

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

instance (Show a, RepWire a) => Probe (CSeq c a) where
    attach i name (Seq s (D d)) = Seq s (D (addAttr pdata d))
        where pdata = ProbeValue (PadVar i name) (wireType witness, fromXStream witness s)
              witness = (error "probe" :: a)

instance (Show a, RepWire a) => Probe (Comb a) where
    attach i name c@(Comb s (D d)) = Comb s (D (addAttr pdata d))
        where pdata = ProbeValue (PadVar i name) (wireType witness, fromXStream witness (fromList $ repeat s))
              witness = (error "probe" :: a)

-- TODO: consider, especially with seperate clocks
--instance Probe (Clock c) where
--    probe probeName c@(Clock s _) = Clock s (D $ Lit 0)	-- TODO: fix hack by having a deep "NULL" (not a call to error)

-- AJG: The number are hacks to make the order of rst before clk work.
-- ACF: Revisit this with new PadVar probe names
instance Probe (Env c) where
    attach i name (Env clk rst clk_en) = Env clk (attach i (name ++ "_0rst") rst)
 						                         (attach i (name ++ "_1clk_en") clk_en)

instance (Show a, Show b,
          RepWire a, RepWire b,
          Size (ADD (WIDTH a) (WIDTH b)),
          Enum (ADD (WIDTH a) (WIDTH b)),
          Probe (f (a,b)),
          Pack f (a,b)) => Probe (f a, f b) where
    attach i name c = val
        where packed :: f (a,b)
              packed = attach i name $ pack c
              val :: (f a, f b)
              val = unpack packed

instance (Show a, Show b, Show c,
          RepWire a, RepWire b, RepWire c,
          Size (ADD (WIDTH a) (WIDTH b)),
          Enum (ADD (WIDTH a) (WIDTH b)),
          Probe (f (a,b,c)),
          Pack f (a,b,c)) => Probe (f a, f b, f c) where
    attach i name c = val
        where packed :: f (a,b,c)
              packed = attach i name $ pack c
              val :: (f a, f b, f c)
              val = unpack packed

instance (Show a, Probe a, Probe b) => Probe (a -> b) where
    -- this shouldn't happen, but if it does, discard int and generate fresh order
    attach _ = probe

    -- The default behavior for probing functions is to generate fresh ordering
    probe name f =  probe' name [0..] f

    probe' name (i:is) f x = probe' name is $ f (attach i name x)

addAttr :: Annotation -> Driver E -> Driver E
addAttr value (Port v (E (Entity n outs ins attrs))) =
            Port v (E (Entity n outs ins $ attrs ++ [value]))
-- TODO: Above is a hack for multiple probes on single node. Idealy want to just store this once with
-- multiple names, since each probe will always observe the same sequence.
addAttr value@(ProbeValue _ (ty,_)) d@(Pad (PadVar _ v)) =
  (Port (Var "o0")
          (E (Entity (Name "probe" v) [(Var "o0", ty)] [(Var "i0", ty,d)]
                       [value])))
addAttr value@(ProbeValue _ (ty,_)) d@(Lit x) =
            (Port (Var "o0")
             (E (Entity (Name "probe" "lit") [(Var "o0", ty)] [(Var "i0", ty,d)]
                 [value])))
addAttr value@(ProbeValue _ (ty,_)) d@(Error _) =
            (Port (Var "o0")
             (E (Entity (Name "probe" "lit") [(Var "o0", ty)] [(Var "i0", ty,d)]
                 [value])))
addAttr _ driver = error $ "Can't probe " ++ show driver

{- showXStream is a utility function for printing out stream representations.
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

-}
