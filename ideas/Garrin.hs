
{-# LANGUAGE TypeFamilies,RankNTypes,GADTs,DeriveDataTypeable,ScopedTypeVariables,StandaloneDeriving,FlexibleInstances,ParallelListComp,UndecidableInstances #-}
module RTL where

import Language.KansasLava
-- import Language.KansasLava.Verification
import Data.Sized.Unsigned
import Data.Sized.Arith

import Control.Monad.RWS
import qualified Data.Map as M

import Data.Dynamic
import Data.Maybe
import Data.List(find,sortBy)
import Data.Graph
import Data.Tree


-- The 'Int' is a gensym. The Map is for the register outputs, which will be
type State = (Int, M.Map Int Dynamic)

-- Transaction IDs (TID) include a transaction name and the parent name.
type TID = [String]

-- A Log consists of a a pair of mappings.  The first associates with a
-- register/output, keyed by an Int, an assignment rhs. The assigned value is
-- lifted to be a dynamic (oh heterogenous maps, how i wish you were possible)
-- and includes the transaction TID guarding the assignment.
-- The second map associates each transaction TID the boolean guard.
data Log = Log (M.Map Int [(TID, Dynamic)]) (M.Map TID (Seq Bool))
	deriving Show

instance Monoid Log where
 (Log l1 t1) `mappend` (Log l2 t2) = Log (M.unionWith (++) l1 l2) (t1 `mappend` t2)
 mempty = Log mempty mempty

-- Inp is used for as the 'reader' monad argument. It carries a variety of data.
-- 1. assigns is passed, in circular programming style, from writer output to
-- reader input in circular programming style.
-- 2. inputs is a list of (shallow embedded) circuit inputs.
-- 3. env is the clock/reset environment for the registers
-- 4. TID is the current transaction ID.
data Inp = Inp { assigns :: Log
              , inputs :: M.Map String Dynamic
              , env :: Env ()
              , tid :: TID
              }

type RTL = RWS Inp Log State

-- Convert an RTL computation into a Log, from which we can get the circuit.
runRTL :: RTL a -> M.Map String Dynamic -> Log
runRTL m ins  = Log ws tmap'
  where ~(_,(_,st),Log ws tmap) = runRWS m init (0,M.empty)
        init = Inp (Log ws tmap') ins globalEnv []
        tmap' = makeTrans tmap


-- This is where we add an integer annotation to a circuit, allowing us to
-- get the ID
setRegId idx v = addAnnotation (Ann "regid" (toDyn idx)) v
getRegId v = getAnnotation "regid" v

newReg :: forall a . (Wire a,Typeable a) => Comb a -> RTL (Seq a)
newReg init = do
 (idx,rs) <- get
 (Log as tmap) <- asks assigns
 clk <- asks env
 -- This is some hacky bullshit, but it's necessary, since 'register' takes apart its
 -- last argument.
 let ~(Just res) = M.lookup idx as
     ~(Seq s d) = makeMux tmap (liftS0 init) res
     -- Note that we add the 'regid' annotation, so that we can get this
     -- later on in assignment.
     reg :: Seq a
     reg = setRegId idx (register clk init (Seq s d))
 put (idx+1,M.insert idx (toDyn reg) rs)
 return reg

-- Log an assignment
(<==) :: (Typeable a) => Seq a -> Seq a -> RTL ()
r <== e = do
 trans <- asks tid
 -- Get the register id annotation.
 let Just idx = getRegId r
 tell (Log (M.singleton idx [(trans,toDyn e)]) mempty)


-- Project out, from the input in the reader environment, a particular input.
newInput :: forall a clk . (Wire a, Typeable a) => a -> String -> RTL (CSeq clk a)
newInput _ name = do
 is <- asks inputs
 let ~(Just v) = M.lookup name is
 let ~(Seq s d) = fromDyn v (error "newInput" :: Seq a)
 return (input name (Seq s (D (Pad (PadVar 0 name)))))


rule nm g m = do
 parent <- asks tid
 let tid' = nm:parent
 tell (Log mempty (M.singleton tid' g))
 local (\i -> i {tid = tid'}) m


makeMux trans init assigns = out
 where enabled :: [(CSeq () Bool, Dynamic)]
       enabled = [(fromJust (M.lookup tid trans), d) | (tid,d) <- assigns]
       -- muxn :: (CSeq () Bool, Dynamic) -> a -> a
       muxn (en,t) f = mux2 en ((fromDyn t init), f)
       out = foldr muxn init enabled

makeTrans ts = foldl (flip f) M.empty sorted
 where sorted = sortBy lsort (M.toList ts)
       lsort (m,_) (n,_) = compare (length m) (length n)
       f ([], _) m = M.insert [] (liftS0 true) m
       f ([p], g) m = M.insert [p] g m
       f (p:ps, g) m = let Just pg = M.lookup ps m
                       in M.insert (p:ps) g m

-- makePorts :: M.Map Int Dynamic -> [(BaseTy, Driver E)]
makePorts = concatMap f . M.elems
 where tys = [let witness :: CSeq () Bool
                  witness = undefined
              in (typeOf witness, \d -> wireCapture (seqDriver (fromDyn d witness)))
             ,
              let witness :: CSeq () Int
                  witness = undefined
              in (typeOf witness, \d -> wireCapture (seqDriver (fromDyn d witness)))
             ,
              let witness :: CSeq () U4
                  witness = undefined
              in (typeOf witness, \d -> wireCapture (seqDriver (fromDyn d witness)))
             ]
       f d = let rep = dynTypeRep d
             in case lookup rep tys of
                  Just conv -> conv d
                  Nothing -> error $ "Couldn't find rep " ++ show rep


instance Ports (M.Map Int Dynamic) where
 ports _ m = makePorts m


instance Ports (M.Map TID (Seq Bool)) where
  ports _ m = concat [ports 0 (output ("guard_" ++ concat tid) guard) | (tid, guard) <- M.toList m]


t1 = do
 ctr <- newReg ((fromInteger 0) :: Comb Int)
 en <- newInput True "en"
 num <- newInput  ((fromInteger 0) :: Int) "num"

 rule "en" en $ ctr <== (ctr + 1)
 rule "en_not" (bitNot en) $ ctr <== (ctr + 1)

 rule "num"  (num .>. 4) $
         ctr <== 0
 return ()

{--
 This is Lava junk, and should go somewhere in Lava
--}
deriving instance Typeable2 CSeq
deriving instance Typeable1 Comb

getAnnotation name (Seq _ (D (Port v (E (Entity n outs ins attrs))))) = find p attrs >>= \(Ann _ v) -> fromDynamic v
 where p (Ann n _)= n == name
       p _ = False
addAnnotation attr (Seq s (D (Port v (E (Entity n outs ins attrs))))) =  Seq s (D (Port v (E (Entity n outs ins (attr:attrs)))))
addAnnotation _ _ = error "Can't add an annotation to a non-port entity"

globalEnv = input "global" $ mkEnv
             (Clock 1 (D $ Pad (PadVar 0 "clk")))
             (Seq shallowRst (D $ Pad (PadVar 0 "rst")))
             (Seq shallowEnable (D $ Pad (PadVar 0 "enable")))
 where (Seq shallowRst _) = toSeq $ [True] ++ repeat False
       (Seq shallowEnable _) = toSeq $ repeat True


deriving instance (Typeable1 Unsigned)
deriving instance Typeable1 X0_
deriving instance Typeable1 X1_
deriving instance Typeable X0

