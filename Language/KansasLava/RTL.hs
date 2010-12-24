{-# LANGUAGE RankNTypes,GADTs, ExistentialQuantification, KindSignatures, ScopedTypeVariables	, TypeFamilies, TypeSynonymInstances
 #-}

module Language.KansasLava.RTL where

import Language.KansasLava.Seq
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Protocols
import Language.KansasLava.Signal
import Control.Concurrent.MVar
import System.IO.Unsafe
import Data.Sized.Matrix
import Data.Sized.Unsigned (U7)
import Data.Default
import Control.Monad.ST
import Data.STRef
-------------------------------------------------------------------------------

data Reg s c a  = Reg (CSeq c a) 					-- the "final" answer
		      (STRef s [CSeq c a -> CSeq c a])
		      Int
	      | forall ix . (Rep ix) =>
		  Arr (CSeq c a)
		      (CSeq c ix)
	    	      (STRef s [CSeq c (Maybe (ix,a)) -> CSeq c (Maybe (ix,a))])
		      Int
						-- the "assignments"
--	(Maybe (CSeq c Bool),CSeq c ix, CSeq c a)])	

-- type R a b c = ()
{-
class IsReg (v :: * -> * -> * -> *) where
--	type R v :: * -> * -> * -> * 
	fromReg :: Reg s c a -> v s c a

{-
instance IsReg (Reg s) where
	type R (Reg s) = Reg
	fromReg x = x
-}
type CCSeq s c a = CSeq c a

-- Project out the final answer,
instance IsReg (CCSeq) where
--	type R CSeq = CSeq
	fromReg (Reg seq _) = seq
--	fromReg (Arr seq _ _) = seq
-}


reg :: Reg s c a -> CSeq c a
reg (Reg seq _ _) = seq
reg (Arr seq _ _ _) = seq

-------------------------------------------------------------------------------

data Pred c = Pred (Maybe (CSeq c Bool))

truePred = Pred Nothing

andPred :: Pred c -> CSeq c Bool -> Pred c
andPred (Pred Nothing) c    = Pred (Just c)
andPred (Pred (Just c1)) c2 = Pred (Just (c1 .&&. c2))

muxPred :: (Rep a) => Pred c -> (CSeq c a, CSeq c a) -> CSeq c a
muxPred (Pred Nothing) (t,f) = t
muxPred (Pred (Just p)) (t,f) = mux2 p (t,f)

-------------------------------------------------------------------------------


data RTL s c a where
	RTL :: (Pred c -> STRef s Int -> ST s (a,[Int])) -> RTL s c a
	(:=) :: forall c b s . (Rep b) => Reg s c b -> CSeq c b -> RTL s c ()
	CASE :: [Cond s c] -> RTL s c ()

-- everything except ($) bids tighter
infixr 0 :=

instance Monad (RTL s c) where
	return a = RTL $ \ _ u -> return (a,[])
	m >>= k = RTL $ \ c u -> do (r1,f1) <- unRTL m c u
			  	    (r2,f2) <- unRTL (k r1) c u
				    return (r2,f1 ++ f2)

runRTL :: forall c a s . (Clock c) => (forall s . RTL s c a) -> a
runRTL rtl = runST (do
	u <- newSTRef 0
	(r,_) <- unRTL rtl (Pred Nothing) u
	return r)

-- This is where our fixed (constant) names get handled.	
unRTL :: RTL s c a -> Pred c -> STRef s Int -> ST s (a,[Int])
unRTL (RTL m) = m
unRTL ((Reg _ var uq) := ss) = \ c _u -> do
	modifySTRef var ((:) (\ r -> muxPred c (ss,r)))
	return ((), [uq])
unRTL ((Arr _ ix var uq) := ss) = \ c _u -> do
	modifySTRef var ((:) (\ r -> muxPred c (enabledS (pack (ix,ss)),r)))
	return ((), [uq])

unRTL (CASE alts) = \ c u -> do
	sequence_ 
	   [ case alt of
		IF p m -> do
		 unRTL m (andPred c p) u 
		OTHERWISE m -> do
		 unRTL m c u
	   | alt <- alts
	   ]
	return ((),[])

-------------------------------------------------------------------------------

data Cond s c
	= IF (CSeq c Bool) (RTL s c ())
	| OTHERWISE (RTL s c ())

-------------------------------------------------------------------------------

-- data NewReg c a = NewReg (forall r . (IsReg r) => r c a)

-- Not quite sure why we need the NewReg indirection;
-- something to do with a limitation of ImpredicativeTypes.

newReg :: forall a c s . (Clock c, Rep a) => Comb a -> RTL s c (Reg s c a)
newReg def = RTL $ \ _ u -> do 
	uq <- readSTRef u
	writeSTRef u (uq + 1)
	var <- newSTRef []
	proj <- unsafeInterleaveST $ do
		assigns <- readSTRef var
		let v_old = register def v_new
		    v_new = foldr (.) id (reverse assigns) v_old
		return $ v_old
	return (Reg proj var uq,[])


-- Arrays support partual updates.

newArr :: forall a c ix s . (Size ix, Clock c, Rep a, Num ix, Rep ix) => Witness ix -> RTL s c (CSeq c ix -> Reg s c a)
newArr Witness = RTL $ \ _ u -> do 
	uq <- readSTRef u
	writeSTRef u (uq + 1)
	var <- newSTRef []
	proj <- unsafeInterleaveST $ do
		assigns <- readSTRef var
{-
		let memMux :: forall a . (Rep a) => (Maybe (CSeq c Bool), CSeq c ix, CSeq c a) -> CSeq c (Maybe (ix,a)) -> CSeq c (Maybe (ix,a))
		    memMux (Nothing,ix,v) d  = enabledS (pack (ix,v))
		    memMux (Just p,ix,v) d  = mux2 p (enabledS (pack (ix,v)),d)
		
--			let mux :: forall a . (Rep a) => (Maybe (CSeq c Bool),CSeq c a) -> CSeq c a -> CSeq c a
--		    mux (Nothing,a) d = a	-- Nothing is used to represent always true
--		    mux (Just b,a) d  = mux2 b (a,d)
-}
		let ass = foldr (.) id (reverse assigns) (pureS Nothing)
		let look ix = pipeToMemory (ass :: CSeq c (Maybe (ix,a))) ix
		return $ look
	return (\ ix -> Arr (proj ix) ix var uq, [])

--assign :: Reg s c (Matrix ix a) -> CSeq c ix -> Reg s c a
--assign (Reg seq var uq) = Arr 

{-
	var <- newMVar []
	proj <- unsafeInterleaveIO $ do
		assigns <- readMVar var

		let memMux :: forall a . (Rep a) => (Maybe (CSeq c Bool), CSeq c ix, CSeq c a) -> CSeq c (Maybe (ix,a)) -> CSeq c (Maybe (ix,a))
		    memMux (Nothing,ix,v) d  = enabledS (pack (ix,v))
		    memMux (Just p,ix,v) d  = mux2 p (enabledS (pack (ix,v)),d)

		let ass = foldr memMux (pureS Nothing) assigns
		let look ix = pipeToMemory (ass :: CSeq c (Maybe (ix,a))) ix
		return $ look

	return (ARR (\ ix -> fromReg (Arr (proj ix) ix var)), [])
-}

{-

newReg :: forall s a c . (Clock c, Rep a) => Comb a -> RTL s c (forall r . (IsReg r) => r c a)
newReg def = do
	NewReg r <- newReg' def
	return r

-------------------------------------------------------------------------------
-- data NewArr c ix a = NewArr (forall v . (IsReg v) => CSeq c ix -> v c a)
-}

-------------------------------------------------------------------------------
