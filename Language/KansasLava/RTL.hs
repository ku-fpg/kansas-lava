{-# LANGUAGE RankNTypes,GADTs, ExistentialQuantification, KindSignatures, ScopedTypeVariables	, TypeFamilies, TypeSynonymInstances
 #-}

module Language.KansasLava.RTL where

import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Data.Sized.Matrix
import Control.Monad.ST
import Data.STRef
import Data.List as L

import Debug.Trace

-------------------------------------------------------------------------------

data Reg s c a  = Reg (CSeq c a) 		-- output of register
		      (CSeq c a)		-- input to register
		      (STRef s [CSeq c a -> CSeq c a])
		      Int
	      | forall ix . (Rep ix) =>
		  Arr (CSeq c a)
		      (CSeq c ix)
	    	      (STRef s [CSeq c (Maybe (ix,a)) -> CSeq c (Maybe (ix,a))])
		      Int
						-- the "assignments"

reg :: Reg s c a -> CSeq c a
reg (Reg iseq _ _ _) = iseq
reg (Arr iseq _ _ _) = iseq

var :: Reg s c a -> CSeq c a
var (Reg iseq _ _ _) = iseq
var (Arr iseq _ _ _) = iseq

-------------------------------------------------------------------------------
data Pred c = Pred (Maybe (CSeq c Bool))

truePred :: Pred c
truePred = Pred Nothing

andPred :: Pred c -> CSeq c Bool -> Pred c
andPred (Pred Nothing) c    = Pred (Just c)
andPred (Pred (Just c1)) c2 = Pred (Just (c1 .&&. c2))

muxPred :: (Rep a) => Pred c -> (CSeq c a, CSeq c a) -> CSeq c a
muxPred (Pred Nothing) (t,_) = t
muxPred (Pred (Just p)) (t,f) = mux2 p (t,f)

-------------------------------------------------------------------------------

data RTL s c a where
	RTL :: (Pred c -> STRef s Int -> ST s (a,[Int])) -> RTL s c a
	(:=) :: forall c b s . (Rep b) => Reg s c b -> CSeq c b -> RTL s c ()
	CASE :: [Cond s c] -> RTL s c ()
	WHEN :: CSeq c Bool -> RTL s c () -> RTL s c ()

-- everything except ($) bids tighter
infixr 0 :=

instance Monad (RTL s c) where
	return a = RTL $ \ _ _ -> return (a,[])
	m >>= k = RTL $ \ c u -> do (r1,f1) <- unRTL m c u
			  	    (r2,f2) <- unRTL (k r1) c u
				    return (r2,f1 ++ f2)

runRTL :: forall c a . (Clock c) => (forall s . RTL s c a) -> a
runRTL rtl = runST (do
	u <- newSTRef 0
	(r,_) <- unRTL rtl (Pred Nothing) u
	return r)

-- This is where our fixed (constant) names get handled.
unRTL :: RTL s c a -> Pred c -> STRef s Int -> ST s (a,[Int])
unRTL (RTL m) = m
unRTL ((Reg _ _ varSt uq) := ss) = \ c _u -> do
	modifySTRef varSt ((:) (\ r -> muxPred c (ss,r)))
	return ((), [uq])
unRTL ((Arr _ ix varSt uq) := ss) = \ c _u -> do
	modifySTRef varSt ((:) (\ r -> muxPred c (enabledS (pack (ix,ss)),r)))
	return ((), [uq])

unRTL (CASE alts) = \ c u -> do
	-- fix
	let conds = [ p | IF p _ <- alts ]
	    -- assumes a resonable optimizer will remove this if needed
	    other_p = bitNot $ foldr (.||.) low conds
	res <- sequence
	   [ case alt of
		IF p m -> do
		 unRTL m (andPred c p) u
		OTHERWISE m -> do
		 unRTL m (andPred c other_p) u
	   | alt <- alts
	   ]
	let assignments = L.nub $ concat [ xs | (_,xs) <- res ]
	() <- trace (show res) $ return ()
	return ((),assignments)
unRTL (WHEN p m) = unRTL (CASE [IF p m])

-------------------------------------------------------------------------------

data Cond s c
	= IF (CSeq c Bool) (RTL s c ())
	| OTHERWISE (RTL s c ())

-------------------------------------------------------------------------------
-- data NewReg c a = NewReg (forall r . (IsReg r) => r c a)

-- Not quite sure why we need the NewReg indirection;
-- something to do with a limitation of ImpredicativeTypes.

newReg :: forall a c s . (Clock c, Rep a) => a -> RTL s c (Reg s c a)
newReg def = RTL $ \ _ u -> do
	uq <- readSTRef u
	writeSTRef u (uq + 1)
	varSt <- newSTRef []
	~(regRes,variable) <- unsafeInterleaveST $ do
		assigns <- readSTRef varSt
		let v_old = register def v_new
		    v_new = foldr (.) id (reverse assigns) v_old
--		    v_new' = debugWith debugs v_new'
		return $ (v_old,v_new)
	return (Reg regRes variable varSt uq,[])

-- Arrays support partual updates.
newArr :: forall a c ix s . (Size ix, Clock c, Rep a, Num ix, Rep ix) => Witness ix -> RTL s c (CSeq c ix -> Reg s c a)
newArr Witness = RTL $ \ _ u -> do
	uq <- readSTRef u
	writeSTRef u (uq + 1)
	varSt <- newSTRef []
	proj <- unsafeInterleaveST $ do
		assigns <- readSTRef varSt
		let ass = foldr (.) id (reverse assigns) (pureS Nothing)
		let look ix = writeMemory (ass :: CSeq c (Maybe (ix,a)))
					`readMemory` ix
		return $ look
	return (\ ix -> Arr (proj ix) ix varSt uq, [])

--assign :: Reg s c (Matrix ix a) -> CSeq c ix -> Reg s c a
--assign (Reg seq var uq) = Arr

-------------------------------------------------------------------------------

-- | match checks for a enabled value, and if so, executes the given RTL in context,
--   by constructing the correct 'Cond'-itional.
match :: (Rep a) => CSeq c (Enabled a) -> (CSeq c a -> RTL s c ()) -> Cond s c
match inp fn = IF (isEnabled inp) (fn (enabledVal inp))
-- To consider: This is almost a bind operator?
