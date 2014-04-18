{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs #-}
-- | The RTL module provides a small DSL that's useful for control-oriented -- stateful -- computations.
module Language.KansasLava.RTL (
        RTL(..),    -- not abstract
        Reg,        -- abstract
        Cond(..),   -- not abstract
        runRTL,
        reg, var,
        newReg, newArr,
        match
        ) where

import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Probes
import Data.Sized.Matrix
import Control.Applicative
import Control.Monad.ST
import Data.STRef
import Data.List as L
import Control.Monad.ST.Unsafe (unsafeInterleaveST)

--import Debug.Trace

-------------------------------------------------------------------------------
-- | A register is used internally to represent a register or memory element.
data Reg s c a  = Reg (Signal c a) 		--  output of register
		      (Signal c a)		--  input to register
		      (STRef s [Signal c a -> Signal c a])
		      (STRef s (Maybe String))	--  name for debug message
		      Int
	      | forall ix . (Rep ix) =>
		  Arr (Signal c a)
		      (Signal c ix)
	    	      (STRef s [Signal c (Maybe (ix,a)) -> Signal c (Maybe (ix,a))])
		      Int
						-- the "assignments"

-- | reg is the value of a register, as set by the start of the cycle.
reg :: Reg s c a -> Signal c a
reg (Reg iseq _ _ _ _) = iseq
reg (Arr iseq _ _ _) = iseq

-- | var is the value of a register, as will be set in the next cycle,
-- so intra-cycle changes are observed. The is simular to a *variable*
-- in VHDL.
var :: Reg s c a -> Signal c a
var (Reg _ iseq _ _ _) = iseq
var (Arr _ _ _ _) = error "can not take the var of an array"

-------------------------------------------------------------------------------

-- | A predicate (boolean) circuit. If the argument is Nothing, treat it as true.
data Pred c = Pred (Maybe (Signal c Bool))

-- | A predicate that's always true.
truePred :: Pred c
truePred = Pred Nothing

-- | Conjunction of predicates.
andPred :: Pred c -> Signal c Bool -> Pred c
andPred (Pred Nothing) c    = Pred (Just c)
andPred (Pred (Just c1)) c2 = Pred (Just (c1 .&&. c2))

-- | If the first predicate is false, then return the first element of the
-- predicate. Otherwise, return the second element.
muxPred :: (Rep a) => Pred c -> (Signal c a, Signal c a) -> Signal c a
muxPred (Pred Nothing) (_,t) = t
muxPred (Pred (Just p)) (f,t) = mux p (f,t)

-------------------------------------------------------------------------------

-- | RTL Monad; s == the runST state; c is governing clock, and a is the result
data RTL s c a where
	RTL :: (Pred c -> STRef s Int -> ST s (a,[Int])) -> RTL s c a
	(:=) :: forall c b s . (Rep b) => Reg s c b -> Signal c b -> RTL s c ()
	CASE :: [Cond s c] -> RTL s c ()
	WHEN :: Signal c Bool -> RTL s c () -> RTL s c ()
	DEBUG :: forall c b s . (Rep b) => String -> Reg s c b -> RTL s c ()

-- everything except ($) bids tighter
infixr 0 :=

instance Functor (RTL s c) where
        fmap f m = RTL $ \ c u -> do
            (x, us) <- unRTL m c u
            return (f x, us)

instance Applicative (RTL s c) where
        pure x = RTL $ \ _ _ -> return (x, [])
        mf <*> mx = RTL $ \ c u -> do
            (f, us1) <- unRTL mf c u
            (x, us2) <- unRTL mx c u
            return (f x, us1 ++ us2)

instance Monad (RTL s c) where
	return = pure
	m >>= k = RTL $ \ c u -> do (r1,f1) <- unRTL m c u
			  	    (r2,f2) <- unRTL (k r1) c u
				    return (r2,f1 ++ f2)

-- | Run the RTL monad.
runRTL :: forall c a . (Clock c) => (forall s . RTL s c a) -> a
runRTL rtl = runST (do
	u <- newSTRef 0
	(r,_) <- unRTL rtl truePred u
	return r)

-- This is where our fixed (constant) names get handled.
-- | 'Execute' a RTL computation, building a circuit.
unRTL :: RTL s c a -> Pred c -> STRef s Int -> ST s (a,[Int])
unRTL (RTL m) = m
unRTL (Reg _ _ varSt _ uq := ss) = \ c _u -> do
	modifySTRef varSt ((:) (\ r -> muxPred c (r,ss)))
	return ((), [uq])
unRTL (Arr _ ix varSt uq := ss) = \ c _u -> do
	modifySTRef varSt ((:) (\ r -> muxPred c (r,enabledS (pack (ix,ss)))))
	return ((), [uq])

unRTL (CASE alts) = \ c u -> do
	-- fix
	let conds = [ p | IF p _ <- alts ]
	    -- assumes a resonable optimizer will remove this if needed
	    other_p = bitNot $ foldr (.||.) low conds
	res <- sequence
	   [ case alt of
		IF p m -> unRTL m (andPred c p) u
		OTHERWISE m -> unRTL m (andPred c other_p) u
	   | alt <- alts
	   ]
	let assignments = L.nub $ concat [ xs | (_,xs) <- res ]
--	() <- trace (show res) $ return ()
	return ((),assignments)
unRTL (DEBUG msg (Reg _ _ _ debugSt _)) = \ _c _u -> do
	writeSTRef debugSt (Just msg)
	return ((),[])
unRTL (DEBUG _msg _) = \ _c _u -> return ((),[])
unRTL (WHEN p m) = unRTL (CASE [IF p m])

-------------------------------------------------------------------------------

-- | A conditional statement.
data Cond s c
	= IF (Signal c Bool) (RTL s c ())
	| OTHERWISE (RTL s c ())

-------------------------------------------------------------------------------
-- data NewReg c a = NewReg (forall r . (IsReg r) => r c a)

-- Not quite sure why we need the NewReg indirection;
-- something to do with a limitation of ImpredicativeTypes.

-- |Declare a new register.
newReg :: forall a c s . (Clock c, Rep a) => a -> RTL s c (Reg s c a)
newReg def = RTL $ \ _ u -> do
	uq <- readSTRef u
	writeSTRef u (uq + 1)
	varSt <- newSTRef []
	debugSt <- newSTRef Nothing
	~(regRes,variable) <- unsafeInterleaveST $ do
		assigns <- readSTRef varSt
		debugs <- readSTRef debugSt
		let v_old = register def v_new
		    v_new = foldr (.) id (reverse assigns) v_old
	    	    v_old' = case debugs of
			       Nothing -> v_old
			       Just msg -> probeS msg v_old
		return (v_old',v_new)
	return (Reg regRes variable varSt debugSt uq,[])

-- | Declare an array. Arrays support partual updates.
newArr :: forall a c ix s . (Size ix, Clock c, Rep a, Num ix, Rep ix) => Witness ix -> RTL s c (Signal c ix -> Reg s c a)
newArr Witness = RTL $ \ _ u -> do
	uq <- readSTRef u
	writeSTRef u (uq + 1)
	varSt <- newSTRef []
	proj <- unsafeInterleaveST $ do
		assigns <- readSTRef varSt
		let ass = foldr (.) id (reverse assigns) (pureS Nothing)
		let look ix = writeMemory (ass :: Signal c (Maybe (ix,a)))
					`asyncRead` ix
		return look
	return (\ ix -> Arr (proj ix) ix varSt uq, [])

--assign :: Reg s c (Matrix ix a) -> Signal c ix -> Reg s c a
--assign (Reg seq var uq) = Arr

-------------------------------------------------------------------------------

-- | match checks for a enabled value, and if so, executes the given RTL in context,
--   by constructing the correct 'Cond'-itional.
match :: (Rep a) => Signal c (Enabled a) -> (Signal c a -> RTL s c ()) -> Cond s c
match inp fn = IF (isEnabled inp) (fn (enabledVal inp))
-- To consider: This is almost a bind operator?


-- debug :: (Rep a,Clock c) => String -> Reg s c a -> RTL
