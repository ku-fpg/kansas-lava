{-# LANGUAGE RankNTypes,GADTs, ExistentialQuantification, KindSignatures, ScopedTypeVariables #-}

import Language.KansasLava
import Control.Concurrent.MVar
import System.IO.Unsafe
import Data.Default

newReg :: (Rep a) => RTL c (NewReg c a)
newReg = NewReg_

data NewReg c a = NewReg (forall v . (IsReg v) => v c a)

class IsReg (v :: * -> * -> *) where
	fromReg :: Reg c a -> v c a

data Reg c a  = Reg (CSeq c a) 					-- the "final" answer
		    (MVar [(Maybe (CSeq c Bool),CSeq c a)])	-- the "assignments"

instance IsReg Reg where
	fromReg = id

-- Project out the final answer,
instance IsReg CSeq where
	fromReg (Reg seq _) = seq

-- everything except ($)
infixl 1 :=

-- Choosing a deep model just because.
data RTL c a where
	(:=) 	:: forall b   c . Reg c b -> CSeq c b -> RTL c ()
	Return 	:: forall a   c . a -> RTL c a
	Bind 	:: forall a b c . RTL c b -> (b -> RTL c a) -> RTL c a
	NewReg_	:: forall a   c . (Rep a) => RTL c (NewReg c a)
	IF 	:: forall c     . RTL c () -> RTL c () -> RTL c ()
	WHEN	:: forall c	. CSeq c Bool -> RTL c () -> RTL c ()

instance Monad (RTL c) where
	return = Return
	(>>=)  = Bind

foo :: RTL () (Seq Int)
foo = do
	NewReg r <- newReg
	WHEN (r .<. 100) $ do
		r := r + (1 :: Seq Int) 
	
--	r .>. 99 ==> r := r + (1 :: Seq Int) 	

{-
	SWITCH 	[ (c .==. 0	, ...)
	       	, (true		, ...)
		]
		
	CASE	[ (0,	
-}
		r := r + (1 :: Seq Int)	

--	SWITCH c 
--	  (0
--	  (
--	r := r + (2 :: Seq Int)
	return r
{-
	SWITCH x 
	  (abc, ...)
	  (
-}


interp :: forall c a . (Clock c) => RTL c a -> Maybe (CSeq c Bool) -> IO a	
interp (Bind m k)  c = do
	r <- interp m c
	interp (k r) c
interp (Return a)       c = return a

interp (NewReg_)   c = do
	var <- newMVar []
	proj <- unsafeInterleaveIO $ do
		assigns <- readMVar var

		let mux :: forall a . (Rep a) => (Maybe (CSeq c Bool),CSeq c a) -> CSeq c a -> CSeq c a
		    mux (Nothing,a) d = a
		    mux (Just b,a) d  = mux2 b (a,d)
		let v_old = register undefinedComb v_new
		    v_new = foldr mux v_old assigns
		return $ v_old
	return (NewReg (fromReg (Reg proj var)))
interp ((Reg _ var) := ss)  c = do
	vs <- takeMVar var
	putMVar var ((c,ss) : vs)
	return ()
interp (WHEN cond code) Nothing      = interp code (Just cond)
interp (WHEN cond code) (Just cond') = interp code (Just (cond .&&. cond'))
	
main = do
	x <- interp foo Nothing
	c <- reifyCircuit x
	c' <- optimizeCircuit def c
	writeDotCircuit "x.dot" c'
	print c'
	
