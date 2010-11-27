{-# LANGUAGE RankNTypes,GADTs, ExistentialQuantification, KindSignatures, ScopedTypeVariables #-}

import Language.KansasLava
import Control.Concurrent.MVar
import System.IO.Unsafe
import Data.Default

data REG c a = REG (forall v . (IsReg v) => v c a)

data ARR c a = ARR (forall v . (IsReg v) => CSeq c a -> v c a)

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
infixr 0 :=

-- Choosing a deep model just because.

data RTL c a where
	RTL :: (Maybe (CSeq c Bool) -> IO a) -> RTL c a
	(:=) :: forall c b . Reg c b -> CSeq c b -> RTL c ()

runRTL :: forall c a . (Clock c) => RTL c a -> IO a	
runRTL rtl = unRTL rtl Nothing

-- This is where our fixed (constant) names get handled.	
unRTL :: RTL c a -> Maybe (CSeq c Bool) -> IO a
unRTL (RTL m)  = m
unRTL ((Reg _ var) := ss) = \ c -> do
	vs <- takeMVar var
	putMVar var ((c,ss) : vs)
	return ()


infixr 0 ?
(?), rule :: CSeq c Bool -> RTL c a -> RTL c a
cond ? code = RTL $ \ c -> case c of
	Nothing -> unRTL code (Just cond)
	Just cond' -> unRTL code (Just (cond .&&. cond'))
rule = (?)

{-
data RTL c a where

	Return 	:: forall a   c . a -> RTL c a
	Bind 	:: forall a b c . RTL c b -> (b -> RTL c a) -> RTL c a
	NewReg_	:: forall a   c . (Rep a) => RTL c (REG c a)
	IF 	:: forall c     . RTL c () -> RTL c () -> RTL c ()
-}

instance Monad (RTL c) where
	return a = RTL $ \ _ -> return a
	m >>= k = RTL $ \ c -> do r <- unRTL m c
				  unRTL (k r) c

foo :: RTL () (Seq Int)
foo = do
	REG r  <- newReg
	REG r2 <- newReg
	REG p  <- newReg :: RTL () (REG () Bool)
	ARR c <- newArr 99
	
	c 1 := (2 :: Seq Int)

	r := c r
	( p ) ? (r2 := r + (1 :: Seq Int))
	
	
	{-
	WHEN (r .<. 100) $ do
		r := r + (1 :: Seq Int) 
-}
{-	


	p .==. 0 ? do 
	    r := r = 1
-}	

--	r .>. 99 ==> r := r + (1 :: Seq Int) 	

{-
	SWITCH 	[ (c .==. 0	, ...)
	       	, (true		, ...)
		]
		
	CASE	[ (0,	
-}
--		r := r + (1 :: Seq Int)	

--	SWITCH c 
--	  (0
--	  (
--	r := r + (2 :: Seq Int)
	return r2
{-
	SWITCH x 
	  (abc, ...)
	  (
-}

newReg :: forall a c . (Clock c, Rep a) => RTL c (REG c a)
newReg = RTL $ \ _ -> do
	var <- newMVar []
	proj <- unsafeInterleaveIO $ do
		assigns <- readMVar var

		let mux :: forall a . (Rep a) => (Maybe (CSeq c Bool),CSeq c a) -> CSeq c a -> CSeq c a
		    mux (Nothing,a) d = a
		    mux (Just b,a) d  = mux2 b (a,d)
		let v_old = register undefinedComb v_new
		    v_new = foldr mux v_old assigns
		return $ v_old
	return (REG (fromReg (Reg proj var)))


newArr :: forall a c . (Clock c, Rep a) => Integer -> RTL c (ARR c a)
newArr n = RTL $ \ _ -> do
	var <- newMVar []
	proj <- unsafeInterleaveIO $ do
		assigns <- readMVar var

		let mux :: forall a . (Rep a) => (Maybe (CSeq c Bool),CSeq c a) -> CSeq c a -> CSeq c a
		    mux (Nothing,a) d = a
		    mux (Just b,a) d  = mux2 b (a,d)
		let v_old = register undefinedComb v_new
		    v_new = foldr mux v_old assigns
		return $ v_old
	return (ARR (\ i -> (fromReg (Reg proj var))))

--runRTLIO :: forall c a . (Clock c) => RTL c a -> IO a	
--runRTLIO = 
{-
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
	return (REG (fromReg (Reg proj var)))
interp ((Reg _ var) := ss)  c = do
	vs <- takeMVar var
	putMVar var ((c,ss) : vs)
	return ()
interp (WHEN cond code) Nothing      = interp code (Just cond)
interp (WHEN cond code) (Just cond') = interp code (Just (cond .&&. cond'))

-}
	
main = do
	x <- runRTL foo 
	c <- reifyCircuit x
	c' <- optimizeCircuit def c
	writeDotCircuit "x.dot" c'
	print c'
