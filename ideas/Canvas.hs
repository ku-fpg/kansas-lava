{-# LANGUAGE RankNTypes,GADTs, ExistentialQuantification, KindSignatures, ScopedTypeVariables, NoMonomorphismRestriction, TypeFamilies #-}

import Language.KansasLava
import Control.Concurrent.MVar
import System.IO.Unsafe
import Data.Sized.Matrix
import Data.Sized.Unsigned (U7)
import Data.Default
import Data.Sized.Matrix as M

{-
data REG c a = REG (forall v . (IsReg v) => v c a)

data ARR c ix a = ARR (forall v . (IsReg v) => CSeq c ix -> v c a)

class IsReg (v :: * -> * -> *) where
	fromReg :: Reg c a -> v c a

data Reg c a  = Reg (CSeq c a) 					-- the "final" answer
		    (MVar [(Maybe (CSeq c Bool),CSeq c a)])	-- the "assignments"
	      | forall ix . Arr (CSeq c a)
				(CSeq c ix)
		    		(MVar [(Maybe (CSeq c Bool),CSeq c ix, CSeq c a)])	-- the "assignments"

instance IsReg Reg where
	fromReg = id

-- Project out the final answer,
instance IsReg CSeq where
	fromReg (Reg seq _) = seq
	fromReg (Arr seq _ _) = seq

-- everything except ($)
infixr 0 :=


-- returns a list of conditions that *have* potentially fired
-- Just [] ==> nothing fired
-- Nothing ==> everything fired (TODO)
data RTL c a where
	RTL :: (Maybe (CSeq c Bool) -> IO (a,[CSeq c Bool])) -> RTL c a
	(:=) :: forall c b . Reg c b -> CSeq c b -> RTL c ()
	CASE :: [Cond c] -> RTL c ()

runRTL :: forall c a . (Clock c) => RTL c a -> IO a
runRTL rtl = do
	(r,_) <- unRTL rtl Nothing
	return r

-- This is where our fixed (constant) names get handled.	
unRTL :: RTL c a -> Maybe (CSeq c Bool) -> IO (a,[CSeq c Bool])
unRTL (RTL m)  = m
unRTL ((Reg _ var) := ss) = \ c -> do
	vs <- takeMVar var
	putMVar var ((c,ss) : vs)
	return ((), [])
unRTL ((Arr _ ix var) := ss) = \ c -> do
	vs <- takeMVar var
	putMVar var ((c,ix,ss) : vs)
	return ((), [])


infixr 0 ?
(?), rule :: CSeq c Bool -> RTL c a -> RTL c a
cond ? code = RTL $ \ c -> case c of
	Nothing -> do (r,fs) <- unRTL code (Just cond)
		      return (r,cond : fs)
	Just cond' -> do (r,fs) <- unRTL code (Just (cond .&&. cond'))
		         return (error "To THINK about")
rule = (?)

{-
data RTL c a where

	Return 	:: forall a   c . a -> RTL c a
	Bind 	:: forall a b c . RTL c b -> (b -> RTL c a) -> RTL c a
	NewReg_	:: forall a   c . (Rep a) => RTL c (REG c a)
	IF 	:: forall c     . RTL c () -> RTL c () -> RTL c ()
-}

instance Monad (RTL c) where
	return a = RTL $ \ _ -> return (a,[])
	m >>= k = RTL $ \ c -> do (r1,f1) <- unRTL m c
				  (r2,f2) <- unRTL (k r1) c
				  return (r2,f1 ++ f2)

foo :: RTL () (Seq Int)
foo = do
	REG r  		<- newReg (0 :: Comb Int)
--	REG r2 		<- newReg 0
	REG p  		<- newReg false
--	ARR c 		<- newArr 99
	REG r_out 	<- newReg 0
	
	(r_out .<. 10) ? do
		r_out := r_out + 1
	  <|> do
		r := 0
{-		
	(r_out .==. 10) ? do		
		r_out := 0
	    ||| do (
		r_

	match
	  [ (r_out .<. 10) ? do ....
	  , (r_our
		
--	c 1 := (2 :: Seq Int)

	r := c r

	c 2 := 9
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
-}
--	SWITCH c 
--	  (0
--	  (
--	r := r + (2 :: Seq Int)
	return r_out
{-
	SWITCH x 
	  (abc, ...)
	  (
-}

infixr 2 <|>

-- This operator
(<|>) :: RTL c () -> RTL c () -> RTL c ()
(<|>) rtl1 rtl2 = RTL $ \ c -> do
	((),fs0) <- unRTL rtl1 c
	((),fs1) <- unRTL rtl2 (c `excluding` fs0)
	return ((),[high])	
   where
	excluding = undefined

{-
	( p ) ? A <|> B
	==>
	( p ) ? A
	( ~p ) ? B
	
	( p1 ) ? A <|> (p2) ? B
	=?
	( p1 ) ? A
	( ~p1 & p2) ? B
	


	( p1 ) ? A <|> (p2) ? B <|> (p3) ? C
	==>
	( p1 ) ? A
	( ~p1 & p2 ) ? B
	( not (~p1 & p2) & p3 ) ? C

	( p1 ) ? (A <|> (p2) ? (B <|> (p3) ? C))
	((p1) ? A) <|> (((p2) ? B) <|> ((p3) ? C))

	


 -}

	


newReg :: forall a c . (Clock c, Rep a) => Comb a -> RTL c (REG c a)
newReg def = RTL $ \ _ -> do
	var <- newMVar []
	proj <- unsafeInterleaveIO $ do
		assigns <- readMVar var

		let mux :: forall a . (Rep a) => (Maybe (CSeq c Bool),CSeq c a) -> CSeq c a -> CSeq c a
		    mux (Nothing,a) d = a
		    mux (Just b,a) d  = mux2 b (a,d)
		let v_old = register def v_new
		    v_new = foldr mux v_old assigns
		return $ v_old
	return (REG (fromReg (Reg proj var)), [])


newArr :: forall a c ix . (Clock c, Rep ix, Rep a) => Integer -> RTL c (ARR c ix a)
newArr n = RTL $ \ _ -> do
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
	x <- runRTL (foo)

	c <- reifyCircuit x
	c' <- optimizeCircuit def c
	writeDotCircuit "x.dot" c'
	print c'
	
xx = updateMatrix :: Seq X4 -> Seq U7 -> Seq (Matrix X4 U7) -> Seq (Matrix X4 U7)
yy = (.!.) :: Seq (Matrix X4 U7) -> Seq X4 -> Seq U7


cond :: [ Cond c ]
cond = undefined

data Cond c
	= IF (CSeq c Bool) (RTL c ())
	| OTHERWISE (RTL c ())

infix 0 ==>
	
(==>) :: CSeq c Bool -> RTL c () -> Cond c
(==>) = IF

example t x = do
	CASE [ IF (x .==. 10) $ do
			t := 1
	     , IF (x .>. 10) $ do
			t := 1
	     , (x .>. 10) ==> do
			t := 10
	     , OTHERWISE $ do
		t := 1
	     ]


-}



example1 :: (Clock c) => CSeq c Int -> CSeq c Int
example1 r' = runRTL $ do
	r <- newReg (0 :: Comb Int)
	r := r'
	return $ reg r

example2 :: forall c sig . (Clock c, sig ~ CSeq c) => sig Int
example2 = runRTL $ do
	r  <- newReg (0 :: Comb Int)
	r := reg r + 1
	return $ reg r

example3 :: forall c sig . (Clock c, sig ~ CSeq c) => sig Int
example3 = runRTL $ do
	r  <- newReg (0 :: Comb Int)
	CASE [ IF (reg r .<. 10) $ do
		r := reg r + 1
	     , OTHERWISE $ do
		r := 0
	     ]
	return $ reg r

example4 :: forall c sig . (Clock c, sig ~ CSeq c) => sig Int
example4 = runRTL $ do
	r  <- newReg (0 :: Comb Int)
	CASE [ IF (reg r .==. 0) $ r := 10
	     , IF (reg r .==. 1) $ r := 20
	     , OTHERWISE $ do
		r := 999999
	     ]
	return $ reg r
	
{-
example3 :: forall c sig . (Clock c, sig ~ CSeq c) => sig Int
example3 = runRTL $ do
	arr  <- newArr 0 :: RTL s c (CSeq c X10 -> Reg s c Int)
	arr 3 := reg (arr 2)
	return $ reg $ arr 3
-}
{-	
	sequence_ 
		[ arr n := if i == (9 :: X10) then 99 else reg (arr (n + 1))
		| i <- [0..9]
		, let n = fromIntegral i
		]
	return $ M.forAll $ \i -> reg (arr (fromIntegral i))
-}

main = do
	c1 <- reifyCircuit (example1 :: CSeq () Int -> CSeq () Int) -- TODO: figure out combinator here
	writeDotCircuit "c1.dot" c1
	
	c2 <- reifyCircuit (example2 :: CSeq () Int)	-- TODO: figure out combinator here
	writeDotCircuit "c2.dot" c2

	c3 <- reifyCircuit (example3 :: (CSeq () Int))	-- TODO: figure out combinator here
	writeDotCircuit "c3.dot" c3
	c3' <- optimizeCircuit def c3
	writeDotCircuit "c3o.dot" c3'
	return ()

	c4 <- reifyCircuit (example4 :: (CSeq () Int))	-- TODO: figure out combinator here
	writeDotCircuit "c4.dot" c4
	c4' <- optimizeCircuit def c4
	writeDotCircuit "c4o.dot" c4'
	return ()	

{-
main = do
	x <- runRTL (foo)

	c <- reifyCircuit x

	c' <- optimizeCircuit def c
	writeDotCircuit "x.dot" c'
	print c'
-}