{-# LANGUAGE ExistentialQuantification, GADTs #-}
import Prelude hiding (read)

import Data.IORef
import Language.KansasLava
import System.IO.Unsafe
import Data.Default
when = undefined

--instance Monad RTL where {}

--instance Show (Reg a) where {}
--instance Eq (Reg a) where {}


clocked :: RTL () -> RTL ()
clocked = undefined

initial :: RTL () -> RTL ()
initial = undefined



i1 :: Seq Int
i1 = undefined

runRTL :: RTL (Reg a) -> Seq a
runRTL = undefined


{-
foo :: Seq Int
foo = runRTL $ do
  r1 <- newReg 0 :: RTL (Reg Int)

  r1 := old r1 + (1 :: Seq Int)

  when (1 .==. (2 :: Seq Int)) $ do
    	r1 := i1 * 2

    	r1 := old r1
	r1 := new r1
  return r1
-}

register' ::  Env () -> Comb Int -> Seq Int -> Seq Int
register' env def inp = rtl $ do
	reg <- newReg env def
	reg $= inp
	return reg

counter2 :: Env () -> Seq Int
counter2 env = rtl $ do
	state <- newReg env true
	count <- newReg env 0
	
	IF (old state .==. high) $ do
		count $= old count + 1
		IF (new count .==. 5) $ do
			state $= low
	IF (old state .==. low) $ do
		count $= old count - 1
		IF (new count .==. 0) $ do
			state $= high
	return count

{-
	IF (inp .==. 1) $ do
		reg $= 2

	IF undefined $ do
		
	
-}


rtl :: RTL (Reg a) -> Seq a
rtl (RTL m) = unsafePerformIO $ do 
	r <- m []
	return (old r)

main = do
	let reg = counter2 -- register' :: Env () -> Comb Int -> Seq Int -> Seq Int
	cir <- reifyCircuit reg
	print cir
	cirO <- optimizeCircuit def cir
	print cirO
	writeDotCircuit "x.dot" cirO
	writeVhdlCircuit [] "X" "X.vhdl" cirO

old :: Reg a -> Seq a
old (Reg _ o _) = o
new :: Reg a -> Seq a
new (Reg _ _ n) = n

unRTL :: RTL a -> [Seq Bool] -> IO a
unRTL (RTL m) = m
unRTL (IF c m) = unRTL (IF c m >>= return) 

-- data RTL a = Return a | WHEN (Seq Bool) (RTL a) | forall b . (Reg b) := (Seq b)
data RTL a where
   RTL :: ([Seq Bool] -> IO a) -> RTL a
   IF  :: Seq Bool -> RTL () -> RTL ()

--	   | WHEN (Seq Bool) (RTL a)
--	   | forall b . (Reg b) := (Seq b)
--	   | Return a 
--	| IF 
	
{-	
	Bind a

-}

infixr 0 $=
($=) :: Reg a -> Seq a -> RTL ()
($=) (Reg var _ _) ss = RTL $ \ env -> do
	(Merged assigns) <- readIORef var
	writeIORef var $ Merged (assigns ++ [(env,ss)])
	return ()

instance Monad RTL where 
	return a = RTL $ \ _ -> return a
	(RTL m) >>= k = RTL $ \ e -> do
			r <- m e
			unRTL (k r) e
	(IF c m) >>= k = RTL $ \ e -> do
			() <- unRTL m (e ++ [c])
			unRTL (k ()) e
{-
	m >>= k = case m of
		    Return a -> k a
		    (v := e) -> 
		
-}

data Merged a = Merged [([Seq Bool],Seq a)] 

data Reg a = Reg (IORef (Merged a)) 
		 (Seq a)		-- old 
		 (Seq a)		-- new

newReg :: (Rep a) => Env () -> Comb a -> RTL (Reg a)
newReg env def = RTL $ \ _ -> do
	var <- newIORef (Merged [])
	~(v_old,v_new) <- unsafeInterleaveIO $ do
		print "tug o newReg"
		Merged assigns <- readIORef var
		let mux :: (Rep a) => ([Seq Bool],Seq a) -> Seq a -> Seq a
	 	    mux ([],a) d = a
 	    	    mux (bs,a) d = mux2 (foldr1 (.&&.) bs) (a,d)
		let v_old = register env def v_new
		    v_new = foldr mux v_old assigns
		return (v_old,v_new)
	-- TODO: make sure that the old and new are thunks or (evaluation)delays.
	-- otherwise, the assignment of the values could happen too soon
	return $ Reg var v_old v_new

-- instance Num (Reg a) where {}
