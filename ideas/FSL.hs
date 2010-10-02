{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module FSM where
-- Use %ghci FSL.hs -i..
-- To load

import Language.KansasLava
import Language.KansasLava.Stream
import Data.Sized.Unsigned
import Debug.Trace
import Data.Maybe as Maybe
import System.IO.Unsafe
import Control.Concurrent.MVar

---------------------------------------------------------------------------
-- Key Types

type Src  a = Seq IsRead -> Seq (Enabled a)		-- pull
type Sink a = Seq IsFull -> Seq (Enabled a)		-- push


-- The -ve's for Src and Sink
data SRC a c = SRC (Seq (Enabled a) -> (Seq IsRead,c))
data SINK a c = SINK (Seq (Enabled a) -> (Seq IsFull,c))




---------------------------------------------------------------------------
-- IsRead

newtype IsRead = IsRead Bool
	deriving (Eq, Ord, Show)

instance Rep IsRead where
	type X IsRead 	= WireVal Bool
	optX (Just (IsRead b)) = return b
	optX Nothing	= fail "Rep IsRead"
	unX (WireVal v)  = return (IsRead v)
	unX (WireUnknown) = fail "Rep IsRead"
	wireType _	= B		-- a bit internally
	toRep w v	= RepValue [v]
	fromRep w (RepValue [v]) = v
	fromRep w rep	 	 = error ("size error for Bool" ++ show rep)



mkIsRead :: Seq Bool -> Seq IsRead
mkIsRead = liftS1 $ \ (Comb a (D ea)) 
	              -> (Comb (optX $ fmap IsRead $ unX a) (D ea :: D IsRead))

---------------------------------------------------------------------------
-- IsFull

newtype IsFull = IsFull Bool
	deriving (Eq, Ord, Show)
	
mkIsFull :: Seq Bool -> Seq IsFull
mkIsFull = liftS1 $ \ (Comb a (D ea)) 
	              -> (Comb (optX $ fmap IsFull $ unX a) (D ea :: D IsFull))

instance Rep IsFull where
	type X IsFull 	= WireVal Bool
	optX (Just (IsFull b)) = return b
	optX Nothing	= fail "Rep IsFull"
	unX (WireVal v)  = return (IsFull v)
	unX (WireUnknown) = fail "Rep IsFull"
	wireType _	= B		-- a bit internally
	toRep w v	= RepValue [v]
	fromRep w (RepValue [v]) = v
	fromRep w rep	 	 = error ("size error for Bool" ++ show rep)

---------------------------------------------------------------------------
-- Shallow Generators

-- Implements the FSL "Read/RHS" protocol, at max speed
toSrc :: (Rep a) => [a] -> Src a
toSrc = toVariableSrc (repeat 0)

toVariableSrc :: (Rep a) => [Int] -> [a] -> Src a
toVariableSrc stutter xs isRead = toSeq (fn stutter xs (fromSeq isRead))
	where
	   -- We rely on the semantics of pattern matching to not match (x:xs)
	   -- if (0:ps) does not match.
	   fn (0:ps) (x:xs) c 
		    = [Just x]
		    ++ case c of -- read c after issuing Just x
			(Nothing:rs)             -> error "toVariableSrc: bad protocol state (1)"
			(Just (IsRead True):rs)  -> fn ps xs rs	    -- has been read
			(Just (IsRead False):rs) -> fn (0:ps) (x:xs) rs -- not read yet
	   fn (p:ps) xs c
		    = [Nothing]
		    ++ case c of
			(Nothing:rs)             -> error "toVariableSrc: bad protocol state (2)"
			(Just (IsRead True):rs)  -> error "toVariableSrc: bad protocol state (3)"
			(Just (IsRead False):rs) -> fn (pred p:ps) xs rs -- nothing read
	   fn ps [] c = [Nothing]
		    ++ case c of
			(Nothing:rs)             -> error "toVariableSrc: bad protocol state (4)"
			(Just (IsRead True):rs)  -> error "toVariableSrc: bad protocol state (5)"
			(Just (IsRead False):rs) -> fn ps [] rs -- nothing read, ever

toVariableSink :: (Rep a) => [Int] -> [a] -> Sink a
toVariableSink stutter xs isFull = toSeq (fn stutter xs (fromSeq isFull))
	where
	   -- We rely on the semantics of pattern matching to not match (x:xs)
	   -- if (0:ps) does not match.
	   fn (0:ps) (x:xs) c 
		    = case c of -- read c after issuing Just x
			(Nothing:rs)             -> error "toVariableSink: bad protocol state (1)"
			(Just (IsFull False):rs) -> Just x : fn ps xs rs     -- has been written
			(Just (IsFull True):rs)  -> Nothing : fn (0:ps) (x:xs) rs -- not written yet
	   fn (p:ps) xs c
		    = case c of
			(Nothing:rs)         -> error "toVariableSink: bad protocol state (2)"
			(Just (IsFull _):rs) -> Nothing : fn (pred p:ps) xs rs -- nothing read
	   fn ps [] c
		    =  case c of
			(Nothing:rs)         -> error "toVariableSink: bad protocol state (3)"
			(Just (IsFull _):rs) -> Nothing : fn ps [] rs -- nothing read

---------------------------------------------------------------------------
-- Shallow Consumers

fromSrc :: (Show a, Rep a) => Src a -> [a]
fromSrc = fromVariableSrc (repeat 0)

fromVariableSrc :: forall a . (Show a, Rep a) => [Int] -> Src a -> [a]
fromVariableSrc stutter src = Maybe.catMaybes internal
   where
	val :: Seq (Enabled a)
	val = src read

	read :: Seq IsRead
	read = toSeq [ IsRead (Maybe.isJust v)
		     | v <- internal 
		     ]

	internal :: [Maybe a]
	internal = fn stutter (fromSeq val)
	
	fn :: [Int] -> [Maybe (Enabled a)] -> [Maybe a]
	fn _      (Nothing:_) = error "fromVariableSrc: bad state, unknown exists line"
	fn (0:ps) (Just (Just x):xs) = Just x : fn ps xs
	fn (p:ps) (Just (Just x):xs) = Nothing : fn (pred p:ps) xs
	fn ps     (Just Nothing:xs)  = Nothing : fn ps xs
	fn []     xs                 = error ("fromVariableSrc: stutter stream ended!" ++ show xs)
	fn _      []                 = error "fromVariableSrc: stream ended?"

fromSink :: (Show a, Rep a) => Sink a -> [a]
fromSink = fromVariableSink (repeat 0)

fromVariableSink :: forall a . (Show a, Rep a) => [Int] -> Sink a -> [a]
fromVariableSink stutter sink = Maybe.catMaybes $ map snd internal
   where
	val :: Seq (Enabled a)
	val = sink full

	full :: Seq IsFull
	full = toSeq (map fst internal)

	internal :: [(IsFull,Maybe a)]
	internal = fn stutter (fromSeq val)
	
	fn :: [Int] -> [Maybe (Enabled a)] -> [(IsFull,Maybe a)]
	fn (0:ps) ~(x:xs) = (IsFull False,rep) : rest
	   where
		(rep,rest) = case x of
			       Nothing       -> error "fromVariableSink: bad reply to low full status"
			       Just Nothing  -> (Nothing,fn (0:ps) xs)
			       Just (Just v) -> (Just v,fn ps xs)
	fn (p:ps) ~(x:xs) = (IsFull True,Nothing) : fn (pred p:ps) xs

---------------------------------------------------------------------------
-- Shallow Consumers


-- A test, that takes the output from our FSL generator,
-- and returns the back edge (the IsRead, and the values)
testFSLRead :: Env () -> Seq (Enabled Int) -> (Seq IsRead,Seq Int)
testFSLRead env sq = (mkIsRead low,res)
  where (en,val) = unpack sq
        res = register env 0 $ mux2 en (res+1,res)

-- The DUT for the testFSLRead, that includes the backedge/looping.
dut env generator = result
   where
	out1             = generator readSig
	(readSig,result) = testFSLRead env out1
	
{-
dut2 env generator = result
   where
	out1             = generator readSig
	(readSig,result) = testFSLRead env out1	
-}

-- Our main test
main = do
--	print (dut shallowEnv (toVariableSrc [1..] [1..10]))
	t <- reifyCircuit s2
	return ()
	t' <- optimizeCircuit (OptimizationOpts 0) t
--	print t'
	writeDotCircuit "x.dot" t'
	writeVhdlCircuit [] "x" "x.vhd" t'

t1 = fromVariableSink (repeat 1000) ((srcToSink (shallowEnv { resetEnv = pureS False }) (toVariableSrc  (repeat 1000) ([1..]::[Int]))))

-- A one-cell mvar-like FIFO.


--				Seq (IsFull -> Enabled a)	



mvar :: (Rep a) => Env () -> (Seq IsFull -> Seq (Enabled a)) -> (Seq IsRead -> Seq (Enabled a))
mvar env f isRead = value
  where 
	value = register env (pureS Nothing) $
			cASE [ ( (state .==. low) `and2` (isEnabled inp)
			       , inp		-- load new value
			       )
			     , ( (state .==. low) `and2` (bitNot (isEnabled inp))
			       , pureS Nothing	-- load no value
			       )
			     , ( (state .==. high) `and2` (isRead .==. pureS (IsRead True))
			       , pureS Nothing	-- output value
			       )
			     , ( (state .==. high) `and2` (isRead .==. pureS (IsRead False))
			       , value		-- keep value
			       )
			     ] (pureS Nothing)
	inp 	      = f $ mkIsFull state
	-- the state is just a bit
	(state,datum) = unpack value



--mux2 (funMap fsm $ pack (isFull,isRead)) (value,inp)
--	      inp        = f $ mkIsFull full
--	      (full,val) = unpack value
--	      fsm :: (IsFull,IsRead) -> Maybe Bool
--	      fsm = (IsFull True,IsRead True) = return False	-- 

type SrcToSinkState = U1

-- (state x isEnabled x isFull) -> (state x write x IsRead )
fsmSrcToSink :: (SrcToSinkState,Bool,IsFull) -> (SrcToSinkState,Bool,IsRead)
fsmSrcToSink (0,False,_) 	= (0,False,IsRead False)
fsmSrcToSink (0,True,_) 	= (1,False,IsRead True)	-- accept value
fsmSrcToSink (1,_,IsFull False) = (0,True,IsRead False)	-- pass on value (rec. is not full)
fsmSrcToSink (1,_,IsFull True)  = (1,False,IsRead False)	

fsmSrcToSink1 :: (SrcToSinkState,Bool,IsFull) -> SrcToSinkState
fsmSrcToSink1 (a,b,c) = case fsmSrcToSink (a,b,c) of (x,y,z) -> x

fsmSrcToSink2 :: (SrcToSinkState,IsFull) -> Bool
fsmSrcToSink2 (a,c) = case fsmSrcToSink (a,False,c) of (x,y,z) -> y

fsmSrcToSink3 :: (SrcToSinkState,Bool) -> IsRead
fsmSrcToSink3 (a,b) = case fsmSrcToSink (a,b,IsFull False) of (x,y,z) -> z

s2 = srcToSink :: Env () -> Src U4 -> Sink U4
	
-- A passthrough; thats all
srcToSink :: forall a. (Rep a) 
	  => Env () -> (Src a) -> (Sink a)
srcToSink env reader isFull = packEnabled write value
   where
	state, state' :: Seq U1
	state = label "state" $ register env 0 state'

	inp :: Seq (Enabled a)
	inp  = reader $ read 

	read' :: Seq IsRead
	read' = pureS (IsRead False) -- read


	state' = label "state'"
	       $ funMap (return . fsmSrcToSink1) 
	       $ pack (state,isEnabled inp,isFull)


	write :: Seq Bool
	write = label "write"
	     $ funMap (return . fsmSrcToSink2) 
	     $ pack (state,isFull) 

	read :: Seq IsRead
	read = label "read"
	     $ funMap (return . fsmSrcToSink3) 
	     $ pack (state,isEnabled inp)



	value :: Seq a
	value =	delay env $ mux2 (read .==. pureS (IsRead True))
			( enabledVal inp
			, value
			)

cASE :: (Rep b, Signal seq) => [(seq Bool,seq b)] -> seq b -> seq b
cASE [] def = def
cASE ((p,e):pes) def = mux2 p (e,cASE pes def)

{-
t2 :: forall a . (a ~ U4, Rep a) => Env () -> (Seq IsFull,Seq (Enabled a)) -> (Seq IsRead,Seq (Enabled a))
t2 env (full,val) = (read,val')
    where
	val' :: Seq (Enabled a)
	val' = rep full

	read :: Seq IsRead
	f :: Seq IsRead -> Seq (Enabled a)

	(read,f) = unsafeUnapply val

	-- There *is* a commitment to linerity here
	rep :: Seq IsFull -> Seq (Enabled a)
	rep = srcToSink env f
-}	
xxzzy :: ((Seq a -> Seq b) -> c) -> Seq b -> (Seq a,c)
xxzzy = undefined

-- A
unsafeUnapply :: a -> (b, b -> a)
unsafeUnapply a = unsafePerformIO $ do
	v <- newEmptyMVar
	let f b = unsafePerformIO $ do
			putMVar v b
			return a
	let b = unsafePerformIO $ do
			takeMVar v
	return $ (b,f)
	

----

fn :: ((Seq Bool -> Seq Bool) -> Seq Int) -> Seq Int
fn g = g bitNot


