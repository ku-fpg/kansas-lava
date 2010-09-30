{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

-- Use %ghci FSL.hs -i..
-- To load

import Language.KansasLava
import Language.KansasLava.Stream
import Data.Sized.Unsigned
import Debug.Trace

---------------------------------------------------------------------------
-- Key Types

type Sink a = Seq IsFull -> Seq (Enabled a)		-- push
type Src  a = Seq IsRead -> Seq (Enabled a)		-- pull

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
	fromRep w _	 	 = error "size error for Bool"



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
	fromRep w _	 	 = error "size error for Bool"

---------------------------------------------------------------------------
-- Shallow Generators

-- Implements the FSL "Read/RHS" protocol, at max speed
toSrc :: (Rep a) => [a] -> Env () -> Src a
toSrc = toVariableSrc (repeat 0)

toVariableSrc :: (Rep a) => [Int] -> [a] -> Env () -> Src a
toVariableSrc stutter xs env isRead = toSeq (fn stutter xs (fromSeq isRead))
	where
	   -- We rely on the semantics of pattern matching to not match (x:xs)
	   -- if (0:ps) does not match.
	   fn (0:ps) (x:xs) c 
		    = [Just x]
		    ++ case c of -- read c after issuing Just x
			(Nothing:rs)             -> error "FSL/Read: bad protocol state (2)"
			(Just (IsRead True):rs)  -> fn ps xs rs	    -- has been read
			(Just (IsRead False):rs) -> fn ps (x:xs) rs -- not read yet
	   fn (p:ps) xs c
		    = [Nothing]
		    ++ case c of
			(Nothing:rs)             -> error "FSL/Read: bad protocol state (2)"
			(Just (IsRead True):rs)  -> error "FSL/Read: bad protocol state (3)"
			(Just (IsRead False):rs) -> fn (pred p:ps) xs rs -- nothing read

---------------------------------------------------------------------------
-- Shallow Consumers

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
	print (dut shallowEnv (toVariableSrc [1..] [1..10] shallowEnv))

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

-- A passthrough; thats all
srcToSink :: forall a. (Rep a) 
	  => Env () -> (Seq IsRead -> Seq (Enabled a)) -> (Seq IsFull -> Seq (Enabled a))
srcToSink env reader isFull = packEnabled (state .==. 1) value
   where
	state, state' :: Seq U1
	state = register env 0 state'

	inp :: Seq (Enabled a)
	inp  = reader $ read
	
	write :: Seq Bool
	read :: Seq IsRead
	(state',write,read) =
		  unpack
	 	$ funMap (return . fsmSrcToSink) 
		$ pack (state,isEnabled inp,isFull)
		 	
	value :: Seq a
	value = delay env $ mux2 (read .==. pureS (IsRead True))
			( enabledVal inp
			, value
			)
	
	
{-
main = 
	fn = 
-}

{-
-- Implements the FSL "Write/LHS" protocol, again at max speed
-- Note the Nothing in the result represents Unknown
fromFSL :: (Rep a) => (Seq Bool -> Seq (Enabled a)) -> [Maybe a]
fromFSL fn = concatMap
 		(\ c -> case c of
			   Nothing       -> [Nothing]
			   Just (Just a) -> [Just a]
			   Just Nothing  -> []
	        ) (fromSeq (fn low))

{-
fromVariableFSL :: (Rep a) -> [Int] -> (Seq Bool -> Seq (Enabled a)) -> [Maybe a]
fromVariableFSL stutter f = 
	where
		res = f
		
:: (Rep a) -> [Int] -> (Seq Bool -> Seq (Enabled a)) -> [Maybe a]
-}

-- It is not possible for a FIFO to go from "EMPTY" to "FULL" without a data push causing it

-}

cASE :: (Rep b, Signal seq) => [(seq Bool,seq b)] -> seq b -> seq b
cASE [] def = def
cASE ((p,e):pes) def = mux2 p (e,cASE pes def)


