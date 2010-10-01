{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

-- Use %ghci FSL.hs -i..
-- To load

import Language.KansasLava
import Language.KansasLava.Stream
import Data.Sized.Unsigned
import Debug.Trace
import Data.Maybe as Maybe

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
	print (dut shallowEnv (toVariableSrc [1..] [1..10]))

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


