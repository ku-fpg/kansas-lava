{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Language.KansasLava.Protocols.FSL where

import Language.KansasLava.Stream
import Language.KansasLava.Wire
import Language.KansasLava.Types
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Language.KansasLava.Comb
import Language.KansasLava.Protocols
import Language.KansasLava.StdLogicVector
import Language.KansasLava.Utils -- for fromSLV

import Data.Sized.Ix
import Data.Sized.Unsigned
import Debug.Trace
import Data.Maybe as Maybe
import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.Char as Char 
import System.IO
import Control.Concurrent

---------------------------------------------------------------------------
-- Key Types

type Src  a = Seq IsRead -> Seq (Enabled a)		-- pull (from producer)
type Sink a = Seq IsFull -> Seq (Enabled a)		-- push (to consumer)

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
	fromRep w rep	 	 = error ("size error for IsRead" ++ show rep)



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
	fromRep w rep	 	 = error ("size error for IsFull" ++ show rep)

--------------------------------------------------
-- Shallow Generators

-- Implements the FSL "Read/RHS" protocol, at max speed
toSrc :: (Rep a) => [a] -> Src a
toSrc = toVariableSrc (repeat 0) . map Just

toVariableSrc :: (Rep a) => [Int] -> [Maybe a] -> Src a
toVariableSrc stutter xs isRead = toSeq (fn stutter xs (fromSeq isRead))
	where
	   -- We rely on the semantics of pattern matching to not match (x:xs)
	   -- if (0:ps) does not match.
	   fn (0:ps) (Just x:xs) c 
		    = [Just x]
		    ++ case c of -- read c after issuing Just x
			(Nothing:rs)             -> error "toVariableSrc: bad protocol state (1)"
			(Just (IsRead True):rs)  -> fn ps xs rs	    -- has been read
			(Just (IsRead False):rs) -> fn (0:ps) (Just x:xs) rs -- not read yet
	   fn (0:ps) (Nothing:xs) c 
		    = [Nothing]
		    ++ case c of
			(Nothing:rs)             -> error "toVariableSrc: bad protocol state (2a)"
			(Just (IsRead True):rs)  -> error "toVariableSrc: bad protocol state (3a)"
			(Just (IsRead False):rs) -> fn ps xs rs -- nothing sent, nothing read
	   fn (p:ps) xs c
		    = [Nothing]
		    ++ case c of
			(Nothing:rs)             -> error "toVariableSrc: bad protocol state (2b)"
			(Just (IsRead True):rs)  -> error "toVariableSrc: bad protocol state (3b)"
			(Just (IsRead False):rs) -> fn (pred p:ps) xs rs -- nothing read
	   fn ps [] c = [Nothing]
		    ++ case c of
			(Nothing:rs)             -> error "toVariableSrc: bad protocol state (4)"
			(Just (IsRead True):rs)  -> error "toVariableSrc: bad protocol state (5)"
			(Just (IsRead False):rs) -> fn ps [] rs -- nothing read, ever

{-
-- TODO: Consider issues
--toVariableSink :: (Rep a) => [Int] -> [Maybe a] -> Sink a
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
-}

---------------------------------------------------------------------------
-- Shallow Consumers
{-
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
-}

fromSink :: (Show a, Rep a) => Sink a -> [a]
fromSink = Maybe.catMaybes . fromVariableSink (repeat 0)

fromVariableSink :: forall a . (Rep a) => [Int] -> Sink a -> [Maybe a]
fromVariableSink stutter sink = map snd internal
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

-------------------------------------------------------------------------------------------

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

--------------------------------------------------------------------------

fmapSrc :: (Rep a, Rep b) => (Comb a -> Comb b) -> Src a -> Src b
fmapSrc f src rd = liftS1 (mapEnabled f) (src rd)

fmapSink :: (Rep a, Rep b) => (Comb a -> Comb b) -> Sink a -> Sink b
fmapSink f src rd = liftS1 (mapEnabled f) (src rd)

--------------------------------------------------------------------------
{-
-- TODO: consider
readFileWith :: (c ~ Byte) => String -> ([c] -> r) -> IO r
readFileWith str toOut = do
	str <- readFile str
	let vals = map Char.ord str
	print vals
	return (toOut $ map fromIntegral vals)


mVarToSrc :: (c ~ Byte) => MVar c -> IO (Src c)
mVarToSrc var = do
	xs <- getMVarContents var
	return (toSrc xs)

sinkToMVar :: (c ~ Byte) => MVar c -> Sink c -> IO ()
sinkToMVar var sink = putMVarContents var (fromSink sink)
-}

---------------------------------------------------------

-- We include maybe, so we can simulate the concept
-- of there being no data available to pass on
-- at a specific point from a FIFO.

data ShallowFIFO a = ShallowFIFO (MVar (Maybe a))

newShallowFIFO :: IO (ShallowFIFO a)
newShallowFIFO = do
	var <- newEmptyMVar 
	return $ ShallowFIFO var
	

-- | blocks if the FIFO is not cycled on.
--   Nothing means step a cycle;
--   Just means cycle until value is read.
writeToFIFO :: ShallowFIFO a -> Maybe a -> IO ()
writeToFIFO (ShallowFIFO var) a = putMVar var a

-- | block if the FIFO has no values in it yet.
-- Nothing means no value issued in a cycle;
-- Just means value accepted from circuit.
readFromFIFO :: ShallowFIFO a -> IO (Maybe a)
readFromFIFO (ShallowFIFO var) = takeMVar var

-- | runs a fifo forever and empty values.
--  return imeduately after forking worker thread.
{-
execFIFO :: ShallowFIFO a -> IO ()
execFIFO fifo = forkIO loop
   where
	loop = do writeToFIFO fifo Nothing ; loop
-}

fifoToSrc :: (Rep a) => ShallowFIFO a -> IO (Src a)
fifoToSrc (ShallowFIFO var) = do
	xs <- getMVarContents var
	return (toVariableSrc (repeat 0) xs)

sinkToFifo :: (Rep a) => ShallowFIFO a -> Sink a -> IO ()
sinkToFifo (ShallowFIFO var) sink = do
	putMVarContents var (fromVariableSink (repeat 0) sink)
	return ()

-- | readFileToFifo returns after the file has been consumed
-- by the FIFO.

readFileToFifo :: String -> ShallowFIFO Byte -> IO ()
readFileToFifo file fifo = do
	h <- openFile file ReadMode
	hGetToFifo h fifo


writeFileFromFifo :: String -> ShallowFIFO Byte -> IO ()
writeFileFromFifo file fifo = do
	h <- openFile file WriteMode
	hSetBuffering h NoBuffering	
	hPutFromFifo h fifo

-- | read a file into a fifo as bytes.
hGetToFifo :: Handle -> ShallowFIFO Byte -> IO ()
hGetToFifo h (ShallowFIFO var) = do 
	str <- hGetContents h
	putMVarContents var (map (Just . toByte) str)


hPutFromFifo :: Handle -> ShallowFIFO Byte -> IO ()
hPutFromFifo h (ShallowFIFO var) = do
   forkIO $ do
	xs <- getMVarContents var
	sequence_ 
		[ do hPutChar h $ fromByte x
		     hFlush h
	        | Just x <- xs
	        ]
	-- never finishes
   return ()

---------------------------------------------------------
-- Candidates for another package

-- Very general, might be useful elsewhere
getMVarContents :: MVar a -> IO [a]
getMVarContents var = unsafeInterleaveIO $ do
	x <- takeMVar var
	xs <- getMVarContents var
	return (x:xs)

putMVarContents :: MVar a -> [a] -> IO ()
putMVarContents var xs = sequence_ (map (putMVar var) xs)

-- Runs a program from stdin to stdout;
-- also an example of coding
-- interact :: (Src a -> Sink b) -> IO ()

