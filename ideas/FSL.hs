{-# LANGUAGE TypeFamilies #-}

-- Use %ghci FSL.hs -i..
-- To load

import Language.KansasLava
import Language.KansasLava.Stream
import Debug.Trace

newtype IsRead = IsRead Bool

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

-- Implements the FSL "Read/RHS" protocol, at max speed
toFSL :: (Rep a) => [a] -> Env () -> (Seq IsRead -> Seq (Enabled a))
toFSL = toVariableFSL (repeat 0)


mkIsRead :: Seq Bool -> Seq IsRead
mkIsRead = liftS1 $ \ (Comb a (D ea)) 
	              -> (Comb (optX $ fmap IsRead $ unX a) (D ea :: D IsRead))

toVariableFSL :: (Rep a) => [Int] -> [a] -> Env () -> (Seq IsRead -> Seq (Enabled a))
toVariableFSL stutter xs env isRead = toSeq (fn stutter xs (fromSeq isRead))
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

newtype HasCapacity = HasCapacity Bool

-- A test, that takes the output from our FSL generator, and returns the back edge (the IsRead, and 
testFSLRead :: Env () -> Seq (Enabled Int) -> (Seq IsRead,Seq Int)
testFSLRead env sq = (mkIsRead low,res)
  where (en,val) = unpack sq
        res = register env 0 $ mux2 en (res+1,res)

-- The DUT for the testFSLRead, that includes the backedge/looping.
dut env generator = result
   where
	out1             = generator readSig
	(readSig,result) = testFSLRead env out1

-- Our main test
main = do
	print (dut shallowEnv (toVariableFSL [1..] [1..10] shallowEnv))


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