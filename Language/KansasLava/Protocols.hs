{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes #-}
module Language.KansasLava.Protocols 
        ( module Language.KansasLava.Protocols 
        ,  -- * Hand Shake
        , toHandShake
        , fromHandShake
        , shallowHandShakeBridge, 
        , mVarToHandShake
        , handShakeToMVar
        , interactMVar
        , hInteract
        ) where

import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Stream as Stream
import Language.KansasLava.Types
import Language.KansasLava.Utils

import Data.Sized.Matrix as M
import Control.Applicative hiding (empty)
import Data.Maybe  as Maybe
-- import Language.KansasLava.Radix as Radix
import Control.Concurrent
import qualified Data.ByteString.Lazy as BS
import System.IO
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Word

import qualified Prelude
import Prelude hiding (tail, lookup)


type Enabled a = Maybe a

type Pipe a d = Enabled (a,d)

type Memory clk a d = CSeq clk a -> CSeq clk d

enabledRegister :: forall a clk. (Rep a, Clock clk) => CSeq clk (Enabled a) -> CSeq clk a
enabledRegister inp = res
   where
	(en,v) = unpack inp
	res    = delay (mux2 en (v,res))

-- | Turns a list of maybe values into enabled values.
toEnabledSeq :: forall a . (Rep a) => [Maybe a] -> Seq (Enabled a)
toEnabledSeq xs = toSeqX [ optX (Just x)
			 | x <- xs
			 ]

memoryToPipe ::  forall a d clk . (Rep a, Rep d, Clock clk) =>  CSeq clk (Enabled a) -> Memory clk a d -> CSeq clk (Pipe a d)
memoryToPipe enA mem = pack (delay en,pack (delay a,mem a))
   where
	(en,a) = unpack enA


pipeToMemory :: forall a d clk1 . (Size a, Clock clk1, Rep a, Rep d)
	=> CSeq clk1 (Pipe a d)
	-> Memory clk1 a d
pipeToMemory pipe addr2 = syncRead (writeMemory (delay pipe)) addr2

-- Later, we will have a two clock version.

-- Does not work for two clocks, *YET*
-- call writeMemory
writeMemory :: forall a d clk1 . (Clock clk1, Size a, Rep a, Rep d)
	=> CSeq clk1 (Pipe a d)
	-> CSeq clk1 (a -> d)
writeMemory pipe = res
  where
	-- Adding a 1 cycle delay, to keep the Xilinx tools happy and working.
	-- TODO: figure this out, and fix it properly
	(wEn,pipe') = unpack $ {- register (pureS Nothing) $ -} pipe
	(addr,dat) = unpack pipe'

    	res :: CSeq clk1 (a -> d)
    	res = Seq shallowRes (D $ Port ("o0") $ E $ entity)

	shallowRes :: Stream (X (a -> d))
	shallowRes = pure (\ m -> XFunction $ \ ix ->
			case getValidRepValue (toRep (optX (Just ix))) of
			       Nothing -> optX Nothing
			       Just a' -> case lookup a' m of
					    Nothing -> optX Nothing
					    Just v -> optX (Just v)
			  )
			<*> mem -- (emptyMEM :~ mem)
--			    <*> ({- optX Nothing :~ -} seqValue addr2)

	-- This could have more fidelity, and allow you
	-- to say only a single location is undefined
	updates :: Stream (Maybe (Maybe (a,d)))
	updates = stepifyStream (\ a -> case a of
					Nothing -> ()
					Just b -> case b of
						   Nothing -> ()
						   Just (c,d) -> eval c `seq` eval d `seq` ()
			        )
		$ pure (\ e a b ->
			   do en'   <- unX e
			      if not en'
				     then return Nothing
				     else do
			      		addr' <- unX a
			      		dat'  <- unX b
			      		return $ Just (addr',dat')
		       ) <*> seqValue wEn
			 <*> seqValue addr
			 <*> seqValue dat

	-- mem
{-
	mem :: Stream (Map [Bool] d)
	mem = id -- stepify
	    $ Map.empty :~ Stream.fromList
		[ case u of
		    Nothing           -> Map.empty	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) -> ((Map.insert $! (M.toList $! (fromWireRep a :: Matrix (WIDTH a) Bool))) $!) d $! m
		| u <- Stream.toList updates
		| m <- Stream.toList mem
		]
-}
	mem :: Stream (Radix d)
	mem = stepifyStream (\ a -> a `seq` ())
	    $ Cons empty $ Stream.fromList
		[ case u of
		    Nothing           -> empty	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) ->
			case getValidRepValue (toRep (optX (Just a))) of
			  Just bs -> ((insert $! bs) $! d) $! m
                          Nothing -> error "mem: can't get a valid rep value"
		| u <- Stream.toList updates
		| m <- Stream.toList mem
		]

    	entity :: Entity E
    	entity =
		Entity (Prim "write")
			[ ("o0",bitTypeOf res)]
			[ ("env",ClkDomTy, unD $ (clock :: D clk1))
			, ("wEn",bitTypeOf wEn,unD $ seqDriver wEn)
			, ("wAddr",bitTypeOf addr,unD $ seqDriver addr)
			, ("wData",bitTypeOf dat,unD $ seqDriver dat)
                        , ("element_count"
                          , GenericTy
                          , Generic (fromIntegral (M.size (error "witness" :: a)))
                          )
			]
{-
readMemory :: forall a d sig clk . (Clock clk, sig ~ CSeq clk, Size a, Rep a, Rep d)
	=> sig (a -> d) -> sig a -> sig d
readMemory mem addr = unpack mem addr
-}

-- This is an alias
readMemory :: forall a d sig . (Signal sig, Size a, Rep a, Rep d)
	=> sig (a -> d) -> sig a -> sig d
readMemory mem addr = asyncRead mem addr


syncRead :: forall a d sig clk . (Clock clk, sig ~ CSeq clk, Size a, Rep a, Rep d)
	=> sig (a -> d) -> sig a -> sig d
syncRead mem addr = delay (asyncRead mem addr)

asyncRead :: forall a d sig . (Signal sig, Size a, Rep a, Rep d)
	=> sig (a -> d) -> sig a -> sig d
asyncRead = liftS2 $ \ (Comb (XFunction f) me) (Comb x xe) ->
				Comb (case (unX x) of
				    	Just x' -> f x'
				    	Nothing -> optX Nothing
			     	     )
			$ entity2 (Prim "asyncRead") me xe

-- | memoryToMatrix should be used with caution/simulation  only,
-- because this actually clones the memory to allow this to work,
-- generating lots of LUTs and BRAMS.

memoryToMatrix ::  (Integral a, Size a, Rep a, Rep d, Clock clk, sig ~ CSeq clk)
	=> sig (a -> d) -> sig (Matrix a d)
memoryToMatrix mem = pack (forAll $ \ x -> asyncRead mem (pureS x))

fullEnabled :: forall a b sig . (Signal sig, Show a, Rep a, Show b, Rep b)
	   => sig a -> (a -> Maybe b) -> sig (Enabled b)
fullEnabled iseq f = pack (funMap (return . isJust . f) iseq :: sig Bool,funMap f iseq :: sig b)

enabledToPipe :: (Rep x, Rep y, Rep z, Signal sig) => (Comb x -> Comb (y,z)) -> sig (Enabled x) -> sig (Pipe y z)
enabledToPipe f se = pack (en, (liftS1 f x))
   where (en,x) = unpack se

-- This is lifting *Comb* because Comb is stateless, and the 'en' Bool being passed on assumes no history,
-- in the 'a -> b' function.
mapEnabled :: (Rep a, Rep b, Signal sig) => (Comb a -> Comb b) -> sig (Enabled a) -> sig (Enabled b)
mapEnabled f en = pack (en_bool,liftS1 f en_val)
   where (en_bool,en_val) = unpack en

zipEnabled :: (Rep a, Rep b, Rep c, Signal sig) => (Comb a -> Comb b -> Comb c) -> sig (Enabled a) -> sig (Enabled b) -> sig (Enabled c)
zipEnabled f en1 en2 = pack (en_bool1 `phi` en_bool2,liftS2 f en_val1 en_val2)
   where (en_bool1,en_val1) = unpack en1
	 (en_bool2,en_val2) = unpack en2


phi :: forall a sig . (Signal sig, Rep a) => sig a -> sig a -> sig a
phi = liftS2 $ \ (Comb a ea) (Comb b _) ->
        Comb (if toRep a == toRep b
		then a
		else optX $ (fail "phi problem" :: Maybe a))	-- an internal error, like an assert
		(ea) -- pick one, they are the same
			-- later, consider puting the phi nodes into the deep syntax

mapPacked :: (Pack sig a, Pack sig b) => (Unpacked sig a -> Unpacked sig b) -> sig a -> sig b
mapPacked f = pack . f . unpack

enabledS :: (Rep a, Signal sig) => sig a -> sig (Enabled a)
enabledS s = pack (pureS True,s)

disabledS :: (Rep a, Signal sig) => sig (Enabled a)
disabledS = pack (pureS False,undefinedS)

packEnabled :: (Rep a, Signal sig) => sig Bool -> sig a -> sig (Enabled a)
packEnabled s1 s2 = pack (s1,s2)

unpackEnabled :: (Rep a, Signal sig) => sig (Enabled a) -> (sig Bool, sig a)
unpackEnabled sig = unpack sig

enabledVal :: (Rep a, Signal sig) => sig (Enabled a) -> sig a
enabledVal = snd .  unpackEnabled

isEnabled :: (Rep a, Signal sig) => sig (Enabled a) -> sig Bool
isEnabled = fst .  unpackEnabled

-- a 'safe' delay that uses the disabled to give a default value.
delayEnabled :: (Rep a, Clock clk) => CSeq clk (Enabled a) -> CSeq clk (Enabled a)
delayEnabled inp = register Nothing inp

{-
-- to move into a counters module
-- Count the number of ticks on a signal. Notice that we start at zero (no ticks),
-- and bump the counter at each sighting.
countTicks :: forall clk x . (Rep x) => x -> (Comb x -> Comb x) ->  CSeq clk Bool -> CSeq clk (Enabled x)
countTicks init succ sysEnv enable = packEnabled enable ctr
   where
        ctr :: CSeq clk x
        ctr = register sysEnv (pureS init) val

        val :: CSeq clk x
        val = mux2 enable (liftS1 succ ctr,ctr)


-- compare with my previous value
cmp :: (Wire a) =>  (Comb a -> Comb a -> Comb b) -> CSeq clk a -> CSeq clk b
cmp env f inp = liftS2 f (delay env inp) inp

-}

zipPacked :: (Pack sig a, Pack sig b, Pack sig c) => (Unpacked sig a -> Unpacked sig b -> Unpacked sig c) -> sig a -> sig b -> sig c
zipPacked f x y = pack $ f (unpack x) (unpack y)

mapPipe :: (Signal sig, Rep a, Rep b, Rep x) => (Comb a -> Comb b) -> sig (Pipe x a) -> sig (Pipe x b)
mapPipe f = mapEnabled (mapPacked $ \ (a0,b0) -> (a0,f b0))

-- | only combines pipes when both inputs are enabled, and *assumes* the
-- x addresses are the same.
zipPipe :: (Signal sig, Rep a, Rep b, Rep c, Rep x) => (Comb a -> Comb b -> Comb c) -> sig (Pipe x a) -> sig (Pipe x b) -> sig (Pipe x c)
zipPipe f = zipEnabled (zipPacked $ \ (a0,b0) (a1,b1) -> (a0 `phi` a1,f b0 b1))


--
joinEnabled :: (Signal sig, Rep a) => sig (Enabled a) -> sig (Enabled a) -> sig (Enabled a)
joinEnabled = liftS2 $ \ e1 e2 ->
			let (en1,v1) = unpack e1
	 		    (en2,v2) = unpack e2
	                in pack (en1 `or2` en2, mux2 en1 (v1,v2))



shiftRegister :: (Rep d, Integral x, Size x, Clock clk) =>  CSeq clk (Enabled d) -> CSeq clk (Matrix x d)
shiftRegister inp = pack m
  where
	(en,val) = unpack inp
	(m, _)   = scanR fn (val, forAll $ \ _ -> ())
	fn (v,()) = (reg,reg)
		where reg = enabledRegister (pack (en,v))


unShiftRegister :: forall x d clk . (Integral x, Size x, Rep d, Clock clk) =>  CSeq clk (Enabled (Matrix x d)) -> CSeq clk (Enabled d)
unShiftRegister inpSig = r
  where
	en :: CSeq clk Bool
	m :: CSeq clk (Matrix x d)
	(en,m) = unpack inpSig
	r :: CSeq clk (Enabled d)
	(_, r) = scanR fn (pack (low,undefinedSeq), unpack m)

	fn (carry,inp) = ((),reg)
	  where (en',mv) = unpack carry
		reg = (delay
		 	      (mux2 en ( pack (high,inp),
				         pack (en',mv)
			)))



-- Should really be in Utils (but needs Protocols!)
-- Assumes input is not too fast; double buffering would fix this.

runBlock :: forall a b x y clk . (Rep x, Bounded x, Integral y, Integral x, Size x, Size y, Rep a, Rep b, Clock clk)
	 => (Comb (Matrix x a) -> Comb (Matrix y b))
	 -> CSeq clk (Enabled a)
	 -> CSeq clk (Enabled b)
runBlock fn inp = unShiftRegister
		       $ addSync
		       $ liftS1 fn
		       $ shiftRegister inp
   where
	addSync a = pack (syncGo,a)

	(en,_) = unpack inp

	-- counting n things put into the queue
	syncGo :: CSeq clk Bool
	syncGo = delay (pureS maxBound .==. syncCounter)

	syncCounter :: CSeq clk x
	syncCounter = counter' en

-- If the Seq Bool is enabled, then we want to generate the
-- next number in the sequence, in the *next* cycle.

-- TODO: remove, its confusing
counter' :: (Rep x, Num x, Clock clk) =>  CSeq clk Bool -> CSeq clk x
counter' inc = res
   where res = register 0 (res + mux2 inc (1,0))



--------------------------------------------------


-- The order here has the function *last*, because it allows
-- for a idiomatic way of writing things
--
--  res = rom inp $ \ a -> ....
--
rom :: (Rep a, Rep b, Clock clk) => CSeq clk a -> (a -> Maybe b) -> CSeq clk b
rom inp fn = delay $ funMap fn inp

---------------------------------


latch :: forall a. (Rep a) => Seq (Enabled a) -> Seq a
latch inp0 = out
  where out = mux2 (isEnabled inp0) (enabledVal inp0,delay out)

----------------------

class Stepify a where
  stepify :: a -> a

--class Rep a => Eval a where

--instance (Rep a) => Stepify (Seq a) where
--  stepify (Seq a d) = Seq (stepify a) d

-- one step behind, to allow knot tying.
--instance (Rep a) => Stepify (Stream a) where
--  stepify (a :~ r) = a :~ (eval a `seq` stepify r)

stepifyStream :: (a -> ()) -> Stream a -> Stream a
stepifyStream f (Cons a r) = Cons a (f a `seq` stepifyStream f r)

--instance Wire (Map [Bool] d) where {}

-- instance Rep (Map (M.Matrix x Bool) d) where {}


{-

instance Eval (WireVal a) where
    eval WireUnknown = ()
    eval (WireVal a) = a `seq` ()

instance (Eval a) => Eval (Maybe a) where
    eval (Just a)  = eval a
    eval (Nothing) = ()

instance (Eval a, Eval b) => Eval (a,b) where
	eval (a,b) = eval a `seq` eval b `seq` ()
-}



-- | A 'Radix' is a trie indexed by bitvectors.
data Radix a
  = Res !a -- ^ A value stored in the tree
  | NoRes -- ^ Non-present value
  -- | A split-node, left corresponds to 'True' key bit, right corresponds to 'False' key bit.
  | Choose !(Radix a) !(Radix a)
	deriving Show

-- | The empty tree
empty :: Radix a
empty = NoRes

-- | Add a value (keyed by the list of bools) into a tree
insert :: [Bool] -> a -> Radix a -> Radix a
insert []    y (Res _) = Res $! y
insert []    y NoRes   = Res $! y
insert []    _ (Choose _ _) = error "inserting with short key"
insert xs     y NoRes   = insert xs y (Choose NoRes NoRes)
insert _  _ (Res _) = error "inserting with too long a key"
insert (True:a) y (Choose l r) = Choose (insert a y l) r
insert (False:a) y (Choose l r) = Choose l (insert a y r)


-- | Find a value in a radix tree
lookup :: [Bool] -> Radix a -> Maybe a
lookup [] (Res v) = Just v
lookup [] NoRes   = Nothing
lookup [] _       = error "lookup error with short key"
lookup (_:_) (Res _) = error "lookup error with long key"
lookup (_:_) NoRes   = Nothing
lookup (True:a) (Choose l _) = lookup a l
lookup (False:a) (Choose _ r) = lookup a r


------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toHandShake :: (Rep a, Clock c, sig ~ CSeq c)
             => [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Bool
             -> sig (Enabled a)     -- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
toHandShake ys ready = toSeq (fn ys (fromSeq ready))
        where
--           fn xs cs | trace (show ("fn",take  5 cs,take 5 cs)) False = undefined
           fn (x:xs) ys' = x :
                case (x,ys') of
                 (_,Nothing:_)          -> error "toHandShake: bad protocol state (1)"
                 (Just _,Just True:rs)  -> fn xs rs     -- has been written
                 (Just _,Just False:rs) -> fn (x:xs) rs -- not written yet
                 (Nothing,Just _:rs)    -> fn xs rs     -- nothing to write
                 (_,[])                 -> error "toHandShake: can't handle empty list of values to receive"
           fn [] _ = error "toHandShaken: can't handle empty list of values to issue"


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I suspect this space-leaks.
fromHandShake :: (Rep a, Clock c, sig ~ CSeq c)
               => sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Bool, [Maybe a]) -- ^ read-ready flag sent back to FIFO and shallow list of values
fromHandShake inp = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp)

        fn (x:xs) = (True,rep) : rest
           where
                (rep,rest) = case x of
                               Nothing       -> error "fromVariableHandshake: bad reply to ready status"
                               Just Nothing  -> (Nothing,fn xs)
                               Just (Just v) -> (Just v,fn xs)
        fn [] = (False,Nothing) : fn []


-- introduce protocol-compliant delays.

shallowHandShakeBridge :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c, Show a) => (sig Bool,sig Bool) -> (sig (Enabled a),sig Bool) -> (sig (Enabled a),sig Bool)
shallowHandShakeBridge (lhsS,rhsS) (inp,back)
        = unpack (toSeq $ fn (fromSeq lhsS) (fromSeq rhsS) (fromSeq inp) (fromSeq back) [])
   where
        fn :: [Maybe Bool] -> [Maybe Bool] -> [Maybe (Enabled a)] -> [Maybe Bool] -> [a] -> [(Enabled a,Bool)]
--        fn _ _ (x:_) _ store | trace (show ("fn",x,store)) False = undefined
        fn (Just True:lhss) rhss (Just (Just a):as) bs store = fn2 lhss rhss as bs (store ++ [a]) True
        fn (_:lhss)      rhss (Just _:as)        bs store = fn2 lhss rhss as bs (store)        False
        fn _ _ _ _ _ = error "failure in shallowHandShakenBridge (fn)"

--        fn2 _ _ _ _ store bk | trace (show ("fn2",store,bk)) False = undefined
        fn2 lhss (Just True:rhss) as bs (s:ss) bk = (Just s,bk) :
                                                 case bs of
                                                   (Just True : bs')  -> fn lhss rhss as bs' ss
                                                   (Just False : bs') -> fn lhss rhss as bs' (s:ss)
                                                   _ -> error "failure in shallowHandShakenBridge (fn2/case)"

        fn2 lhss (Just _:rhss)    as bs store bk  = (Nothing,bk)   : fn lhss rhss as (Prelude.tail bs) store
        fn2 _ _ _ _ _ _ = error "failure in shallowHandShakenBridge (fn2)"


----------------------------------------------------------------------------------------------------
-- These are functions that are used to thread together Hand shaking and FIFO.

-- | This function takes a MVar, and gives back a Handshaken signal that represents
-- the continuous sequence of contents of the MVar.

mVarToHandShake :: (Clock c, Rep a) => MVar a -> IO (CSeq c Bool -> (CSeq c (Enabled a)))
mVarToHandShake sfifo = do
        xs <- getFIFOContents sfifo
        return (toHandShake xs)
 where
        getFIFOContents :: MVar a -> IO [Maybe a]
        getFIFOContents var = unsafeInterleaveIO $ do
 	        x <- tryTakeMVar var
 	        xs <- getFIFOContents var
 	        return (x:xs)

handShakeToMVar :: (Clock c, Rep a) => MVar a -> (CSeq c Bool -> CSeq c (Enabled a)) -> IO ()
handShakeToMVar sfifo sink = do
        sequence_
                $ map (putMVar sfifo)
                $ Maybe.catMaybes
                $ (let (back,res) = fromHandShake $ sink back in res)
        return ()


-- interactMVar
interactMVar :: forall src sink
         . (Rep src, Rep sink)
        => (forall clk sig . (Clock clk, sig ~ CSeq clk) => (sig (Enabled src),sig Bool) -> (sig Bool,sig (Enabled sink)))
        -> MVar src
        -> MVar sink
        -> IO ()
interactMVar fn varA varB = do
        inp_fifo <- mVarToHandShake varA

        handShakeToMVar varB $ \ rhs_back ->
                -- use fn at a specific (unit) clock
                let (lhs_back,rhs_out) = fn (lhs_inp,rhs_back :: CSeq () Bool)
                    lhs_inp = inp_fifo lhs_back
                in
                    rhs_out

hInteract :: (forall clk sig . (Clock clk, sig ~ CSeq clk)
                => (sig (Enabled Word8),sig Bool) -> (sig Bool, sig (Enabled Word8))
             )
          -> Handle
          -> Handle
          -> IO ()
hInteract fn inp out = do
        inp_fifo_var <- newEmptyMVar
        out_fifo_var <- newEmptyMVar

        -- send the inp handle to the inp fifo
        _ <- forkIO $ forever $ do
                bs <- BS.hGetContents inp
                sequence_ $ map (putMVar inp_fifo_var) $ BS.unpack bs

        -- send the out fifo to the out handle
        _ <- forkIO $ forever $ do
                x <- takeMVar out_fifo_var
                BS.hPutStr out $ BS.pack [x]

        interactMVar fn inp_fifo_var out_fifo_var

