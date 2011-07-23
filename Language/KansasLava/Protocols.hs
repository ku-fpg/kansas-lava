{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances #-}
module Language.KansasLava.Protocols (
	module Language.KansasLava.Protocols.Enabled,
	module Language.KansasLava.Protocols.Memory,
	module Language.KansasLava.Protocols.HandShake,
	module Language.KansasLava.Protocols.MailBox
	) where



import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Protocols.HandShake
import Language.KansasLava.Protocols.MailBox

{-
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
import System.Random

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
			[ ("clk_en",B,  ClkDom "domain")
   		        , ("clk",ClkTy, Pad $ OVar (-2) "clk")
   		        , ("rst",B,     Pad $ OVar (-1) "rst")
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



-- An Ack is always in response to an incoming packet or message
newtype Ready = Ready { unReady :: Bool }

instance Rep Ready where
  data X Ready = XReadyRep { unXReadyRep :: (X Bool) }
  type W Ready = W Bool
  -- The template for using representations
  unX             = liftM Ready   . unX  . unXReadyRep
  optX            = XReadyRep     . optX . liftM unReady 
  toRep           = toRep       . unXReadyRep
  fromRep         = XReadyRep     . fromRep
  repType Witness = repType (Witness :: Witness Bool)

toReady :: Comb Bool -> Comb Ready
toReady = coerce Ready

fromReady :: Comb Ready -> Comb Bool
fromReady = coerce unReady


----------------------------------------------------------------------------------------------------

toInvited :: (Rep a, Clock c, sig ~ CSeq c)
             => [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Ready	     -- ^ takes a flag back from FIFO that indicates a write would be successful
             -> sig (Enabled a)     

toInvited ys ack = toSeq (fn ys (fromSeq ack))
        where
--           fn xs cs | trace (show ("fn",take  5 cs,take 5 cs)) False = undefined
	   -- send the value *before* checking thr Ack
           fn xs ys' = 
                case ys' of
                 (Nothing:_)                  -> error "toInvited: bad protocol state (1)"
                 (Just (Ready True) :rs) -> 
			case xs of
			   (x:xs') -> x : fn xs' rs      -- has been written
			   []      -> Nothing : fn [] rs -- nothing to write
                 (Just (Ready False):rs) -> Nothing : fn xs rs   -- not ready yet

-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I suspect this space-leaks.
fromHandShake :: (Rep a, Clock c, sig ~ CSeq c)
               => sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Ack, [Maybe a]) -- ^ read-ready flag sent back to FIFO and shallow list of values
fromHandShake inp = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp)

        fn (x:xs) = (Ack True,rep) : rest
           where
                (rep,rest) = case x of
                               Nothing       -> error "fromVariableHandshake: bad reply to ready status"
                               Just Nothing  -> (Nothing,fn xs)
                               Just (Just v) -> (Just v,fn xs)
        fn [] = (Ack False,Nothing) : fn []


-}
