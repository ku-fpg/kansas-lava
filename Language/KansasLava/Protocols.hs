{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Language.KansasLava.Protocols where

import Language.KansasLava.Comb
import Language.KansasLava.Seq
-- import Language.KansasLava.Entity
import Language.KansasLava.Wire
import Language.KansasLava.Utils
import Language.KansasLava.Types
import Language.KansasLava.Stream as Stream
import Language.KansasLava.Signal

import Data.Sized.Matrix as M
import Data.Map as Map
import Data.Word
import Control.Applicative
import Data.Maybe  as Maybe
import Data.Sized.Unsigned (Unsigned,U1)
import Language.KansasLava.Deep
import Language.KansasLava.Radix

type Enabled a = Maybe a

type Pipe a d = Enabled (a,d)

type Memory clk a d = CSeq clk a -> CSeq clk d

enabledRegister :: forall a clk. (Rep a, Clock clk) =>  Comb a -> CSeq clk (Enabled a) -> CSeq clk a
enabledRegister c inp = res
   where
	(en,v) = unpack inp
	res    = register c (mux2 en (v,res))

-- | Turns a list of maybe values into enabled values.
toEnabledSeq :: forall a . (Rep a) => [Maybe a] -> Seq (Enabled a)
toEnabledSeq xs = toSeqX [ optX (Just x)
			 | x <- xs
			 ]

memoryToPipe ::  forall a d clk . (Rep a, Rep d, Clock clk) =>  CSeq clk (Enabled a) -> Memory clk a d -> CSeq clk (Pipe a d)
memoryToPipe enA mem = pack (delay en,pack (delay a,mem a))
   where
	(en,a) = unpack enA


pipeToMemory :: forall a d clk1 clk2. (Size a, Clock clk1, Rep a, Rep d)
	=> CSeq clk1 (Pipe a d)
	-> Memory clk1 a d
pipeToMemory pipe addr2 = unpack (pipeToMemory' pipe) addr2 

-- Later, we will have a two clock version.

-- Does not work for two clocks, *YET*
pipeToMemory' :: forall a d clk1 clk2. (Clock clk1, Size a, Rep a, Rep d)
	=> CSeq clk1 (Pipe a d)
	-> CSeq clk1 (a -> d)
pipeToMemory' pipe = res
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
			       Just a' -> case lookupRadix a' m of
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
	    $ emptyRadix :~ Stream.fromList
		[ case u of
		    Nothing           -> emptyRadix	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) ->
			case getValidRepValue (toRep (optX (Just a))) of
			  Just bs -> ((insertRadix $! bs) $! d) $! m
		| u <- Stream.toList updates
		| m <- Stream.toList mem
		]

    	entity :: MuE E
    	entity =
		Entity (Prim "BRAM")
			[ ("o0",bitTypeOf res)]
			[ ("env",ClkDomTy, unD $ (clock :: D clk1))
			, ("wEn",bitTypeOf wEn,unD $ seqDriver wEn)
			, ("wAddr",bitTypeOf addr,unD $ seqDriver addr)
			, ("wData",bitTypeOf dat,unD $ seqDriver dat)
--			, ("rAddr",bitTypeOf addr2,unD $ seqDriver addr2)
			]
		[]


fullEnabled :: forall a b sig . (Signal sig, Show a, Rep a, Show b, Rep b)
	   => sig a -> (a -> Maybe b) -> sig (Enabled b)
fullEnabled seq f = pack (funMap (return . isJust . f) seq :: sig Bool,funMap f seq :: sig b)

enabledToPipe :: (Rep x, Rep y, Rep z, Signal sig) => (Comb x -> Comb (y,z)) -> sig (Enabled x) -> sig (Pipe y z)
enabledToPipe f se = pack (en, (liftS1 f x))
   where (en,x) = unpack se

-- This is lifting *Comb* because Comb is stateless, and the 'en' Bool being passed on assumes no history,
-- in the 'a -> b' function.
mapEnabled :: (Rep a, Rep b, Signal sig) => (Comb a -> Comb b) -> sig (Enabled a) -> sig (Enabled b)
mapEnabled f en = pack (en_bool,liftS1 f en_val)
   where (en_bool,en_val) = unpack en

zipEnabled :: (Rep a, Rep b, Rep c, Signal sig) => (Comb a -> Comb b -> Comb c) -> sig (Enabled a) -> sig (Enabled b) -> sig (Enabled c)
zipEnabled f en1 en2 = packY (en_bool1 `phi` en_bool2,liftS2 f en_val1 en_val2)
   where (en_bool1,en_val1) = unpackY en1
	 (en_bool2,en_val2) = unpackY en2


packY :: forall a sig . (Rep a, Signal sig) => (sig Bool, sig a) -> sig (Maybe a)
packY (a,b) = {-# SCC "pack(MaybeTT)" #-}
			liftS2 (\ (Comb a ae) (Comb b be) ->
				    Comb (case unX a of
					    Nothing -> optX Nothing
					    Just False -> optX (Just Nothing)
					    Just True ->
						case unX b of
						   Just v -> optX (Just (Just v))
							-- This last one is strange.
						   Nothing -> optX (Just Nothing)
					 )
					 (entity2 (Name "Lava" "pair") ae be)
			     ) a b
unpackY :: forall a sig . (Rep a, Signal sig) => sig (Maybe a) -> (sig Bool, sig a)
unpackY ma = {-# SCC "unpack(MaybeY)" #-}
		   ((,) $!
		    ( {-# SCC "unpack(MaybeY1)" #-}liftS1 ({-# SCC "a_1" #-} (\ (Comb a abe) -> {-# SCC "unpack(Maybe_B)" #-}
						Comb (case unX a of
							Nothing -> {-# SCC "unpack(Maybe,1)" #-}optX Nothing
							Just Nothing -> {-# SCC "unpack(Maybe,2)" #-}optX (Just False)
							Just (Just _) -> {-# SCC "unpack(Maybe,3)" #-}optX (Just True)
						     )
						     ({-# SCC"a_2" #-}(entity1 (Name "Lava" "fst") abe))
			      )) ma
		    )) $! ( {-# SCC "unpack(MaybeY2)" #-}liftS1 (\ (Comb a abe) -> {-# SCC "unpack(Maybe_a)" #-}
						Comb (case unX a of
							Nothing -> {-# SCC "unpack(Maybe,3)" #-}optX Nothing
							Just Nothing -> {-# SCC "unpack(Maybe,4)" #-}optX Nothing
							Just (Just v) ->{-# SCC "unpack(Maybe,5)" #-} optX (Just v)
						     )
						     (entity1 (Name "Lava" "snd") abe)
			      ) ma
		    )

{-
packX :: (Rep a, Rep b, Signal sig) => (sig a, sig b) -> sig (a,b)
packX ~(a,b) = {-# SCC "pack(,)" #-}
			liftS2 (\ ~(Comb a ae) ~(Comb b be) -> {-# SCC "pack(,)i" #-} Comb (a,b) (entity2 (Name "Lava" "pair") ae be))
			    a b
unpackX :: (Rep a, Rep b, Signal sig) => sig (a,b) -> (sig a, sig b)
unpackX ab = {-# SCC "unpack(,)" #-}
		    ( liftS1 (\ (Comb (~(a,b)) abe) -> Comb a (entity1 (Name "Lava" "fst") abe)) ab
		    , liftS1 (\ (Comb (~(a,b)) abe) -> Comb b (entity1 (Name "Lava" "snd") abe)) ab
		    )
-}

phi :: forall a sig . (Signal sig, Rep a) => sig a -> sig a -> sig a
phi = liftS2 $ \ (Comb a ea) (Comb b eb) ->
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
delayEnabled inp = register disabledS inp

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


-- Used for simulation, because this actually clones the memory to allow this to work, generating lots of LUTs.
memoryToMatrix ::  (Integral a, Size a, Rep a, Rep d) => Memory clk a d -> CSeq clk (Matrix a d)
memoryToMatrix mem = pack (forAll $ \ x -> mem $ pureS x)

shiftRegister :: (Rep d, Integral x, Size x, Clock clk) =>  CSeq clk (Enabled d) -> CSeq clk (Matrix x d)
shiftRegister inp = pack m
  where
	(en,val) = unpack inp
	(m, _)   = scanR fn (val, forAll $ \ _ -> ())
	fn (v,()) = (reg,reg)
		where reg = enabledRegister (undefinedComb) (pack (en,v))


unShiftRegister :: forall x d clk . (Integral x, Size x, Rep d, Clock clk) =>  CSeq clk (Enabled (Matrix x d)) -> CSeq clk (Enabled d)
unShiftRegister inp = r
  where
	en :: CSeq clk Bool
	m :: CSeq clk (Matrix x d)
	(en,m) = unpack inp
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
--  res = rom env inp $ \ a -> ....
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
stepifyStream f (a :~ r) = a :~ (f a `seq` stepifyStream f r)

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

