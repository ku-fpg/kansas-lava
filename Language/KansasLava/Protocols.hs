{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Language.KansasLava.Protocols where

import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Entity
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
import Language.KansasLava.Entity.Utils

type Enabled a = Maybe a

type Pipe a d = Enabled (a,d)

type Memory clk a d = CSeq clk a -> CSeq clk d

enabledRegister :: forall a clk. (Rep a) => Env clk -> Comb a -> CSeq clk (Enabled a) -> CSeq clk a
enabledRegister sysEnv c inp = res
   where 
	(en,v) = unpack inp
	res    = register sysEnv c (mux2 en (v,res))

-- | Turns a list of maybe values into enabled values.
toEnabledSeq :: forall a . (Rep a) => [Maybe a] -> Seq (Enabled a)
toEnabledSeq xs = toSeqX [ optX (Just x)
			 | x <- xs
			 ]

memoryToPipe ::  forall a d clk . (Rep a, Rep d) => Env clk -> CSeq clk (Enabled a) -> Memory clk a d -> CSeq clk (Pipe a d)
memoryToPipe clk enA mem = pack (delay clk en,pack (delay clk a,mem a))
   where
	(en,a) = unpack enA


-- Does not work for two clocks, *YET*
pipeToMemory :: forall a d clk1 clk2. (Rep a, Rep d) 
	=> Env clk1
	-> Env clk2
	-> CSeq clk1 (Pipe a d) 
	-> Memory clk2 a d
pipeToMemory env1@(Env (Clock _ clk) rst clk_en) _env2 pipe addr2 = res
  where
	(wEn,pipe') = unpack pipe
	(addr,dat) = unpack pipe'

    	res :: CSeq clk2 d
    	res = Seq shallowRes (D $ Port ("o0") $ E $ entity)

	shallowRes :: Stream (X d)
	shallowRes = pure (\ m a2 -> case getValidRepValue (toRep (witness :: a) a2) of
				       Nothing -> optX (Nothing :: Maybe d)
				       Just a' -> case lookupMEM a' m of
						    Nothing -> optX (Nothing :: Maybe d)
						    Just v -> optX (Just v)
			  ) <*> (emptyMEM :~ mem)
			    <*> (optX (Nothing :: Maybe a) :~ seqValue addr2)

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
			   do en'   <- unX e :: Maybe Bool
			      if not en'
				     then return Nothing
				     else do
			      		addr' <- unX a :: Maybe a
			      		dat'  <- unX b :: Maybe d
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
	mem :: Stream (Mem d)
	mem = stepifyStream (\ a -> a `seq` ()) 
	    $ emptyMEM :~ Stream.fromList
		[ case u of
		    Nothing           -> emptyMEM	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) -> 
			case getValidRepValue (toRep (witness :: a) (optX (Just a) :: X a)) of 
			  Just bs -> ((insertMEM $! bs) $! d) $! m
		| u <- Stream.toList updates
		| m <- Stream.toList mem
		]

    	entity :: MuE E
    	entity =
		Entity (Name "Memory" "BRAM")
			[ ("o0",bitTypeOf res)]
			[ ("clk",ClkTy,unD $ clk)
			, ("rst",bitTypeOf rst,unD $ seqDriver rst)
			, ("wEn",bitTypeOf wEn,unD $ seqDriver wEn)
			, ("en",bitTypeOf clk_en,unD $ seqDriver clk_en)
			, ("wAddr",bitTypeOf addr,unD $ seqDriver addr)
			, ("wData",bitTypeOf dat,unD $ seqDriver dat)
			, ("rAddr",bitTypeOf addr2,unD $ seqDriver addr2)
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
				    Comb (case unX (a :: X Bool) :: Maybe Bool of
					    Nothing -> optX (Nothing :: Maybe (Maybe a))
					    Just False -> optX (Just Nothing :: Maybe (Maybe a))
					    Just True -> 
						case unX (b :: X a) :: Maybe a of
						   Just v -> optX (Just (Just v) :: Maybe (Maybe a))
							-- This last one is strange.
						   Nothing -> optX (Just Nothing :: Maybe (Maybe a))
					 )
					 (entity2 (Name "Lava" "pair") ae be)
			     ) a b
unpackY :: forall a sig . (Rep a, Signal sig) => sig (Maybe a) -> (sig Bool, sig a) 
unpackY ma = {-# SCC "unpack(MaybeY)" #-}
		   ((,) $! 
		    ( {-# SCC "unpack(MaybeY1)" #-}liftS1 ({-# SCC "a_1" #-} (\ (Comb a abe) -> {-# SCC "unpack(Maybe_B)" #-}
						Comb (case unX (a :: X (Maybe a)) :: Maybe (Maybe a) of
							Nothing -> {-# SCC "unpack(Maybe,1)" #-}optX (Nothing :: Maybe Bool)
							Just Nothing -> {-# SCC "unpack(Maybe,2)" #-}optX (Just False :: Maybe Bool)
							Just (Just _) -> {-# SCC "unpack(Maybe,3)" #-}optX (Just True :: Maybe Bool)
						     )
						     ({-# SCC"a_2" #-}(entity1 (Name "Lava" "fst") abe))
			      )) ma
		    )) $! ( {-# SCC "unpack(MaybeY2)" #-}liftS1 (\ (Comb a abe) -> {-# SCC "unpack(Maybe_a)" #-}
						Comb (case unX (a :: X (Maybe a)) :: Maybe (Maybe a) of
							Nothing -> {-# SCC "unpack(Maybe,3)" #-}optX (Nothing :: Maybe a)
							Just Nothing -> {-# SCC "unpack(Maybe,4)" #-}optX (Nothing :: Maybe a)
							Just (Just v) ->{-# SCC "unpack(Maybe,5)" #-} optX (Just v :: Maybe a)
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
        Comb (if toRep (witness :: a) a == toRep (witness :: a) b
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
delayEnabled :: (Rep a) => Env clk -> CSeq clk (Enabled a) -> CSeq clk (Enabled a)
delayEnabled env inp = register env disabledS inp

{-
-- to move into a counters module
-- Count the number of ticks on a signal. Notice that we start at zero (no ticks),
-- and bump the counter at each sighting.
countTicks :: forall clk x . (Rep x) => x -> (Comb x -> Comb x) -> Env clk -> CSeq clk Bool -> CSeq clk (Enabled x)
countTicks init succ sysEnv enable = packEnabled enable ctr
   where
        ctr :: CSeq clk x
        ctr = register sysEnv (pureS init) val

        val :: CSeq clk x
        val = mux2 enable (liftS1 succ ctr,ctr)


-- compare with my previous value
cmp :: (Wire a) => Env clk -> (Comb a -> Comb a -> Comb b) -> CSeq clk a -> CSeq clk b
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
	                in pack (mux2 en1 (en1,en2), mux2 en1 (v1,v2))


-- Used for simulation, because this actually clones the memory to allow this to work, generating lots of LUTs.
memoryToMatrix ::  (Integral a, Size a, Rep a, Rep d) => Memory clk a d -> CSeq clk (Matrix a d)
memoryToMatrix mem = pack (forAll $ \ x -> mem $ pureS x)

shiftRegister :: (Rep d, Integral x, Size x) => Env clk -> CSeq clk (Enabled d) -> CSeq clk (Matrix x d)
shiftRegister sysEnv inp = pack m
  where
	(en,val) = unpack inp
	(m, _)   = scanR fn (val, forAll $ \ _ -> ())
	fn (v,()) = (reg,reg)
		where reg = enabledRegister sysEnv (undefinedComb) (pack (en,v))


unShiftRegister :: forall x d clk . (Integral x, Size x, Rep d) => Env clk -> CSeq clk (Enabled (Matrix x d)) -> CSeq clk (Enabled d)
unShiftRegister env inp = r
  where
	en :: CSeq clk Bool
	m :: CSeq clk (Matrix x d)
	(en,m) = unpack inp
	r :: CSeq clk (Enabled d)
	(_, r) = scanR fn (pack (low,undefinedSeq), unpack m)

	fn (carry,inp) = ((),reg)
	  where (en',mv) = unpack carry
		reg = (delay env 
		 	      (mux2 en ( pack (high,inp),
				         pack (en',mv)
			)))



-- Should really be in Utils (but needs Protocols!)
-- Assumes input is not too fast; double buffering would fix this.

runBlock :: forall a b x y clk . (Rep x, Bounded x, Integral y, Integral x, Size x, Size y, Rep a, Rep b) 
	 => Env clk 
	 -> (Comb (Matrix x a) -> Comb (Matrix y b)) 
	 -> CSeq clk (Enabled a) 
	 -> CSeq clk (Enabled b)
runBlock env fn inp = unShiftRegister env
		       $ addSync
		       $ liftS1 fn
		       $ shiftRegister env inp
   where
	addSync a = pack (syncGo,a)

	(en,_) = unpack inp

	-- counting n things put into the queue
	syncGo :: CSeq clk Bool
	syncGo = delay env (pureS maxBound .==. syncCounter)

	syncCounter :: CSeq clk x
	syncCounter = counter env en
		
-- If the Seq Bool is enabled, then we want to generate the
-- next number in the sequence, in the *next* cycle.

-- TODO: remove, its confusing
counter :: (Rep x, Num x) => Env clk -> CSeq clk Bool -> CSeq clk x
counter rst inc = res
   where res = register rst 0 (res + mux2 inc (1,0))



--------------------------------------------------


-- The order here has the function *last*, because it allows
-- for a idiomatic way of writing things
--
--  res = rom env inp $ \ a -> .... 
--
rom :: (Rep a, Rep b) => Env clk -> CSeq clk a -> (a -> Maybe b) -> CSeq clk b
rom env inp fn = delay env $ funMap fn inp

---------------------------------

-- A latch that can cross clock domains
latch :: forall clk1 clk2 a. (Rep a) => Env clk1 -> Env clk2 -> CSeq clk1 (Enabled a) -> CSeq clk2 a
latch env1 env2 inp = pipeToMemory env1 env2 wr (pureS ())
    where 
	wr :: CSeq clk1 (Pipe () a)
	wr = enabledToPipe (\ a -> pack (pureS (),a)) inp

----------------------

class Stepify a where
  stepify :: a -> a

--class Rep a => Eval a where
eval :: forall a . (Rep a) => a -> ()
eval a = count $ unRepValue $ toRep (witness :: a) (optX (Just a) :: X a)
  where count (WireVal True:rest) = count rest
	count (WireVal False:rest) = count rest
	count (WireUnknown:rest) = count rest
	count [] = ()

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


-- LATER: GADTs
data Mem a 
  = Res !a
  | NoRes
  | Choose !(Mem a) !(Mem a)
	deriving Show

emptyMEM :: Mem a 
emptyMEM = NoRes

insertMEM :: [Bool] -> a -> Mem a -> Mem a

insertMEM []    y (Res _) = Res $! y
insertMEM []    y NoRes   = Res $! y
insertMEM []    y (Choose _ _) = error "inserting with short key"

insertMEM (x:a) y NoRes   = insertMEM (x:a) y expanded
insertMEM (x:a) y (Res _) = error "inserting with to long a key"
insertMEM (x:a) y (Choose l r) 
	| x == True 	  = Choose (insertMEM a y l) r
	| x == False	  = Choose l (insertMEM a y r)

-- Would this be lifted
expanded = Choose NoRes NoRes

lookupMEM :: [Bool] -> Mem a -> Maybe a
lookupMEM [] (Res v) = Just v
lookupMEM [] NoRes   = Nothing
lookupMEM [] _       = error "lookup error with short key"

lookupMEM (x:a) (Res _) = error "lookup error with long key"
lookupMEM (x:a) NoRes   = Nothing
lookupMEM (True:a) (Choose l r) = lookupMEM a l
lookupMEM (False:a) (Choose l r) = lookupMEM a r


