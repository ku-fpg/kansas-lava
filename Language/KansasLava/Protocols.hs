{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp  #-}
module Language.KansasLava.Protocols where

import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Entity
import Language.KansasLava.Wire
import Language.KansasLava.Utils
import Language.KansasLava.Type
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

enabledRegister :: forall a clk. (Wire a) => Env clk -> Comb a -> CSeq clk (Enabled a) -> CSeq clk a
enabledRegister sysEnv c inp = res
   where 
	(en,v) = unpack inp
	res    = register sysEnv c (mux2 en (v,res))

-- | Turns a list of maybe values into enabled values.
toEnabledSeq :: forall a . (Wire a) => [Maybe a] -> Seq (Enabled a)
toEnabledSeq xs = toSeqX [ optX (Just x)
			 | x <- xs
			 ]

memoryToPipe ::  forall a d clk . (Wire a, Wire d) => Env clk -> CSeq clk (Enabled a) -> Memory clk a d -> CSeq clk (Pipe a d)
memoryToPipe clk enA mem = pack (delay clk en,pack (delay clk a,mem a))
   where
	(en,a) = unpack enA


-- Does not work for two clocks, *YET*
pipeToMemory :: forall a d clk1 clk2. (Size (WIDTH a), RepWire a, RepWire d) 
	=> Env clk1
	-> Env clk2
	-> CSeq clk1 (Pipe a d) 
	-> Memory clk2 a d
pipeToMemory env1@(Env (Clock _ clk) rst en) _env2 pipe addr2 = res
  where
	(en,pipe') = unpack pipe
	(addr,dat) = unpack pipe'

    	res :: CSeq clk2 d
    	res = Seq shallowRes (D $ Port (Var "o0") $ E $ entity)
{-
	shallowRes' :: Stream (X (Matrix a d))
	shallowRes' = pure (\ m -> forAll $ \ ix ->
				    case Map.lookup (M.toList $ (fromWireRep ix :: Matrix (WIDTH a) Bool)) m of
						    Nothing -> optX (Nothing :: Maybe d)
						    Just v -> optX (Just v)
			  ) <*> mem
-}
	shallowRes :: Stream (X d)
	shallowRes = pure (\ m a2 -> case unX a2 :: Maybe a of
				       Nothing -> optX (Nothing :: Maybe d)
				       Just a' -> case Map.lookup (M.toList $ (fromWireRep a' :: Matrix (WIDTH a) Bool)) m of
						    Nothing -> optX (Nothing :: Maybe d)
						    Just v -> optX (Just v)
			  ) <*> (Map.empty :~ mem)
			    <*> (optX (Nothing :: Maybe a) :~ seqValue addr2)

	-- This could have more fidelity, and allow you
	-- to say only a single location is undefined
	updates :: Stream (Maybe (Maybe (a,d)))
	updates = pure (\ e a b ->
			   do en'   <- unX e :: Maybe Bool
			      if not en'
				     then return Nothing
				     else do
			      		addr' <- unX a :: Maybe a
			      		dat'  <- unX b :: Maybe d
			      		return $ Just (addr',dat')
		       ) <*> seqValue en
			 <*> seqValue addr
			 <*> seqValue dat

	-- mem
	mem :: Stream (Map [Bool] d)
	mem = Map.empty :~ Stream.fromList
		[ case u of
		    Nothing           -> Map.empty	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) -> Map.insert (M.toList $ (fromWireRep a :: Matrix (WIDTH a) Bool)) d m
		| u <- Stream.toList updates
		| m <- Stream.toList mem
		]

    	entity :: Entity BaseTy E
    	entity =
		Entity (Name "Memory" "BRAM")
			[ (Var "o0",bitTypeOf res)]
			[ (Var "clk",ClkTy,unD $ clk)
			, (Var "rst",bitTypeOf rst,unD $ seqDriver rst)
			, (Var "wEn",bitTypeOf en,unD $ seqDriver en)
			, (Var "wAddr",bitTypeOf addr,unD $ seqDriver addr)
			, (Var "wData",bitTypeOf dat,unD $ seqDriver dat)
			, (Var "rAddr",bitTypeOf addr2,unD $ seqDriver addr2)
			]
		[]


fullEnabled :: forall a b sig . (Signal sig, Show a, RepWire a, Show b, RepWire b)
	   => sig a -> (a -> Maybe b) -> sig (Enabled b)
fullEnabled seq f = pack (funMap (return . isJust . f) seq :: sig Bool,funMap f seq :: sig b)

enabledToPipe :: (Wire x, Wire y, Wire z, Signal sig) => (Comb x -> Comb (y,z)) -> sig (Enabled x) -> sig (Pipe y z)
enabledToPipe f se = pack (en, (liftS1 f x))
   where (en,x) = unpack se

-- This is lifting *Comb* because Comb is stateless, and the 'en' Bool being passed on assumes no history,
-- in the 'a -> b' function.
mapEnabled :: (Wire a, Wire b, Signal sig) => (Comb a -> Comb b) -> sig (Enabled a) -> sig (Enabled b)
mapEnabled f en = pack (en_bool,liftS1 f en_val)
   where (en_bool,en_val) = unpack en

zipEnabled :: (Wire a, Wire b, Wire c, Signal sig) => (Comb a -> Comb b -> Comb c) -> sig (Enabled a) -> sig (Enabled b) -> sig (Enabled c)
zipEnabled f en1 en2 = packY (en_bool1 `phi` en_bool2,liftS2 f en_val1 en_val2)
   where (en_bool1,en_val1) = unpackY en1
	 (en_bool2,en_val2) = unpackY en2
	

packY :: forall a sig . (Wire a, Signal sig) => (sig Bool, sig a) -> sig (Maybe a)
packY ~(a,b) = {-# SCC "pack(Maybe)" #-}
			liftS2 (\ ~(Comb a ae) ~(Comb b be) ->
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
unpackY :: forall a sig . (Wire a, Signal sig) => sig (Maybe a) -> (sig Bool, sig a) 
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
packX :: (Wire a, Wire b, Signal sig) => (sig a, sig b) -> sig (a,b)
packX ~(a,b) = {-# SCC "pack(,)" #-}
			liftS2 (\ ~(Comb a ae) ~(Comb b be) -> {-# SCC "pack(,)i" #-} Comb (a,b) (entity2 (Name "Lava" "pair") ae be))
			    a b
unpackX :: (Wire a, Wire b, Signal sig) => sig (a,b) -> (sig a, sig b)
unpackX ab = {-# SCC "unpack(,)" #-}
		    ( liftS1 (\ (Comb (~(a,b)) abe) -> Comb a (entity1 (Name "Lava" "fst") abe)) ab
		    , liftS1 (\ (Comb (~(a,b)) abe) -> Comb b (entity1 (Name "Lava" "snd") abe)) ab
		    )
-}

phi :: forall a sig . (Signal sig, RepWire a) => sig a -> sig a -> sig a
phi = liftS2 $ \ (Comb a ea) (Comb b eb) ->
        Comb (optX
		 $ do a' <- unX a :: Maybe a
		      b' <- unX b :: Maybe a
		      if fromWireRep a' == fromWireRep b'
			then return a'
			else fail "phi problem")	-- an internal error, like an assert
		(ea) -- pick one, they are the same
			-- later, consider puting the phi nodes into the deep syntax

mapPacked :: (Pack sig a, Pack sig b) => (Unpacked sig a -> Unpacked sig b) -> sig a -> sig b
mapPacked f = pack . f . unpack

zipPacked :: (Pack sig a, Pack sig b, Pack sig c) => (Unpacked sig a -> Unpacked sig b -> Unpacked sig c) -> sig a -> sig b -> sig c
zipPacked f x y = pack $ f (unpack x) (unpack y)

mapPipe :: (Signal sig, Wire a, Wire b, RepWire x) => (Comb a -> Comb b) -> sig (Pipe x a) -> sig (Pipe x b)
mapPipe f = mapEnabled (mapPacked $ \ (a0,b0) -> (a0,f b0))

-- | only combines pipes when both inputs are enabled, and *assumes* the 
-- x addresses are the same.
zipPipe :: (Signal sig, Wire a, Wire b, Wire c, RepWire x) => (Comb a -> Comb b -> Comb c) -> sig (Pipe x a) -> sig (Pipe x b) -> sig (Pipe x c)
zipPipe f = zipEnabled (zipPacked $ \ (a0,b0) (a1,b1) -> (a0 `phi` a1,f b0 b1))


-- 
joinEnabled :: (Signal sig, Wire a) => sig (Enabled a) -> sig (Enabled a) -> sig (Enabled a)
joinEnabled = liftS2 $ \ e1 e2 -> 
			let (en1,v1) = unpack e1
	 		    (en2,v2) = unpack e2
	                in pack (mux2 en1 (en1,en2), mux2 en1 (v1,v2))


-- Used for simulation, because this actually clones the memory to allow this to work, generating lots of LUTs.
memoryToMatrix ::  (Wire a, Integral a, Size a, RepWire a, Wire d) => Memory clk a d -> CSeq clk (Matrix a d)
memoryToMatrix mem = pack (forAll $ \ x -> mem $ pureS x)

shiftRegister :: (Wire d, Integral x, Size x) => Env clk -> CSeq clk (Enabled d) -> CSeq clk (Matrix x d)
shiftRegister sysEnv inp = pack m
  where
	(en,val) = unpack inp
	(m, _)   = scanR fn (val, forAll $ \ _ -> ())
	fn (v,()) = (reg,reg)
		where reg = enabledRegister sysEnv (errorComb) (pack (en,v))


unShiftRegister :: forall x d clk . (Integral x, Size x, Wire d) => Env clk -> CSeq clk (Enabled (Matrix x d)) -> CSeq clk (Enabled d)
unShiftRegister env inp = r
  where
	en :: CSeq clk Bool
	m :: CSeq clk (Matrix x d)
	(en,m) = unpack inp
	r :: CSeq clk (Enabled d)
	(_, r) = scanR fn (pack (low,errorSeq), unpack m)

	fn (carry,inp) = ((),reg)
	  where (en',mv) = unpack carry
		reg = (delay env 
		 	      (mux2 en ( pack (high,inp),
				         pack (en',mv)
			)))



-- Should really be in Utils (but needs Protocols!)
-- Assumes input is not too fast; double buffering would fix this.

runBlock :: forall a b x y clk . (RepWire x, Bounded x, Integral y, Integral x, Size x, Size y, Wire a, Wire b) 
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
counter :: (RepWire x, Num x) => Env clk -> CSeq clk Bool -> CSeq clk x
counter rst inc = res
   where res = register rst 0 (res + mux2 inc (1,0))



--------------------------------------------------


-- The order here has the function *last*, because it allows
-- for a idiomatic way of writing things
--
--  res = rom env inp $ \ a -> .... 
--
rom :: (RepWire a, RepWire b) => Env clk -> CSeq clk a -> (a -> Maybe b) -> CSeq clk b
rom env inp fn = delay env $ funMap fn inp



