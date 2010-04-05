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

type Enabled a = Maybe a

type Pipe a d = Enabled (a,d)

type Memory a d = Seq a -> Seq d

enabledRegister :: forall a. (Wire a) => Rst -> Comb a -> Seq (Enabled a) -> Seq a
enabledRegister sysEnv c inp = res
   where 
	(en,v) = unpack inp
	res    = register sysEnv c (mux2 en (v,res))

-- | Turns a list of maybe values into enabled values.
toEnabledSeq :: forall a . (Wire a) => [Maybe a] -> Seq (Enabled a)
toEnabledSeq xs = toSeqX [ optX (Just x)
			 | x <- xs
			 ]

memoryToPipe ::  forall a d . (Wire a, Wire d) => Seq (Enabled a) -> Memory a d -> Seq (Pipe a d)
memoryToPipe enA mem = pack (delay en,pack ((delay a),mem a))
   where
	(en,a) = unpack enA

-- Warning, I'm pretty sure this will space leak. Call it a gut feel :-)
pipeToMemory :: forall a d . (Size (WIDTH a), RepWire a, RepWire d) => Rst -> Seq (Pipe a d) -> Memory a d
pipeToMemory rst pipe addr2 = res
  where
	(en,pipe') = unpack pipe
	(addr,dat) = unpack pipe'

    	res :: Seq d
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
			[ (Var "clk",ClkTy,Pad (Var "clk"))
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

enabledToPipe :: (Wire x, Wire y, Wire z, Signal sig) => (sig x -> sig (y,z)) -> sig (Enabled x) -> sig (Pipe y z)
enabledToPipe f se = pack (en, (f x))
   where (en,x) = unpack se

mapEnabled :: (Wire a, Wire b, Signal sig) => (Comb a -> Comb b) -> sig (Enabled a) -> sig (Enabled b)
mapEnabled f en = pack (en_bool,liftS1 f en_val)
   where (en_bool,en_val) = unpack en

zipEnabled :: (Wire a, Wire b, Wire c, Signal sig) => (Comb a -> Comb b -> Comb c) -> sig (Enabled a) -> sig (Enabled b) -> sig (Enabled c)
zipEnabled f en1 en2 = pack (en_bool1 `phi` en_bool2,liftS2 f en_val1 en_val2)
   where (en_bool1,en_val1) = unpack en1
	 (en_bool2,en_val2) = unpack en2

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
memoryToMatrix ::  (Wire a, Integral a, Size a, RepWire a, Wire d) => Memory a d -> Seq (Matrix a d)
memoryToMatrix mem = pack (forAll $ \ x -> mem $ pureS x)

shiftRegister :: (Wire d, Integral x, Size x) => Rst -> Seq (Enabled d) -> Seq (Matrix x d)
shiftRegister sysEnv inp = pack m
  where
	(en,val) = unpack inp
	(m, _)   = scanR fn (val, forAll $ \ _ -> ())
	fn (v,()) = (reg,reg)
		where reg = enabledRegister sysEnv (errorComb) (pack (en,v))


unShiftRegister :: forall x d . (Integral x, Size x, Wire d) => Seq (Enabled (Matrix x d)) -> Seq (Enabled d)
unShiftRegister inp = r
  where
	en :: Seq Bool
	m :: Seq (Matrix x d)
	(en,m) = unpack inp
	r :: Seq (Enabled d)
	(_, r) = scanR fn (pack (low,errorSeq), unpack m)

	fn (carry,inp) = ((),reg)
	  where (en',mv) = unpack carry
		reg = (delay 
		 	      (mux2 en ( pack (high,inp),
				         pack (en',mv)
			)))



-- Should really be in Utils (but needs Protocols!)
-- Assumes input is not too fast; double buffering would fix this.

runBlock :: forall a b x y . (RepWire x, Bounded x, Integral y, Integral x, Size x, Size y, Wire a, Wire b) 
	 => Rst 
	 -> (Comb (Matrix x a) -> Comb (Matrix y b)) 
	 -> Seq (Enabled a) 
	 -> Seq (Enabled b)
runBlock rst fn inp = unShiftRegister 
		       $ addSync
		       $ liftS1 fn
		       $ shiftRegister rst inp
   where
	addSync a = pack (syncGo,a)

	(en,_) = unpack inp

	-- counting n things put into the queue
	syncGo :: Seq Bool
	syncGo = delay (pureS maxBound .==. syncCounter)

	syncCounter :: Seq x
	syncCounter = counter rst en
		
counter :: (RepWire x, Num x) => Rst -> Seq Bool -> Seq x
counter rst inc = res
   where res = register rst 0 (res + mux2 inc (1,0))

