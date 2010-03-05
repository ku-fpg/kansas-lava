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

type Enabled a = (Bool,a)

type Pipe a d = Enabled (a,d)

type Memory a d = Seq a -> Seq d

-- | Turns a list of maybe values into enabled values.
toEnabledSeq :: forall a . (RepWire a) => [Maybe a] -> Seq (Enabled a)
toEnabledSeq xs = toSeqX [ case x of
			    Nothing -> (return False :: X Bool,optX (Nothing :: Maybe a)) :: X (Enabled a)
			    Just v -> (return True, optX (Just v :: Maybe a)) :: X (Enabled a)
			 | x <- xs
			 ]

memoryToPipe ::  (Wire a, Wire d) => Seq (Enabled a) -> Memory a d -> Seq (Pipe a d)
memoryToPipe enA mem = pack (delay (delay en),pack (delay (delay a),mem a))
   where
	(en,a) = unpack enA

-- Warning, I'm pretty sure this will space leak. Call it a gut feel :-)
pipeToMemory :: forall a d . (Size (WIDTH a), RepWire a, RepWire d) => Seq SysEnv -> Seq (Pipe a d) -> Memory a d
pipeToMemory sysEnv pipe addr2 = res
  where
	(clk,rst)  = unpack sysEnv
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
			  ) <*> (Map.empty :~ Map.empty :~ mem)
			    <*> (optX (Nothing :: Maybe a) :~ optX (Nothing :: Maybe a) :~ seqValue addr2)

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
		Entity (Name "Memory" "memory")
			[ (Var "o0",bitTypeOf res)]
			[ (Var "clk",bitTypeOf clk,unD $ seqDriver clk)
			, (Var "rst",bitTypeOf rst,unD $ seqDriver rst)
			, (Var "en",bitTypeOf en,unD $ seqDriver en)
			, (Var "addr",bitTypeOf addr,unD $ seqDriver addr)
			, (Var "dat",bitTypeOf dat,unD $ seqDriver dat)
			, (Var "addr2",bitTypeOf addr2,unD $ seqDriver addr2)
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
			else fail "phi problem")
		(undefined)


mapPacked :: (Pack sig a, Pack sig b) => (Unpacked sig a -> Unpacked sig b) -> sig a -> sig b
mapPacked f = pack . f . unpack


zipPacked :: (Pack sig a, Pack sig b, Pack sig c) => (Unpacked sig a -> Unpacked sig b -> Unpacked sig c) -> sig a -> sig b -> sig c
zipPacked f x y = pack $ f (unpack x) (unpack y)

zipPipe :: (Signal sig, Wire a, Wire b, Wire c, RepWire x) => (Comb a -> Comb b -> Comb c) -> sig (Pipe x a) -> sig (Pipe x b) -> sig (Pipe x c)
zipPipe f = zipEnabled (zipPacked $ \ (a0,b0) (a1,b1) -> (a0 `phi` a1,f b0 b1))


-- Used for simulation, because this actually clones the memory to allow this to work.
memoryToMatrix ::  (Wire a, Integral a, Size a, RepWire a, Wire d) => Memory a d -> Seq (Matrix a d)
memoryToMatrix mem = pack (forAll $ \ x -> mem $ pureS x)

