{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp  #-}
module Language.KansasLava.Protocols where
	
import Language.KansasLava.K
import Language.KansasLava.Entity
import Language.KansasLava.Wire
import Language.KansasLava.Signal
import Language.KansasLava.Utils
import Language.KansasLava.Sequential
import Language.KansasLava.Type
import Language.KansasLava.Seq as Seq

import Data.Sized.Matrix as M
import Data.Map as Map
import Data.Word
import Control.Applicative

type Enabled a = (Bool,a)

type Pipe a d = Enabled (a,d)

type Memory a d = Signal a -> Signal d

{-
pipeToMem2 :: forall a d . (OpType a, OpType d) => Time -> Pipe a d -> Mem2 a d
pipeToMem2 (Time clk rst) ((en,addr),dat) addr2 = res


-- o0 $ entity2 (op s1 nm) defaultInputs [Var "o0"]  f s1 s2

  where 
    res :: Signal d
    res = Signal undefined (Port (Var "o0") $ E $ entity)

    entity :: Entity BaseTy E
    entity = 
	Entity (Name "Mem" "mem2") 
		[ (Var "o0",bitTypeOf res)]
		[ (Var "clk",bitTypeOf clk,signalDriver clk)
		, (Var "rst",bitTypeOf rst,signalDriver rst)
		, (Var "en",bitTypeOf en,signalDriver en)
		, (Var "addr",bitTypeOf addr,signalDriver addr)
		, (Var "dat",bitTypeOf dat,signalDriver dat)
		, (Var "addr2",bitTypeOf addr2,signalDriver addr2)
		] 
		[]
-}

memoryToPipe ::  (Wire a, Wire d) => Signal (Enabled a) -> Memory a d -> Signal (Pipe a d)
memoryToPipe enA mem = pack (en,pack (a,mem a))
   where
	(en,a) = unpack enA
	
-- Warning, I'm pretty sure this will space leak. Call it a gut feel :-)
pipeToMemory :: forall a d . (Size (WIDTH a), RepWire a, RepWire d) => Signal SysEnv -> Signal (Pipe a d) -> Memory a d
pipeToMemory sysEnv pipe addr2 = res
  where
	(clk,rst)  = unpack sysEnv
	(en,pipe') = unpack pipe
	(addr,dat) = unpack pipe'

    	res :: Signal d
    	res = Signal shallowRes (D $ Port (Var "o0") $ E $ entity)


	shallowRes :: Seq (X d)
	shallowRes = pure (\ m a2 -> case unX a2 :: Maybe a of
				       Nothing -> optX (Nothing :: Maybe d)
				       Just a' -> case Map.lookup (M.toList $ (fromWireRep a' :: Matrix (WIDTH a) Bool)) m of
						    Nothing -> optX (Nothing :: Maybe d)
						    Just v -> optX (Just v)
			  ) <*> mem
			    <*> signalValue addr2

	-- This could have more fidelity, and allow you
	-- to say only a single location is undefined
	updates :: Seq (Maybe (Maybe (a,d)))
	updates = pure (\ e a b -> 
			   do en'   <- unX e :: Maybe Bool
			      if not en' 
				     then Nothing
				     else do 
			      		addr' <- unX a :: Maybe a
			      		dat'  <- unX b :: Maybe d
			      		return $ Just (addr',dat')
		       ) <*> signalValue en
			 <*> signalValue addr
			 <*> signalValue dat

	-- mem
	mem :: Seq (Map [Bool] d)
	mem = Map.empty :~ Map.empty :~ Seq.fromList
		[ case u of
		    Nothing           -> Map.empty	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) -> Map.insert (M.toList $ (fromWireRep a :: Matrix (WIDTH a) Bool)) d m
		| u <- Seq.toList updates 
		| m <- Prelude.tail (Seq.toList mem)
		]

    	entity :: Entity BaseTy E
    	entity = 
		Entity (Name "Memory" "memory") 
			[ (Var "o0",bitTypeOf res)]
			[ (Var "clk",bitTypeOf clk,unD $ signalDriver clk)
			, (Var "rst",bitTypeOf rst,unD $ signalDriver rst)
			, (Var "en",bitTypeOf en,unD $ signalDriver en)
			, (Var "addr",bitTypeOf addr,unD $ signalDriver addr)
			, (Var "dat",bitTypeOf dat,unD $ signalDriver dat)
			, (Var "addr2",bitTypeOf addr2,unD $ signalDriver addr2)
			] 
		[]

-- How to test
pip :: Signal (Pipe Word8 Int)
pip = shallowSignal $ Seq.fromList $ Prelude.map (optX :: Maybe (Enabled (Word8,Int)) -> X (Enabled (Word8,Int)))
	 [ return (True,(0,99))
	 , return (True,(1,99))
	 , return (True,(2,99))
	 , return (True,(3,99))
	 , Nothing
	 , return (True,(0,100))
	 , return (True,(1,99))
	 , return (True,(0,99))
	 ] ++ repeat (optX (Nothing :: Maybe (Pipe Word8 Int)))

ff :: Signal Word8
ff = shallowSignal $ Seq.fromList $ repeat (optX (Just 0 :: Maybe Word8) :: X Word8)

r :: Memory Word8 Int
r = pipeToMemory sysEnv pip