{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp  #-}
module Language.KansasLava.Protocols where
	
import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Entity
import Language.KansasLava.Wire
import Language.KansasLava.Utils
import Language.KansasLava.Sequential
import Language.KansasLava.Type
import Language.KansasLava.Stream as Stream

import Data.Sized.Matrix as M
import Data.Map as Map
import Data.Word
import Control.Applicative

type Enabled a = (Bool,a)

type Pipe a d = Enabled (a,d)

type Memory a d = Seq a -> Seq d

{-
pipeToMem2 :: forall a d . (OpType a, OpType d) => Time -> Pipe a d -> Mem2 a d
pipeToMem2 (Time clk rst) ((en,addr),dat) addr2 = res


-- o0 $ entity2 (op s1 nm) defaultInputs [Var "o0"]  f s1 s2

  where 
    res :: Seq d
    res = Seq undefined (Port (Var "o0") $ E $ entity)

    entity :: Entity BaseTy E
    entity = 
	Entity (Name "Mem" "mem2") 
		[ (Var "o0",bitTypeOf res)]
		[ (Var "clk",bitTypeOf clk,seqDriver clk)
		, (Var "rst",bitTypeOf rst,seqDriver rst)
		, (Var "en",bitTypeOf en,seqDriver en)
		, (Var "addr",bitTypeOf addr,seqDriver addr)
		, (Var "dat",bitTypeOf dat,seqDriver dat)
		, (Var "addr2",bitTypeOf addr2,seqDriver addr2)
		] 
		[]
-}

memoryToPipe ::  (Wire a, Wire d) => Seq (Enabled a) -> Memory a d -> Seq (Pipe a d)
memoryToPipe enA mem = pack (en,pack (a,mem a))
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


	shallowRes :: Stream (X d)
	shallowRes = pure (\ m a2 -> case unX a2 :: Maybe a of
				       Nothing -> optX (Nothing :: Maybe d)
				       Just a' -> case Map.lookup (M.toList $ (fromWireRep a' :: Matrix (WIDTH a) Bool)) m of
						    Nothing -> optX (Nothing :: Maybe d)
						    Just v -> optX (Just v)
			  ) <*> mem
			    <*> seqValue addr2

	-- This could have more fidelity, and allow you
	-- to say only a single location is undefined
	updates :: Stream (Maybe (Maybe (a,d)))
	updates = pure (\ e a b -> 
			   do en'   <- unX e :: Maybe Bool
			      if not en' 
				     then Nothing
				     else do 
			      		addr' <- unX a :: Maybe a
			      		dat'  <- unX b :: Maybe d
			      		return $ Just (addr',dat')
		       ) <*> seqValue en
			 <*> seqValue addr
			 <*> seqValue dat

	-- mem
	mem :: Stream (Map [Bool] d)
	mem = Map.empty :~ Map.empty :~ Stream.fromList
		[ case u of
		    Nothing           -> Map.empty	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) -> Map.insert (M.toList $ (fromWireRep a :: Matrix (WIDTH a) Bool)) d m
		| u <- Stream.toList updates 
		| m <- Prelude.tail (Stream.toList mem)
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
