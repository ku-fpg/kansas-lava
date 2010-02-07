{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
module Language.KansasLava.Protocols where
	
import Language.KansasLava.K
import Language.KansasLava.Entity
import Language.KansasLava.Wire
import Language.KansasLava.Signal
import Language.KansasLava.Utils
import Language.KansasLava.Sequential
import Language.KansasLava.Type

type Enabled a = (Bool,a)

type Pipe a b = Enabled (a,b)

type Memory d a = Signal d -> Signal a

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
	
-- Space leak!
pipeToMemory :: forall a d . (RepWire a, RepWire d) => Signal SysEnv -> Signal (Pipe a d) -> Memory a d
pipeToMemory sysEnv pipe addr2 = res
  where
	(clk,rst)  = unpack sysEnv
	(en,pipe') = unpack pipe
	(addr,dat) = unpack pipe'

    	res :: Signal d
    	res = Signal undefined (D $ Port (Var "o0") $ E $ entity)

--	mem :: X a -> X d
--	mem = undefined

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
