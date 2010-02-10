{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
module Language.KansasLava.Sequential where

import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Stream as S
import Language.KansasLava.Wire
import Language.KansasLava.Utils
import Data.Sized.Ix


type SysEnv = (Clk,Rst)

type Clk = Int 	-- for now

type Rst = Bool

{-	
instance Wire Clk where
	type X Clk = Integer		-- no way of hiding this

instance RepWire Clk where
	type WIDTH Clk = X1

instance Wire Rst where
	type X Rst = Maybe Bool

instance RepWire Rst where
	type WIDTH Rst = X1
-}

sysEnv :: Seq SysEnv 
sysEnv = shallowSeq $ S.fromList $ zip (map (optX . Just :: Int -> X Int) [0..] :: [X Int])
 					    (map (optX  . Just) ([True] ++ repeat False))

latch :: forall a . (Wire a) => Seq a -> Seq a
latch dat@(Seq a ea) = res

  where
	res = Seq (optX (Nothing :: Maybe a) :~ a) (D $ Port (Var "o0") $ E $ entity)
	
	entity :: Entity BaseTy E
    	entity = 
		Entity (Name "Memory" "latch") 
			[ (Var "o0",bitTypeOf res)]
			[ (Var "i0",bitTypeOf dat,unD $ seqDriver dat)
			] 
		[]

delay :: (Wire a) => Seq SysEnv -> Comb a -> Seq a -> Seq a
delay sysEnv def line = mux2 en (liftS0 def,latch line)
   where
	(_,en) = unpack sysEnv


-- hack
ans = delay sysEnv 99 ((shallowSeq $ S.fromList $ map (optX . Just) [(1::Int)..100]) :: Seq Int)


--import Language.KansasLava.Applicative

--delay :: (Sig sig) => sig a -> sig a -> sig a
-- delay = error "delay!"

-- To revisit: Should this be a -> S a -> S a ??
{-
latch :: forall a. (OpType a) => Time -> Seq a -> Seq a
latch tm sig = delay tm def sig
  where def = initVal


delay :: forall a. (OpType a) => Time -> Seq a -> Seq a -> Seq a
delay ~(Time ~(Seq _ tm_w) ~(Seq r r_w)) ~(Seq d def) ~(Seq rest w)
        = Seq (shallowDelay r d rest)
        $ Port (Var "o0")
        $ E
        $ Entity (Name "Lava" "delay") [(Var "o0",aTy)]
            [(Var "clk",clkTy,tm_w), (Var "rst", rstTy,r_w), (Var "init",aTy,def),(Var "i",aTy,w)] []
  where aTy = tyRep (error "delay/aTy" ::  a)
        clkTy = tyRep (error "delay/clk" :: Clk)
        rstTy = tyRep (error "delay/rst" :: Rst)


shallowDelay :: Seq Rst -> Seq a -> Seq a -> Seq a
shallowDelay sT sInit input =
	seqMux (fmap (\ (Rst reset) -> reset) reset_delayed)
	       sInit
	       input_delayed
   where
	input_delayed = Nothing :~ input
	reset_delayed = sT -- Just (Rst False) :~ sT


data Time = Time
		(Seq Clk)		-- threading clock
		(Seq Rst)		-- threading reset

newtype Clk = Clk Integer
newtype Rst = Rst Bool

instance Show Time where
	-- show (Time t r) = show (pure (,) <*> t <*> r)
        show (Time (Seq t_s _) (Seq r_s _)) = show $ zipWith' (,) t_s r_s

{-
instance REIFY Time where
--	capture p (a,b) = capture (p `w` 1) a ++ capture (p `w` 2) b
	create     = Time <$> create <*> create

	-- create = Time <$> named "clk" <*> named "rst"

--	capture'' (a,b) = (++) <$> capture'' a <*> capture'' b
        capture'' = error "No method nor default method for `capture''' in the instance declaration for `REIFY Time'"
-}

instance Show Clk where
	show (Clk n) = show n

instance OpType Clk
  where op _ nm = Name "Time" nm
	bitTypeOf _ = ClkTy
	initVal = error "can not use a clock as an init value"

instance Show Rst where
	show (Rst n) = show n

instance OpType Rst
  where op _ nm = Name "Time" nm
	bitTypeOf _ = RstTy
	initVal = error "can not use a reset as an init value"



-- newtype Clk = Clk Integer	-- always running

clk :: Time -> Seq Integer
clk (Time _ _) = error "Sequential.clk" -- fmap (\ (Clk n) -> n) c

rst :: Time -> Seq Bool
rst (Time _ _) = error "Sequential.rst"  -- fmap (\ (Rst r') -> r') r

{-
time :: Time -> Seq Integer		-- simluation only
time (Time t) = t
-}

-- 'clock' gives 2 cycles of bla, 1 cycle of reset, then counts from 0.
clock :: Time
clock = Time (with "global_clock" $ map Clk [0..])
	     (with "global_reset" $ map Rst (True:(repeat False)))

waitFor :: (OpType a) => Int -> Seq a -> Seq a
waitFor n ~(Seq s _) = Seq (fromList (take n (repeat Nothing) ++ toList s)) (error "bad entity")

-- waitForReset ::

{-

	Time $ Seq clock_times (Port (Var "o0") $ E $ Entity (Name "Lava" "clock") [] [] [[TyVar $ Var "o0",BaseTy T]])
  where clock_times = Nothing :~ Nothing :~ (Seq.fromList $ map Just $ (-1 : [0..]))
-}

{-
reset :: Time
reset = Time $ Seq (pure (-1)) (Port (Var "o0") $ E $ Entity (Name "Lava" "clock") [] [] [[TyVar $ Var "o0",BaseTy T]])

switch :: Int -> Seq a -> Seq a -> Signal a
switch n (Signal s1 _) (Signal s2 _)
  = Signal (pure (\ s1 s2 i -> if i < n then s1 else s2)
		<*> s1
		<*> s2
		<*> (Seq.fromList [ Just i | i <- [0..]])
	   ) undefined
-}

{-
-}

-}