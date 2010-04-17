{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ParallelListComp, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses  #-}

module Language.KansasLava.Utils where

import Language.KansasLava.Entity as E
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Language.KansasLava.Stream  as S
import Language.KansasLava.Signal
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Language.KansasLava.StdLogicVector as SLV

import Data.Sized.Matrix	as M
import Data.Sized.Unsigned	as U
import Data.Sized.Signed
import Data.Sized.Arith
import qualified Data.Sized.Sampled	as Sam
import Language.KansasLava.Entity.Utils

import Control.Applicative
import Control.Monad
import Data.Word
import Data.Bits
import Data.Ratio
import Debug.Trace

-----------------------------------------------------------------------------------------------

high, low :: Seq Bool
high = pureS True
low  = pureS False

true, false :: Comb Bool
true = pureS True
false = pureS False

-----------------------------------------------------------------------------------------------
and2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
and2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (liftA2 (&&) a b) $ entity2 (Name "Bool" "and2") ae be

or2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
or2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (liftA2 (||) a b) $ entity2 (Name "Bool" "or2") ae be

xor2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
xor2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (liftA2 (/=) a b) $ entity2 (Name "Bool" "xor2") ae be

bitNot :: (Signal sig) => sig Bool -> sig Bool
bitNot = liftS1 $ \ (Comb a ae) -> Comb (liftA (not) a) $ entity1 (Name "Bool" "not") ae

testABit :: forall sig a . (Bits a, Wire a, Signal sig) => sig a -> Int -> sig Bool
testABit x y = liftS1 (\ (Comb a e) -> Comb (optX $ liftA (flip testBit y) (unX a :: Maybe a)) $ error "test a bit") x


isPositive :: forall sig ix . (Signal sig, Size ix, Enum ix, Integral ix, Bits (sig (Signed ix))) => sig (Signed ix) -> sig Bool
isPositive a = bitNot $ testABit a  msb
    where msb = bitSize a - 1

-- TODO: maCombe over Signal
(.!.) :: (Size x, Wire a, Wire x) => Comb (Matrix x a) -> Comb x -> Comb a
(.!.) = fun2 "!" (!)

-----------------------------------------------------------------------------------------------

instance (Show a, RepWire a, Num a) => Num (Comb a) where
    s1 + s2 = fun2 "+" (+) s1 s2
    s1 - s2 = fun2 "-" (-) s1 s2
    s1 * s2 = fun2 "*" (*) s1 s2
    negate s = fun1 "negate" (negate) s
    abs s    = fun1 "abs"    (abs)    s
    signum s = fun1 "signum" (signum) s
    fromInteger n = pureS (fromInteger n)

instance (Show a, RepWire a, Num a) => Num (Seq a) where
    (+) = liftS2 (+)
    (-) = liftS2 (-)
    (*) = liftS2 (*)
    negate = liftS1 negate
    abs = liftS1 abs
    signum = liftS1 signum
    fromInteger n = pureS (fromInteger n)

instance (Show a, Bits a, RepWire a)
	=> Bits (Comb a) where
    s1 .&. s2 = fun2 ".&." (.&.) s1 s2
    s1 .|. s2 = fun2 ".|." (.|.) s1 s2
    s1 `xor` s2 = fun2 "xor" (xor) s1 s2
    s1 `shift` n = fun2 "shift" (shift) s1 (fromIntegral n)
    s1 `rotate` n = fun2 "rotate" (rotate) s1 (fromIntegral n)
    complement s = fun1 "complement" (complement) s
    bitSize s                       = baseTypeLength (bitTypeOf s)
    isSigned s                      = baseTypeIsSigned (bitTypeOf s)

instance (Show a, Bits a, RepWire a)
	=> Bits (Seq a) where
    (.&.)   = liftS2 (.&.)
    (.|.)  = liftS2 (.|.)
    xor    = liftS2 (xor)
    shift s n = liftS1 (flip shift n) s
    rotate s n = liftS1 (flip rotate n) s
    complement = liftS1 complement
    bitSize s                       = baseTypeLength (bitTypeOf s)
    isSigned s                      = baseTypeIsSigned (bitTypeOf s)

instance (Eq a, Show a, Fractional a, RepWire a) => Fractional (Comb a) where
    s1 / s2 = fun2 "/" (/) s1 s2
    recip s1 = fun1 "recip" (recip) s1
    -- This should just fold down to the raw bits.
    fromRational r = constComb (fromRational r :: a)

instance (Eq a, Show a, Fractional a, RepWire a) => Fractional (Seq a) where
    (/) = liftS2 (/)
    recip = liftS1 recip
    fromRational = liftS0 . fromRational

-----------------------------------------------------------------------------------------------
-- Matrix ops

mapToBoolMatrix :: forall sig w . (Signal sig, Size (WIDTH w), RepWire w) => sig w -> sig (Matrix (WIDTH w) Bool)
mapToBoolMatrix = liftS1 $ \ (Comb a d) -> Comb
	(( optX (liftM fromWireRep ((unX a) :: Maybe w
		    ) :: Maybe (Matrix (WIDTH w) Bool))
	 ) :: X (Matrix (WIDTH w) Bool))
	(entity1 (Name "Lava" "toBoolMatrix") d)

toBoolMatrix :: forall sig w . (Signal sig, Integral (WIDTH w), Size (WIDTH w), RepWire w)
             => sig w -> Matrix (WIDTH w) (sig Bool)
toBoolMatrix = unpack . mapToBoolMatrix

mapFromBoolMatrix :: forall sig w . (Signal sig, Size (WIDTH w), RepWire w) => sig (Matrix (WIDTH w) Bool) -> sig w
mapFromBoolMatrix = liftS1 $ \ (Comb a d) -> Comb
	(case unX (a :: X (Matrix (WIDTH w) Bool)) :: Maybe (Matrix (WIDTH w) Bool) of
	     Nothing -> optX (Nothing :: Maybe w)
	     Just r0 -> optX (toWireRep r0 :: Maybe w)
	)
	(entity1 (Name "Lava" "fromBoolMatrix") d)

fromBoolMatrix :: forall sig w . (Signal sig, Integral (WIDTH w), Size (WIDTH w), RepWire w)
	       => Matrix (WIDTH w) (sig Bool) ->  sig w
fromBoolMatrix = mapFromBoolMatrix . pack

-----------------------------------------------------------------------------------------------
-- Map Ops


-- Assumping that the domain is finite (beacause of RepWire), and *small* (say, < ~256 values).

funMap :: forall sig a b . (Signal sig, RepWire a, RepWire b) => (a -> Maybe b) -> sig a -> sig b
funMap fn = liftS1 $ \ (Comb a (D ae))
			-> Comb (case unX (a :: X a) :: Maybe a of
				   Nothing -> optX (Nothing :: Maybe b) :: X b
				   Just v -> optX (fn v :: Maybe b) :: X b)
				     (D $ Port (Var "o0")
					$ E
					$ Table (Var "o0",tB)
						(Var "i0",tA,ae)
						tab
				     )
	where tA = wireType (error "table" :: a)
	      tB = wireType (error "table" :: b)
	      all_a_bitRep :: [Matrix (WIDTH a) Bool]
	      all_a_bitRep = allWireReps

	      tab = [ ( fromIntegral $ U.fromMatrix $ w_a
		      , showRepWire (undefined "table" :: a) $ optX $ Just a
		      , fromIntegral $ U.fromMatrix $ w_b
		      , showRepWire (undefined "table" :: b) $ optX $ Just b
		      )
		    | w_a <- all_a_bitRep
		    , a <- case toWireRep $ w_a :: Maybe a of
				 Nothing -> []
				 Just a -> [a]
		    , b <- case fn a of
			     Nothing -> []
			     Just b -> [b]
		    , let w_b = fromWireRep b
		    ]



-----------------------------------------------------------------------------------------------

mux2 :: forall sig a . (Signal sig, Wire a) => sig Bool -> (sig a,sig a) -> sig a
mux2 i ~(t,e)
	= liftS3 (\ ~(Comb i ei)
	 	    ~(Comb t et)
	 	    ~(Comb e ee)
			-> Comb (case unX i :: Maybe Bool of
			          Nothing -> optX (Nothing :: Maybe a)
				  Just True -> t
				  Just False -> e
			     )
			     (entity3 (Name "Lava" "mux2") ei et ee)
	         ) i t e


class MUX a where
	wideMux2 :: Comb Bool -> (a, a) -> a

instance (Wire a) => MUX (Comb a) where
	wideMux2 = mux2


instance (MUX a, MUX b) => MUX (a,b) where
	wideMux2 b ((x0,y0),(x1,y1)) = (wideMux2 b (x0,x1), wideMux2 b (y0,y1))

instance (MUX a) => MUX [a] where
	wideMux2 b (as0,as1) = Prelude.zipWith (\ a0 a1 -> wideMux2 b (a0,a1)) as0 as1


{-
-- A varient of mux that works over
-- Perhaps a bad idea?
choose :: forall sig a . (Pack sig a, Wire a) => sig Bool -> Unpacked sig a -> Unpacked sig a ->  Unpacked sig a
choose i t e = unpack (mux2 i (pack t :: sig a,pack e :: sig a))
-}



-- Selects an arbitrary element of a list
-- The selector has type, [sig Bool]
-- is a binary representation of the unsigned index to select,
-- with the leftmost (first) element most significant.
--
-- The list is indexed: 0-based, left to right
--
-- If (2 ** (length selector)) < (length inputlist)
--     not all elements of the list are selectable.
-- If (2 ** (length selector)) > (length inputlist)
--     the output for selector "value" >= (length inputlist) is
--     not defined.
muxList :: forall sig a . (Signal sig, Wire a) =>[sig Bool] -> [sig a] -> sig a
muxList [s] [a0] = a0
muxList [s] [a0, a1] = mux2 s  (a1,a0)
muxList sel@(s:rest) as = if (aLength <= halfRange)
                          then muxList sel' as
                          else if (aLength > maxRange)
                               then muxList sel as'
                               else muxList'
    where aLength = Prelude.length as
          halfRange = 2 ^ ((Prelude.length sel) -1)
          maxRange = 2 * halfRange
          nselbits = max 1 (ceiling (logBase 2 (fromIntegral aLength)))
          sel' = drop ((Prelude.length sel) - nselbits) sel
          as' = take  maxRange as
          -- muxList' knows that the number of selection bits matches range input choices
          muxList' = mux2 s ((muxList topSelect top), (muxList rest bottom))
          (bottom, top) = splitAt halfRange as
          topLen = fromIntegral $ Prelude.length top
          nbits = max 1 (ceiling (logBase 2 topLen))
          topSelect = drop ((Prelude.length rest) - nbits) rest

-------------------------------------------------------------------------------------------------

instance (Ord a, Wire a) => Ord (Comb a) where
  compare _ _ = error "compare not supported for Comb"
  (<) _ _ = error "(<) not supported for Comb"
  (>=) _ _ = error "(>=) not supported for Comb"
  (>) _ _ = error "(>) not supported for Comb"
  (<=)_ _ = error "(<=) not supported for Comb"
  s1 `max` s2 = fun2 "max" max s1 s2
  s1 `min` s2 = fun2 "min" min s1 s2

	
instance (Ord a, Wire a) => Ord (Seq a) where
  compare _ _ = error "compare not supported for Seq"
  (<) _ _ = error "(<) not supported for Seq"
  (>=) _ _ = error "(>=) not supported for Seq"
  (>) _ _ = error "(>) not supported for Seq"
  (<=)_ _ = error "(<=) not supported for Seq"
  max = liftS2 max
  min = liftS2 min

boolOp :: forall a sig . (Wire a, Signal sig) => String -> (a -> a -> Bool) -> sig a -> sig a -> sig Bool
boolOp nm fn =
	liftS2 $ \ (Comb a ea) (Comb b eb) ->
		    Comb (optX $ do a' <- unX a :: Maybe a
			            b' <- unX b :: Maybe a
			            return $ a' `fn` b')
		      (entity2 (Name (wireName (error "boolOp" :: a)) nm) ea eb)

(.==.) :: forall a sig . (Wire a, Eq a, Signal sig) => sig a -> sig a -> sig Bool
(.==.) = boolOp ".==." (==)

(.>=.) :: forall a sig . (Wire a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.>=.) = boolOp ".>=." (>=)

(.<=.) :: forall a sig . (Wire a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.<=.) = boolOp ".<=." (<=)

(.<.) :: forall a sig . (Wire a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.<.) = boolOp ".<." (<)

(.>.) :: forall a sig . (Wire a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.>.) = boolOp ".>." (>)

-------------------------------------------------------------------------------

-- Perhaps should be in its own module.

{-
type SysEnv = (Clk,Rst)

data Clk = Clk Integer 	-- a single wire, the counter is for debugging/printing only
	deriving (Show)

data Rst = Rst Bool
	deriving (Eq,Show)

-}

type Rst = Seq Bool
	
{-

instance Wire Bool where 
	type X Bool 	= WireVal Bool
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire Bool"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Bool"
	wireName _	= "Bool"
	wireType _	= B
	
instance RepWire Bool where
	type WIDTH Bool	= X1
	toWireRep m  		= return $ m ! 0
	fromWireRep v 		= matrix [v]
	showRepWire _ = show


instance Wire Rst where
	type X Rst = WireVal Rst
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire Rst"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Rst"
	wireName _	= "Rst"
	wireType _	= RstTy

instance RepWire Rst where
	type WIDTH Rst	= X1
	toWireRep m		= return $ Rst $ m ! 0
	fromWireRep (Rst v)	= matrix [v]
	showRepWire _ = show
	
instance Wire Clk where
	type X Clk = WireVal Clk
	optX (Just b) 	= return b
	optX Nothing	= fail "Wire Clk"
	unX (WireVal v)  = return v
	unX (WireUnknown) = fail "Wire Clk"
	wireName _	= "Clk"
	wireType _	= ClkTy

instance RepWire Clk where
	type WIDTH Clk	= X0
	toWireRep m		= return $ Clk 0	-- hu?
	fromWireRep (Clk _)	= matrix []
	showRepWire _ = show

instance Eq Clk where
	_ == _ = False


-}


-- for use only with shallow
shallowRst :: Rst
shallowRst =  Seq (S.fromList $ (map (optX  . Just) ([True] ++ repeat False)))
                  (D $ Pad $ Var "shallowRst")

-- zip (map (optX . Just :: Clk -> X Clk) (map Clk [0..]) :: [X Clk])


delay :: forall a . (Wire a) => Seq a -> Seq a
delay = register' low errorComb


register :: forall a. (Wire a) => Rst -> Comb a -> Seq a -> Seq a
register rst def v = mux2 rst (liftS0 def,delay v)

register' :: forall a. (Wire a) => Rst -> Comb a -> Seq a -> Seq a
register' rst c@(Comb def edef) l@(Seq line eline) = res
   where
	res = Seq sres (D $ Port (Var "o0") $ E $ entity)
	sres = S.zipWith (\ i v -> 
				case unX i :: Maybe Bool of
				   Nothing -> optX (Nothing :: Maybe a)
				   Just (True) -> def
				   Just (False) -> v
			 ) (seqValue rst) (optX (Nothing :: Maybe a) :~ line)		
        entity = Entity (Name "Memory" "register")
                    [(Var "o0", bitTypeOf res)]
                    [(Var "def", bitTypeOf res, unD $ edef),
		     (Var "i0", bitTypeOf res, unD eline), 
		     (Var "rst", RstTy, unD $ seqDriver $ rst), 
		     (Var "clk", ClkTy, Pad (Var "clk"))] []


-- hack
--ans = delay sysEnv 99 ((shallowSeq $ S.fromList $ map (optX . Just) [(1::Int)..100]) :: Seq Int)

{-
instance Wire SysEnv where
	type X SysEnv 	= (X Clk, X Rst)
        optX _ = error "Wire.SysEnv(optX)"
        unX _ = error "Wire.SystEnv(unX)"
	wireName _	= "SysEnv"
	wireType _	= TupleTy [ClkTy, RstTy]

instance RepWire SysEnv where
  type WIDTH SysEnv = X2
  toWireRep _ = error "RepWire.SysEnv(toWireRep)"
  fromWireRep _ = error "RepWire.SysEnv(fromRepWire)"
  showRepWire _ = error "RepWire.SysEnv(showRepWire)"


instance (Signal sig) => Pack sig SysEnv where
	type Unpacked sig SysEnv = (sig Clk, sig Rst)
        pack ~(a,b) = liftS2 pf a b
            where pf  :: Comb Clk -> Comb Rst -> Comb SysEnv
                  pf ~(Comb a ae) ~(Comb b be) =
                    let d = entity2 (Name "Lava" "pair") ae be
                        s = (a,b)
                    in (Comb s d)

	unpack ab =
          ( liftS1 (\ (Comb ~(a,b) abe) -> Comb a (D$ BitIndex 1 (unD abe))) ab
	  , liftS1 (\ (Comb ~(a,b) abe) -> Comb b (D $ BitIndex 0 (unD abe))) ab
	  )
-}

-- For coerce operations, the boolean indicates if the coercian
-- causes a loss of information (an error?)
-- If the error flag is never examined, no extra hardware will be generated to
-- compute or represent the value.
coerceSized ::  (Enum a, Enum b) => a -> (b, Bool)
coerceSized a  = (b, err)
 where valA = fromEnum a
       b = toEnum valA
       valB = fromEnum b
       err = not (valA == valB)


---------------------------------------------------------------------------------------------
-- A StdLogicVector is just an array of bits, but will be represented using
-- std_logic_vector for its Lava *and* IEEE type.


toStdLogicVector :: forall sig w . (Signal sig, Size (WIDTH w), RepWire w) => sig w -> sig (StdLogicVector (WIDTH w))
toStdLogicVector = fun1 "toStdLogicVector" (StdLogicVector . fromWireRep)
 
fromStdLogicVector :: forall sig w . (Signal sig, Size (WIDTH w), RepWire w) => sig (StdLogicVector (WIDTH w)) -> sig w
fromStdLogicVector = fun1 "fromStdLogicVector" $ \ (StdLogicVector v) -> 
				  case toWireRep (v :: Matrix (WIDTH w) Bool) of
				     Just r -> r
				     Nothing -> error "fromStdLogicVector problem"

coerceStdLogicVector :: forall sig a b . (Signal sig, Size a, Size b) 
		     => sig (StdLogicVector a) -> sig (StdLogicVector b)
coerceStdLogicVector = fun1 "coerceStdLogicVector" (SLV.coerce)

spliceStdLogicVector :: forall sig a b . (Signal sig, Integral a, Integral b, Size a, Size b) 
		     => Int -> sig (StdLogicVector a) -> sig (StdLogicVector b)
spliceStdLogicVector i = fun1 "spliceStdLogicVector" (SLV.splice i)
