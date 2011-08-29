{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ParallelListComp, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses  #-}
-- | This module contains a number of primitive circuits, and instance
-- definitions for standard type classes for circuits.
module Language.KansasLava.Utils where

import Control.Applicative
import Data.Bits

import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types

import Data.Sized.Arith
import Data.Sized.Matrix	as M
import Data.Sized.Signed	as SI

-----------------------------------------------------------------------------------------------

-- | The 'Signal' representing True.
high :: forall (sig :: * -> *) . (Signal sig) => sig Bool
high = pureS True

-- | The 'Signal' representing False.
low :: forall (sig :: * -> *) . (Signal sig) => sig Bool
low  = pureS False

-- | The constant combinational values for True.
true :: Comb Bool
true = pureS True

-- | The constant combinational values for False.
false :: Comb Bool
false = pureS False

-----------------------------------------------------------------------------------------------
-- | 1-bit and gate.
and2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
and2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (case (unX a,unX b) of
						     (Just True,Just True) -> optX $ Just True
						     (Just False,_)        -> optX $ Just False
					     	     (_,Just False)        -> optX $ Just False
						     _                     -> optX $ Nothing)
					 $ entity2 (Prim "and2") ae be

-- | 1 bit or gate.
or2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
or2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (case (unX a,unX b) of
						     (Just False,Just False) -> optX $ Just False
						     (Just True,_)           -> optX $ Just True
					     	     (_,Just True)           -> optX $ Just True
						     _                       -> optX $ Nothing )
					 $ entity2 (Prim "or2") ae be
-- | 1 bit xor gate.
xor2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
xor2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (optX $ liftA2 (/=) (unX a) (unX b)) $ entity2 (Prim "xor2") ae be

-- | 1 bit inverter.
bitNot :: (Signal sig) => sig Bool -> sig Bool
bitNot = liftS1 $ \ (Comb a ae) -> Comb (optX $ liftA (not) (unX a)) $ entity1 (Prim "not") ae

-- | Extract the n'th bit of a signal that can be represented as Bits.
testABit :: forall sig a . (Bits a, Rep a, Signal sig) => sig a -> Int -> sig Bool
testABit x y = liftS1 (\ (Comb a ae) -> Comb (optX $ liftA (flip testBit y) (unX a))
                                      $ entity2 (Prim "testBit") ae (D $ Generic (fromIntegral y) :: D Integer)
		      ) x

-- | Predicate to see if a Signed value is positive.
isPositive :: forall sig ix . (Signal sig, Size ix, Enum ix, Integral ix, Bits (sig (Signed ix))) => sig (Signed ix) -> sig Bool
isPositive a = bitNot $ testABit a  msb
    where msb = bitSize a - 1


infixr 3 .&&.
infixr 2 .||.
infixr 2 .^.

-- | Alias for 'and2'.
(.&&.) :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
(.&&.) = and2

-- | Alias for 'or2'.
(.||.) :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
(.||.) = or2

-- | Alias for 'xor2'.
(.^.) :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
(.^.)  = xor2

-----------------------------------------------------------------------------------------------

instance (Show a, Rep a, Num a) => Num (Comb a) where
    s1 + s2 = fun2 "+" (+) s1 s2
    s1 - s2 = fun2 "-" (-) s1 s2
    s1 * s2 = fun2 "*" (*) s1 s2
    negate s = fun1 "negate" (negate) s
    abs s    = fun1 "abs"    (abs)    s
    signum s = fun1 "signum" (signum) s
    fromInteger n = pureS (fromInteger n)

instance (Show a, Rep a, Num a) => Num (CSeq c a) where
    (+) = liftS2 (+)
    (-) = liftS2 (-)
    (*) = liftS2 (*)
    negate = liftS1 negate
    abs = liftS1 abs
    signum = liftS1 signum
    fromInteger n = pureS (fromInteger n)

instance (Bounded a, Rep a) => Bounded (Comb a) where
    minBound = pureS $ minBound
    maxBound = pureS $ maxBound

-- Somehow, these ignore the type name
instance (Show a, Bits a, Rep a)
	=> Bits (Comb a) where
    s1 .&. s2 = fun2 ".&." (.&.) s1 s2
    s1 .|. s2 = fun2 ".|." (.|.) s1 s2
    s1 `xor` s2 = fun2 ".^." (xor) s1 s2
    s1 `shiftL` n = fun2 "shiftL" (shiftL) s1 (fromIntegral n)
    s1 `shiftR` n = fun2 "shiftR" (shiftR) s1 (fromIntegral n)
    s1 `rotateL` n = fun2 "rotateL" (rotateL) s1 (fromIntegral n)
    s1 `rotateR` n = fun2 "rotateR" (rotateR) s1 (fromIntegral n)
    complement s = fun1 "complement" (complement) s
    bitSize s                       = typeWidth (bitTypeOf s)
    isSigned s                      = isTypeSigned (bitTypeOf s)

instance (Show a, Bits a, Rep a)
	=> Bits (CSeq c a) where
    (.&.)   = liftS2 (.&.)
    (.|.)  = liftS2 (.|.)
    xor    = liftS2 (xor)
    shiftL s n = liftS1 (flip shiftL n) s
    shiftR s n = liftS1 (flip shiftR n) s
    rotateL s n = liftS1 (flip rotateL n) s
    rotateR s n = liftS1 (flip rotateR n) s
    complement = liftS1 complement
    bitSize s                       = typeWidth (bitTypeOf s)
    isSigned s                      = isTypeSigned (bitTypeOf s)

instance (Eq a, Show a, Fractional a, Rep a) => Fractional (Comb a) where
    s1 / s2 = fun2 "/" (/) s1 s2
    recip s1 = fun1 "recip" (recip) s1
    -- This should just fold down to the raw bits.
    fromRational r = toComb (fromRational r :: a)

instance (Eq a, Show a, Fractional a, Rep a) => Fractional (CSeq c a) where
    (/) = liftS2 (/)
    recip = liftS1 recip
    fromRational = liftS0 . fromRational

instance (Rep a, Enum a) => Enum (Comb a) where
	toEnum   = error "toEnum not supported for Comb"
	fromEnum = error "fromEnum not supported for Comb"

instance (Rep a, Real a) => Real (Comb a) where
	toRational = error "toRational not supported for Comb"

instance (Rep a, Integral a) => Integral (Comb a) where
	div num dom = fun2 "div" quot num dom
	mod num dom = fun2 "mod" mod num dom

	quotRem = error "quotRem not supported for Comb"
	toInteger = error "toInteger not supported for Comb"

-----------------------------------------------------------------------------------------------
-- Matrix ops

{-
 - TO reinstall!
mapToBoolMatrix :: forall sig w . (Signal sig, Size (WIDTH w), Rep w) => sig w -> sig (Matrix (WIDTH w) Bool)
mapToBoolMatrix = liftS1 $ \ (Comb a d) -> Comb
	(( optX (liftM fromWireRep ((unX a) :: Maybe w
		    ) :: Maybe (Matrix (WIDTH w) Bool))
	 ) :: X (Matrix (WIDTH w) Bool))
	(entity1 (Prim "toBoolMatrix") d)

toBoolMatrix :: forall sig w . (Signal sig, Integral (WIDTH w), Size (WIDTH w), Rep w)
             => sig w -> Matrix (WIDTH w) (sig Bool)
toBoolMatrix = unpack . mapToBoolMatrix

mapFromBoolMatrix :: forall sig w . (Signal sig, Size (WIDTH w), Rep w) => sig (Matrix (WIDTH w) Bool) -> sig w
mapFromBoolMatrix = liftS1 $ \ (Comb a d) -> Comb
	(case unX (a :: X (Matrix (WIDTH w) Bool)) :: Maybe (Matrix (WIDTH w) Bool) of
	     Nothing -> optX (Nothing :: Maybe w)
	     Just r0 -> optX (toWireRep r0 :: Maybe w)
	)
	(entity1 (Prim "fromBoolMatrix") d)

fromBoolMatrix :: forall sig w . (Signal sig, Integral (WIDTH w), Size (WIDTH w), Rep w)
	       => Matrix (WIDTH w) (sig Bool) ->  sig w
fromBoolMatrix = mapFromBoolMatrix . pack

-}

-----------------------------------------------------------------------------------------------
-- Map Ops


-- Assumping that the domain is finite (beacause of Rep), and *small* (say, < ~256 values).

-- | Given a function over a finite domain, generate a ROM representing the
-- function. To make this feasible to implement, we assume that the domain is
-- small (< 2^8 values).
funMap :: forall sig a b . (Signal sig, Rep a, Rep b) => (a -> Maybe b) -> sig a -> sig b
funMap fn = liftS1 $ \ (Comb a (D ae))
			-> Comb (case unX a of
				   Nothing -> optX Nothing
				   Just v -> optX (fn v))
				     (D $ Port ("o0")
					$ E
					$ Entity (Prim "asyncRead")
					         [("o0",tB)]
						 [ ("i0",tMB,rom)
						 , ("i1",tA,ae)
						 ]
				     )
	where tA = repType (Witness :: Witness a)
	      tB = repType (Witness :: Witness b)
              tMB = MatrixTy (Prelude.length all_a_bitRep) tB

              undefB = unknownRepValue (Witness :: Witness b)


	      all_a_bitRep :: [RepValue]
	      all_a_bitRep = allReps (Witness :: Witness a)

              rom = Port "o0" $ E $ Entity (Prim "rom") [("o0",tMB)] [("defs",RomTy (Prelude.length all_a_bitRep),Lits lits)]

              -- assumes in order table generation
	      lits :: [RepValue]
	      lits = [ case unX (fromRep w_a) of
				 Nothing -> undefB
				 Just a -> case fn a of
			                    Nothing -> undefB
			                    Just b -> toRep (pureX b)
		    | w_a <- all_a_bitRep
		    ]

-- TODO: move this to somewhere else
-- | Convert a RepValue to a Haskell Integer.
repValueToInteger :: RepValue -> Integer
repValueToInteger (RepValue []) = 0
repValueToInteger (RepValue (Just True:xs)) = 1 + repValueToInteger (RepValue xs) * 2
repValueToInteger (RepValue (Just False:xs)) = 0 + repValueToInteger (RepValue xs) * 2
repValueToInteger (RepValue _) = error "repValueToInteger over unknown value"



-----------------------------------------------------------------------------------------------

-- mux2 uses a hack around liftS3 to eliminate an unnecessary (unpack . pack) arising from
-- the use of liftS3. This is safe, because we know the kind of node that we're building.
-- | Multiplexer with a 1-bit selector and arbitrary width data inputs.
mux2 :: forall sig a . (Signal sig, Rep a) => sig Bool -> (sig a,sig a) -> sig a
mux2 iSig (tSig,eSig)
	= liftS3 (\ (Comb i ei)
	 	    (Comb t et)
	 	    (Comb e ee)
			-> Comb (mux2shallow i t e)
                                (entity3 (Prim "mux2") ei et ee)
	         ) iSig tSig eSig

-- | Shallow definition of a multiplexer. Deals with 3-value logic.
mux2shallow :: forall a . (Rep a) => X Bool -> X a -> X a -> X a
mux2shallow i t e =
   case unX i of
       Nothing -> optX Nothing
       Just True -> t
       Just False -> e

-------------------------------------------------------------------------------------------------
-- | TODO: Document this.
eval :: forall a . (Rep a) => a -> ()
eval a = count $ unRepValue $ toRep (optX (Just a))
  where count (Just True:rest) = count rest
	count (Just False:rest) = count rest
	count (Nothing:rest) = count rest
	count [] = ()

-- | TODO: Document this.
evalX :: forall a . (Rep a) => X a -> ()
evalX a = count $ unRepValue $ toRep a
  where count (Just True:rest) = count rest
	count (Just False:rest) = count rest
	count (Nothing:rest) = count rest
	count [] = ()

-------------------------------------------------------------------------------------------------

-- | Alias for '.!.'
muxMatrix
	:: forall sig x a
	 . (Signal sig, Size x, Rep x, Rep a)
	=> sig (Matrix x a)
	-> sig x
	-> sig a
muxMatrix = (.!.)

-- | Extract the n'th element of a vector.
(.!.)	:: forall sig x a
	 . (Signal sig, Size x, Rep x, Rep a)
	=> sig (Matrix x a)
	-> sig x
	-> sig a
(.!.) mSig xSig = liftS2 (\
		    (Comb (XMatrix m) me)
	 	    (Comb x xe)
			-> Comb (evalX (XMatrix m) `seq` case (unX x) of
				    Just x' -> m M.! x'
				    Nothing -> optX Nothing
				)
			     (entity2 (Prim "index") xe me) -- order reversed
	         ) mSig xSig

{-
updateMatrix :: forall sig x a
	 . (Signal sig, Size x, Rep x, Rep a)
	=> sig x
	-> sig a
	-> sig (Matrix x a)
	-> sig (Matrix x a)
updateMatrix x v m = liftS3 (\
	 	    (Comb x xe)
	 	    (Comb v ve)
		    (Comb (XMatrix m) me)
			-> Comb (case (unX x) of
				    Just x' -> XMatrix (m M.// [(x',v)])
				    Nothing -> optX Nothing
				)
			     (entity3 (Prim "update") xe ve me)
	         ) x v m
-}

-------------------------------------------------------------------------------------------------

instance (Ord a, Rep a) => Ord (Comb a) where
  compare _ _ = error "compare not supported for Comb"
  (<) _ _ = error "(<) not supported for Comb"
  (>=) _ _ = error "(>=) not supported for Comb"
  (>) _ _ = error "(>) not supported for Comb"
  (<=)_ _ = error "(<=) not supported for Comb"
  s1 `max` s2 = fun2 "max" max s1 s2
  s1 `min` s2 = fun2 "min" min s1 s2


instance (Ord a, Rep a) => Ord (CSeq c a) where
  compare _ _ = error "compare not supported for Seq"
  (<) _ _ = error "(<) not supported for Seq"
  (>=) _ _ = error "(>=) not supported for Seq"
  (>) _ _ = error "(>) not supported for Seq"
  (<=)_ _ = error "(<=) not supported for Seq"
  max = liftS2 max
  min = liftS2 min

instance (Bounded a, Rep a) => Bounded (CSeq c a) where
    minBound = liftS0 minBound
    maxBound = liftS0 maxBound

-- | Lift a (named) binary function over bools to be over 'Signal's.
boolOp :: forall a sig . (Rep a, Signal sig) => String -> (a -> a -> Bool) -> sig a -> sig a -> sig Bool
boolOp nm fn =
	liftS2 $ \ (Comb a ea) (Comb b eb) ->
		    Comb (optX $ do a' <- unX a
			            b' <- unX b
			            return $ a' `fn` b')
		      (entity2 (Prim nm) ea eb)

infix 4 .==., .>=., .<=., .<., .>.

-- | N-bit equality.
(.==.) :: forall a sig . (Rep a, Eq a, Signal sig) => sig a -> sig a -> sig Bool
(.==.) = boolOp ".==." (==)

-- | N-bit not-equals.
(./=.) :: forall a sig . (Rep a, Eq a, Signal sig) => sig a -> sig a -> sig Bool
(./=.) xs ys = bitNot (xs .==. ys) -- TODO: consider making this a primitive

-- | N-bit greater-than-or-equals.
(.>=.) :: forall a sig . (Rep a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.>=.) = boolOp ".>=." (>=)

-- | N-bit less-than-or-equals.
(.<=.) :: forall a sig . (Rep a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.<=.) = boolOp ".<=." (<=)

-- | N-bit less-than.
(.<.) :: forall a sig . (Rep a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.<.) = boolOp ".<." (<)

-- | N-bit greater-than.
(.>.) :: forall a sig . (Rep a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
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

-- Rational number is Frequency, in Hz
--data Clock c = Clock Rational (D ())
--    deriving (Show)

-- For now, till other codes get fixed
-- | An Env represents a clock domain.
type Env c = ()

-- | The shallow representation of a clock.
shallowEnv :: ()
shallowEnv = ()

{-
Env { clockEnv  :: Clock c
	         , resetEnv  :: CSeq c Bool
	         , enableEnv :: CSeq c Bool
                 }
    deriving (Show)

toEnv :: Clock c -> Env c
toEnv c = Env
	    c
	    (toSeq $ repeat False) -- [True] ++ repeat False)
	    (toSeq $ repeat True)

-- | Takes a clock, a 'Seq' of reset signals, and a 'Seq' of enables.
mkEnv :: Clock c -> CSeq c Bool -> CSeq c Bool -> Env c
mkEnv = Env

shallowEnv :: Env ()
shallowEnv = toEnv (Clock 1 (D $ Error "no deep clock"))

clkDriver :: Clock c -> D ()
clkDriver (Clock _ d) = d
-}

{-
-- for use only with shallow
shallowRst :: Rst
shallowRst =  Seq (S.fromList $ (map (optX  . Just) ([True] ++ repeat False)))
                  (D $ Pad $ Var "shallowRst")
-}

-- zip (map (optX . Just :: Clk -> X Clk) (map Clk [0..]) :: [X Clk])

-- a delay is a register with no defined default / initial value.

-- | a delay is a register with no defined default / initial value.
delay :: forall a clk . (Rep a, Clock clk) => CSeq clk a -> CSeq clk a
delay ~(Seq line eline) = res
   where
        def = optX $ Nothing

        -- rep = toRep def
	res = Seq sres1 (D $ Port ("o0") $ E $ entity)

	sres0 = line
	sres1 = S.Cons def sres0

        entity = Entity (Prim "delay")
                    [("o0", bitTypeOf res)]
                    [("i0", bitTypeOf res, unD eline),
		     ("clk",ClkTy, Pad $ OVar (-2) "clk"),
		     ("rst",B,     Pad $ OVar (-1) "rst")
		    ]
-- | delays generates a serial sequence of n delays.
delays :: forall a clk .  (Rep a, Clock clk) => Int -> CSeq clk a -> CSeq clk a
delays n ss = iterate delay ss !! n


-- | A register is a state element with a reset. The reset is supplied by the clock domain in the CSeq.
register :: forall a clk .  (Rep a, Clock clk) => a -> CSeq clk a -> CSeq clk a
register first  ~(Seq line eline) = res
   where
        def = optX $ Just first

        rep = toRep def
	res = Seq sres1 (D $ Port ("o0") $ E $ entity)

	sres0 = line
	sres1 = S.Cons def sres0

        entity = Entity (Prim "register")
                    [("o0", bitTypeOf res)]
                    [("i0", bitTypeOf res, unD eline),
                     ("def",GenericTy,Generic (fromRepToInteger rep)),
		     ("clk",ClkTy, Pad $ OVar (-2) "clk"),
		     ("rst",B,     Pad $ OVar (-1) "rst")
		    ]
-- | registers generates a serial sequence of n registers, all with the same initial value.
registers :: forall a clk .  (Rep a, Clock clk) => Int -> a -> CSeq clk a -> CSeq clk a
registers n def ss = iterate (register def) ss !! n

-- hack
--ans = delay sysEnv 99 ((shallowSeq $ S.fromList $ map (optX . Just) [(1::Int)..100]) :: Seq Int)

-- For coerce operations, the boolean indicates if the coercian
-- causes a loss of information (an error?)
-- If the error flag is never examined, no extra hardware will be generated to
-- compute or represent the value.


{-
---------------------------------------------------------------------------------------------
-- A StdLogicVector is just an array of bits, but will be represented using
-- std_logic_vector for its Lava *and* IEEE type.
-- These will fail at LRT if the sizes are incorrect, and this could be handled by including .
--

class Size (WIDTH w) => StdLogic w where
  type WIDTH w

instance StdLogic Bool where
   type WIDTH Bool = X1

instance Size w => StdLogic (U.Unsigned w) where
   type WIDTH (U.Unsigned w) = w

instance Size w => StdLogic (SI.Signed w) where
   type WIDTH (SI.Signed w)  = w

instance Size w => StdLogic (M.Matrix w Bool) where
   type WIDTH (M.Matrix w Bool)  = w

instance StdLogic X0 where
   type WIDTH X0 = X0

-- MESSSSYYYYY.
instance (Size (LOG (SUB (X1_ x) X1)), StdLogic x) => StdLogic (X1_ x) where
   type WIDTH (X1_ x) = LOG (SUB (X1_ x) X1)

instance (Size (LOG (APP1 (ADD x N1))), StdLogic x) => StdLogic (X0_ x) where
   type WIDTH (X0_ x) = LOG (SUB (X0_ x) X1)

toSLV :: forall w . (Rep w, StdLogic w) => w -> StdLogicVector (WIDTH w)
toSLV v = case toRep (Witness :: Witness w) (optX (return v) :: X w) of
		RepValue v -> StdLogicVector $ M.matrix $ v

fromSLV :: forall w . (Rep w, StdLogic w) =>  StdLogicVector (WIDTH w) -> Maybe w
fromSLV x@(StdLogicVector v) = unX (fromRep (Witness :: Witness w) (RepValue (M.toList v))) :: Maybe w

-}



{-
instance (Integral ix, Size ix, Signal sig) => Pack sig (StdLogicVector ix) where
	type Unpacked sig (StdLogicVector ix) = Matrix ix (sig Bool)
	pack m = liftSL (\ ms -> let sh :: Matrix ix (Just Bool)
				     sh = M.fromList [ m | Comb (XBool m) _ <- ms ]
				     de = entityN (Prim "concat") [ d | Comb _ d <- ms ]
				 in Comb (XSV (StdLogicVector sh)) de) (M.toList m)
	unpack sig = forAll $ \ i -> testABit sig (fromIntegral i)


--  toStdLogicVector :: (Signal sig, StdLogic c, Size x) => sig (c x) -> sig (StdLogicVector x)
--  fromStdLogicVector :: (Signal sig, StdLogic c, Size x) => sig (c x) -> sig (StdLogicVector x)
-- This is pack/unpack???
-}

{-
toStdLogicVector :: forall sig w . (Signal sig, Rep w, StdLogic w) => sig w -> sig (StdLogicVector (WIDTH w))
toStdLogicVector = fun1 "toStdLogicVector" $ \ v -> case toRep (optX (return v)) of
						       RepValue v' -> StdLogicVector $ M.matrix $ v'

-- TODO: way may have to lift these up to handle unknowns better.
fromStdLogicVector :: forall sig w . (Signal sig, StdLogic w, Rep w) => sig (StdLogicVector (WIDTH w)) -> sig w
fromStdLogicVector = fun1' "fromStdLogicVector" $ \ (StdLogicVector v) ->
				  unX (fromRep (RepValue (M.toList v)))

-- This is done bit-wise; grab the correct (aka size 'b') number of bits, adding zeros or truncating if needed.
coerceStdLogicVector :: forall sig a b . (Signal sig, Size a, Size b)
		     => sig (StdLogicVector a) -> sig (StdLogicVector b)
coerceStdLogicVector = fun1 "coerceStdLogicVector" (SLV.coerceSLV)

-- Starting at the given bit; grab the specified (by the type) number of bits.
extractStdLogicVector :: forall sig a b . (Signal sig, Integral a, Integral b, Size a, Size b)
		     => Int -> sig (StdLogicVector a) -> sig (StdLogicVector b)
extractStdLogicVector i =  -- fun2 "spliceStdLogicVector" (SLV.splice i)
	liftS1 $ \ (Comb a ea) ->
		    Comb (optX $ do a' <- unX a
			            return $ (SLV.spliceSLV i a' :: StdLogicVector b))
		         (entity2 (Prim "spliceStdLogicVector") (D $ Generic (fromIntegral i) :: D Integer) ea)
-}
{-
{-
append :: forall sig a b c . (Signal sig, Rep a, Rep b, Rep c)
	=> sig a
	-> sig b
	-> sig c
appendStdLogicVector = liftS2 $ \ (Comb a ea) (Comb b eb) ->
			Comb (optX $ do a' <- unX a
					b' <- unX b
					return $ undefined)
			     (entity2 (Prim "concat") ea eb)
-}

appendStdLogicVector :: forall sig a b . (Signal sig, Size a, Size b, Size (ADD a b))
	=> sig (StdLogicVector a)
	-> sig (StdLogicVector b)
	-> sig (StdLogicVector (ADD a b))
appendStdLogicVector = liftS2 $ \ (Comb a ea) (Comb b eb) ->
			Comb (optX $ do a' <- unX a
					b' <- unX b
					return $ SLV.appendSLV a' b')
			     (entity2 (Prim "concat") ea eb)



-}

-- This is the funny one, needed for our application
--instance (Enum ix, Size ix, Integral m, Size m) => StdLogic (Sampled.Sampled m ix) where
--	type WIDTH (Sampled.Sampled m ix) = m

-- Move this to a better place.

-------------------------------------------------------------------------------------


{-
	   , Signal sig, Rep a2, Rep a1
	   , StdLogic a, StdLogic a1, StdLogic a2) => sig a -> sig (a1,a2)
factor a = pack ( fromStdLogicVector $ extractStdLogicVector 0 vec
		 , fromStdLogicVector $ extractStdLogicVector (size (error "witness" :: WIDTH a1)) vec
		 )
	 where vec :: sig (StdLogicVector (WIDTH a))
	       vec = toStdLogicVector a
-}

-------------------------------------------------------------------------------------

-- | The identity function, lifted to 'Signal's.
lavaId :: (Signal sig, Rep a) => sig a -> sig a
lavaId = fun1 "id" id

-------------------------------------------------------------------------------------

-- | 'ignoring' is used to make sure a value is reified.
ignoring :: (Signal sig, Rep a, Rep b) => sig a -> sig b -> sig a
ignoring = fun2 "const" const

-------------------------------------------------------------------------------------

-- | Given a representable value for a discirminant and a list of input signals, generate a n-ary mux.
cASE :: (Rep b, Signal seq) => [(seq Bool,seq b)] -> seq b -> seq b
cASE [] def = def
cASE ((p,e):pes) def = mux2 p (e,cASE pes def)

-------------------------------------------------------------------------------------
--
-- | if the first argument is Nothing, take whole list, otherwise, normal take with the Int inside the Just
takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe = maybe id take

-------------------------------------------------------------------------------------

-- | translate using raw underlying bits, Width *must* be the same.
bitwise :: forall sig a b . (Signal sig, Rep a, Rep b, W a ~ W b) => sig a -> sig b
bitwise = liftS1 $ \ (Comb a ae) -> Comb (fromRep (toRep a)) $ entity1 (Prim "coerce") ae


-- | translate using raw underlying bits for deep, but given function for shallow, Width *must* be the same.
coerce :: forall sig a b . (Signal sig, Rep a, Rep b, W a ~ W b) => (a -> b) -> sig a -> sig b
coerce f = liftS1 $ \ (Comb a ae) ->
	let
	    b = liftX f a
	    b' | toRep a == toRep b = b
	       | otherwise          = error "coerce fails to preserve bit pattern"
	in Comb b' $ entity1 (Prim "coerce") ae

-- | Coerce a value from on type to another, interpreting the bits as a signed
-- value. Do not sign extend.
signedX :: forall a b . (Rep a, Rep b) => X a -> X b
signedX = id
       . fromRep
       . RepValue
       . (\ m -> take (repWidth (Witness :: Witness b)) (m ++ repeat (last m)))  -- not signed extended!
       . unRepValue
       . toRep

-- | consider the bits as signed number (sign extend)
signed :: (Rep a, Rep b, Num b, Signal sig)  => sig a -> sig b
signed = liftS1 $ \ (Comb a ae) -> Comb (signedX a) $ entity1 (Prim "signed") ae

-- | Consider the value as an unsigned value.
unsignedX :: forall a b . (Rep a, Rep b) => X a -> X b
unsignedX = id
       . fromRep
       . RepValue
       . (\ m -> take (repWidth (Witness :: Witness b)) (m ++ repeat (Just False)))  -- not signed extended!
       . unRepValue
       . toRep

-- | consider the bits an unsigned number (zero extend)
unsigned :: (Rep a, Rep b, Num b, Signal sig)  => sig a -> sig b
unsigned = liftS1 $ \ (Comb a ae) -> Comb (unsignedX a) $ entity1 (Prim "unsigned") ae

----------------------------------------------------------------------------
-- | translate using raw underlying bits, type  *must* be the same, but is not statically checked.
unsafeId :: forall sig a b . (Signal sig, Rep a, Rep b) => sig a -> sig b
unsafeId = liftS1 $ \ (Comb a (D ae)) -> Comb (fromRep $ toRep a) $ (D ae)

----------------------------------------------------------------------------

-- | given a signal of a1 + a2 width, yield a signal with a pair of values of width a1 and a2 respectively.
factor :: forall a a1 a2 sig . (Signal sig, Rep a, Rep a1, Rep a2, W a ~ ADD (W a1) (W a2)) => sig a -> (sig a1, sig a2)
factor a = unpack (bitwise a :: sig (a1,a2))

-- | given two signals of a1 and a2 width, respectively, pack them into a signal of a1 + a2 width.
append :: forall sig a b c . (Signal sig, Rep a, Rep b, Rep c, W c ~ ADD (W a) (W b)) => sig a -> sig b -> sig c
append x y = bitwise (pack (x,y) :: sig (a,b))

----------------------------------------------------------------------------

-- | The first argument is the value is our value under test;
-- the second is our reference value.
-- If the reference is undefined, then the VUT *can* also be under test.
-- This only works for shallow circuits, and is used when creating test benches.
refinesFrom :: forall sig a . (Signal sig, Rep a) => sig a -> sig a -> sig Bool
refinesFrom = liftS2 $ \ (Comb a _) (Comb b _) ->
                        Comb (let res = and
                                      [ case (vut,ref) of
                                           (_,Nothing)       -> True
                                           (Just x,Just y) -> x == y
                                           _                     -> False
                                      | (vut,ref) <- zip (unRepValue (toRep a))
                                                         (unRepValue (toRep b))
                                      ]
                              in optX (Just res))
                             (D $ Error "no deep entity for refinesFrom")

--------------------------------------------------------------------------------
-- | Create a register, pass the output of the register through some
-- combinational logic, then pass the result back into the register input.
iterateS :: (Rep a, Clock c, seq ~ CSeq c) => (Comb a -> Comb a) -> a -> seq a
iterateS f start = out where
        out = register start (liftS1 f out)

---------------------------------------------------------------------

-- These varients of succ/pred can handle bounded values and do proper looping.
loopingInc :: (Bounded a, Num a, Rep a, Signal sig) => sig a -> sig a
loopingInc a = mux2 (a .==. pureS maxBound) (pureS 0,liftS2 (+) a (pureS 1))

loopingDec :: (Bounded a, Num a, Rep a, Signal sig) => sig a -> sig a
loopingDec a = mux2 (a .==. pureS 0) (pureS maxBound,liftS2 (-) a (pureS 1))


