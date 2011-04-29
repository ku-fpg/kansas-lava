{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ParallelListComp, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses  #-}

module Language.KansasLava.Utils where

import Control.Applicative
import Control.Monad
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
import qualified Data.Sized.Sampled as Sampled

-----------------------------------------------------------------------------------------------

high, low :: forall (sig :: * -> *) . (Signal sig) => sig Bool
high = pureS True
low  = pureS False

true, false :: Comb Bool
true = pureS True
false = pureS False

-----------------------------------------------------------------------------------------------
and2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
and2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (case (unX a,unX b) of
						     (Just True,Just True) -> optX $ Just True
						     (Just False,_)        -> optX $ Just False
					     	     (_,Just False)        -> optX $ Just False
						     _                     -> optX $ Nothing)
					 $ entity2 (Name "Lava" "and2") ae be

or2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
or2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (case (unX a,unX b) of
						     (Just False,Just False) -> optX $ Just False
						     (Just True,_)           -> optX $ Just True
					     	     (_,Just True)           -> optX $ Just True
						     _                       -> optX $ Nothing )
					 $ entity2 (Name "Lava" "or2") ae be
xor2 :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
xor2 = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (optX $ liftA2 (/=) (unX a) (unX b)) $ entity2 (Name "Lava" "xor2") ae be

bitNot :: (Signal sig) => sig Bool -> sig Bool
bitNot = liftS1 $ \ (Comb a ae) -> Comb (optX $ liftA (not) (unX a)) $ entity1 (Name "Lava" "not") ae

testABit :: forall sig a . (Bits a, Rep a, Signal sig) => sig a -> Int -> sig Bool
testABit x y = liftS1 (\ (Comb a ae) -> Comb (optX $ liftA (flip testBit y) (unX a))
                                      $ entity2 (Name "Lava" "testBit") ae (D $ Generic (fromIntegral y) :: D Integer)
		      ) x


isPositive :: forall sig ix . (Signal sig, Size ix, Enum ix, Integral ix, Bits (sig (Signed ix))) => sig (Signed ix) -> sig Bool
isPositive a = bitNot $ testABit a  msb
    where msb = bitSize a - 1


infixr 3 .&&.
infixr 2 .||.
infixr 2 .^.

(.&&.) :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
(.&&.) = and2

(.||.) :: (Signal sig) => sig Bool -> sig Bool -> sig Bool
(.||.) = or2

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

-----------------------------------------------------------------------------------------------
-- Matrix ops

{-
 - TO reinstall!
mapToBoolMatrix :: forall sig w . (Signal sig, Size (WIDTH w), Rep w) => sig w -> sig (Matrix (WIDTH w) Bool)
mapToBoolMatrix = liftS1 $ \ (Comb a d) -> Comb
	(( optX (liftM fromWireRep ((unX a) :: Maybe w
		    ) :: Maybe (Matrix (WIDTH w) Bool))
	 ) :: X (Matrix (WIDTH w) Bool))
	(entity1 (Name "Lava" "toBoolMatrix") d)

toBoolMatrix :: forall sig w . (Signal sig, Integral (WIDTH w), Size (WIDTH w), Rep w)
             => sig w -> Matrix (WIDTH w) (sig Bool)
toBoolMatrix = unpack . mapToBoolMatrix

mapFromBoolMatrix :: forall sig w . (Signal sig, Size (WIDTH w), Rep w) => sig (Matrix (WIDTH w) Bool) -> sig w
mapFromBoolMatrix = liftS1 $ \ (Comb a d) -> Comb
	(case unX (a :: X (Matrix (WIDTH w) Bool)) :: Maybe (Matrix (WIDTH w) Bool) of
	     Nothing -> optX (Nothing :: Maybe w)
	     Just r0 -> optX (toWireRep r0 :: Maybe w)
	)
	(entity1 (Name "Lava" "fromBoolMatrix") d)

fromBoolMatrix :: forall sig w . (Signal sig, Integral (WIDTH w), Size (WIDTH w), Rep w)
	       => Matrix (WIDTH w) (sig Bool) ->  sig w
fromBoolMatrix = mapFromBoolMatrix . pack

-}

-----------------------------------------------------------------------------------------------
-- Map Ops


-- Assumping that the domain is finite (beacause of Rep), and *small* (say, < ~256 values).

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
repValueToInteger :: RepValue -> Integer
repValueToInteger (RepValue []) = 0
repValueToInteger (RepValue (WireVal True:xs)) = 1 + repValueToInteger (RepValue xs) * 2
repValueToInteger (RepValue (WireVal False:xs)) = 0 + repValueToInteger (RepValue xs) * 2
repValueToInteger (RepValue _) = error "repValueToInteger over unknown value"



-----------------------------------------------------------------------------------------------

-- mux2 uses a hack around liftS3 to eliminate an unnecessary (unpack . pack) arising from
-- the use of liftS3. This is safe, because we know the kind of node that we're building.
mux2 :: forall sig a . (Signal sig, Rep a) => sig Bool -> (sig a,sig a) -> sig a
mux2 iSig (tSig,eSig)
	= liftS3 (\ (Comb i _)
	 	    (Comb t _)
	 	    (Comb e _)
			-> Comb (mux2shallow i t e)
                            (entity3 (Name "Lava" "mux2") (deepS iSig) (deepS tSig) (deepS eSig))
--			        (entity3 (Name "Lava" "mux2") (ei et ee)

	         ) iSig tSig eSig

liftS3' :: forall a b c d sig . (Signal sig, Rep a, Rep b, Rep c, Rep d, sig a ~ Seq a, sig b ~ Seq b, sig c ~ Seq c, sig d ~ Seq d)
       => (Comb a -> Comb b -> Comb c -> Comb d) -> sig a -> sig b -> sig c -> sig d
liftS3' f (Seq a _) (Seq b _) (Seq c _) = Seq (streamZipWith3 f' a b c) ed
      where
	Comb _ ed = error "" -- f (deepComb ea) (deepComb eb) (deepComb ec)
	f' :: X a -> X b -> X c -> X d
	f' x y z = case f (shallowComb x) (shallowComb y) (shallowComb z)  of
		      Comb d _ -> d
        streamZipWith3 fun (S.Cons x xs) (S.Cons y ys) (S.Cons z zs) = S.Cons (fun x y z) (streamZipWith3 fun xs ys zs)


mux2shallow :: forall a . (Rep a) => X Bool -> X a -> X a -> X a
mux2shallow i t e =
   case unX i of
       Nothing -> optX Nothing
       Just True -> t
       Just False -> e

class MUX a where
	wideMux2 :: Comb Bool -> (a, a) -> a

instance (Rep a) => MUX (Comb a) where
	wideMux2 = mux2


instance (MUX a, MUX b) => MUX (a,b) where
	wideMux2 b ((x0,y0),(x1,y1)) = (wideMux2 b (x0,x1), wideMux2 b (y0,y1))

instance (MUX a) => MUX [a] where
	wideMux2 b (as0,as1) = Prelude.zipWith (\ a0 a1 -> wideMux2 b (a0,a1)) as0 as1


{-
-- A varient of mux that works over
-- Perhaps a bad idea?
choose :: forall sig a . (Pack sig a, Rep a) => sig Bool -> Unpacked sig a -> Unpacked sig a ->  Unpacked sig a
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
muxList :: forall sig a . (Signal sig, Rep a) =>[sig Bool] -> [sig a] -> sig a
muxList [] _ = error "muxList: must have at least one selection bit"
muxList [_] [a0] = a0
muxList [s] [a0, a1] = mux2 s  (a1,a0)
muxList sel@(s:rest) as = if (aLength <= halfRange)
                          then muxList sel' as
                          else if (aLength > maxRange)
                               then muxList sel as'
                               else muxList'
    where aLength = Prelude.length as
          halfRange = 2 ^ ((Prelude.length sel) -1)
          maxRange = 2 * halfRange
          nselbits = max 1 (ceiling (logBase (2 :: Double) (fromIntegral aLength)))
          sel' = drop ((Prelude.length sel) - nselbits) sel
          as' = take  maxRange as
          -- muxList' knows that the number of selection bits matches range input choices
          muxList' = mux2 s ((muxList topSelect top), (muxList rest bottom))
          (bottom, top) = splitAt halfRange as
          topLen = fromIntegral $ Prelude.length top
          nbits = max 1 (ceiling (logBase (2::Double) topLen))
          topSelect = drop ((Prelude.length rest) - nbits) rest


-------------------------------------------------------------------------------------------------
eval :: forall a . (Rep a) => a -> ()
eval a = count $ unRepValue $ toRep (optX (Just a))
  where count (WireVal True:rest) = count rest
	count (WireVal False:rest) = count rest
	count (WireUnknown:rest) = count rest
	count [] = ()

evalX :: forall a . (Rep a) => X a -> ()
evalX a = count $ unRepValue $ toRep a
  where count (WireVal True:rest) = count rest
	count (WireVal False:rest) = count rest
	count (WireUnknown:rest) = count rest
	count [] = ()

-------------------------------------------------------------------------------------------------

muxMatrix
	:: forall sig x a
	 . (Signal sig, Size x, Rep x, Rep a)
	=> sig (Matrix x a)
	-> sig x
	-> sig a
muxMatrix = (.!.)
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
			     (entity2 (Name "Lava" "index") xe me) -- order reversed
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

boolOp :: forall a sig . (Rep a, Signal sig) => String -> (a -> a -> Bool) -> sig a -> sig a -> sig Bool
boolOp nm fn =
	liftS2 $ \ (Comb a ea) (Comb b eb) ->
		    Comb (optX $ do a' <- unX a
			            b' <- unX b
			            return $ a' `fn` b')
		      (entity2 (Name (wireName (error "boolOp" :: a)) nm) ea eb)

infix 4 .==., .>=., .<=., .<., .>.
(.==.) :: forall a sig . (Rep a, Eq a, Signal sig) => sig a -> sig a -> sig Bool
(.==.) = boolOp ".==." (==)

(.>=.) :: forall a sig . (Rep a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.>=.) = boolOp ".>=." (>=)

(.<=.) :: forall a sig . (Rep a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.<=.) = boolOp ".<=." (<=)

(.<.) :: forall a sig . (Rep a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.<.) = boolOp ".<." (<)

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
type Env c = ()

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

{-
register :: forall a. (Rep a) => Env -> Comb a -> Seq a -> Seq a
register (Env (Clk clk)   c@(Comb def edef) l@(Seq line eline) = res
   where
	res = Seq sres (D $ Port ("o0") $ E $ entity)
	sres = S.zipWith (\ i v ->
				case unX i :: Maybe Bool of
				   Nothing -> optX (Nothing :: Maybe a)
				   Just (True) -> def
				   Just (False) -> v
			 ) (seqValue rst) (optX (Nothing :: Maybe a) :~ line)
        entity = Entity (Name "Memory" "register")
                    [("o0", bitTypeOf res)]
                    [("def", bitTypeOf res, unD $ edef),
		     ("i0", bitTypeOf res, unD eline),
		     ("rst", RstTy, unD $ seqDriver $ rst),
		     ("clk", ClkTy, Pad ("clk"))] []

-}

{-



delay :: forall a . (Rep a) => Seq a -> Seq a
delay = register' low undefinedComb
-}

-- a delay is a register with no defined default / initial value.

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
		     ("env",ClkDomTy, unD $ (clock :: D clk))
		    ]

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
		     ("env",ClkDomTy, unD $ (clock :: D clk))
		    ]


-- hack
--ans = delay sysEnv 99 ((shallowSeq $ S.fromList $ map (optX . Just) [(1::Int)..100]) :: Seq Int)

{-
instance Wire SysEnv where
	type X SysEnv 	= (X Clk, X Rst)
        optX _ = error "Wire.SysEnv(optX)"
        unX _ = error "Wire.SystEnv(unX)"
	wireName _	= "SysEnv"
	repType _	= TupleTy [ClkTy, RstTy]

instance Rep SysEnv where
  type WIDTH SysEnv = X2
  toWireRep _ = error "Rep.SysEnv(toWireRep)"
  fromWireRep _ = error "Rep.SysEnv(fromRep)"
  showRep _ = error "Rep.SysEnv(showRep)"


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
	pack m = liftSL (\ ms -> let sh :: Matrix ix (WireVal Bool)
				     sh = M.fromList [ m | Comb (XBool m) _ <- ms ]
				     de = entityN (Name "Lava" "concat") [ d | Comb _ d <- ms ]
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
		         (entity2 (Name "Lava" "spliceStdLogicVector") (D $ Generic (fromIntegral i) :: D Integer) ea)
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
			     (entity2 (Name "Lava" "concat") ea eb)
-}

appendStdLogicVector :: forall sig a b . (Signal sig, Size a, Size b, Size (ADD a b))
	=> sig (StdLogicVector a)
	-> sig (StdLogicVector b)
	-> sig (StdLogicVector (ADD a b))
appendStdLogicVector = liftS2 $ \ (Comb a ea) (Comb b eb) ->
			Comb (optX $ do a' <- unX a
					b' <- unX b
					return $ SLV.appendSLV a' b')
			     (entity2 (Name "Lava" "concat") ea eb)



-}

-- This is the funny one, needed for our application
--instance (Enum ix, Size ix, Integral m, Size m) => StdLogic (Sampled.Sampled m ix) where
--	type WIDTH (Sampled.Sampled m ix) = m

-- Move this to a better place.
instance (Enum ix, Size m, Size ix) => Rep (Sampled.Sampled m ix) where
        type W (Sampled.Sampled m ix) = ix
	data X (Sampled.Sampled m ix) = XSampled (WireVal (Sampled.Sampled m ix))
	optX (Just b)	    = XSampled $ return b
	optX Nothing	    = XSampled $ fail "Wire Sampled"
	unX (XSampled (WireVal a))     = return a
	unX (XSampled WireUnknown)   = fail "Wire Sampled"
	repType _   	    = SampledTy (size (error "witness" :: m)) (size (error "witness" :: ix))
	toRep (XSampled WireUnknown) = unknownRepValue (Witness :: Witness (Sampled.Sampled m ix))
	toRep (XSampled (WireVal a))   = RepValue $ fmap WireVal $ M.toList $ Sampled.toMatrix a
	fromRep r = optX (liftM (Sampled.fromMatrix . M.fromList) $ getValidRepValue r)
	showRep = showRepDefault

-------------------------------------------------------------------------------------

factor :: forall a a1 a2 sig . (Signal sig, Rep a, Rep a1, Rep a2, W a ~ ADD (W a1) (W a2)) => sig a -> (sig a1, sig a2)
factor a = unpack (coerce a :: sig (a1,a2))

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


lavaId :: (Signal sig, Rep a) => sig a -> sig a
lavaId = fun1 "id" id

-------------------------------------------------------------------------------------

-- | 'ignoring' is used to make sure a value is reified.

ignoring :: (Signal sig, Rep a, Rep b) => sig a -> sig b -> sig a
ignoring = fun2 "const" const

-------------------------------------------------------------------------------------

cASE :: (Rep b, Signal seq) => [(seq Bool,seq b)] -> seq b -> seq b
cASE [] def = def
cASE ((p,e):pes) def = mux2 p (e,cASE pes def)

-------------------------------------------------------------------------------------
--
-- if Nothing, take whole list, otherwise, normal take with the Int inside the Just
takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe = maybe id take

-------------------------------------------------------------------------------------


coerceX :: forall a b . (Rep a, Rep b, W a ~ W b ) => X a -> X b
coerceX = id
       . fromRep
       . toRep

-- Comment from Nick Frisby: Somewhere, an angel has lost its wings!
-- We need to read up and figure out something better.

-- ^ translate using raw underlying bits, Width *must* be the same.
coerce :: forall sig a b . (Signal sig, Rep a, Rep b, W a ~ W b) => sig a -> sig b
coerce = liftS1 $ \ (Comb a ae) -> Comb (coerceX a) $ entity1 (Prim "coerce") ae

-- ^ translate using raw underlying bits, Width *must* be the same,
-- but is not statically checked.
unsafeCoerce :: forall sig a b . (Signal sig, Rep a, Rep b) => sig a -> sig b
unsafeCoerce = liftS1 $ \ (Comb a ae) -> Comb (fromRep $ toRep a) $ entity1 (Prim "coerce") ae


signedX :: forall a b . (Rep a, Rep b) => X a -> X b
signedX = id
       . fromRep
       . RepValue
       . (\ m -> take (repWidth (Witness :: Witness b)) (m ++ repeat (last m)))  -- not signed extended!
       . unRepValue
       . toRep

-- | consider the bits as signed number (sign extend)
signed :: (Rep a, Rep b, Signal sig)  => sig a -> sig b
signed = liftS1 $ \ (Comb a ae) -> Comb (signedX a) $ entity1 (Prim "signed") ae

unsignedX :: forall a b . (Rep a, Rep b) => X a -> X b
unsignedX = id
       . fromRep
       . RepValue
       . (\ m -> take (repWidth (Witness :: Witness b)) (m ++ repeat (WireVal False)))  -- not signed extended!
       . unRepValue
       . toRep

-- | consider the bits an unsigned number (zero extend)
unsigned :: (Rep a, Rep b, Signal sig)  => sig a -> sig b
unsigned = liftS1 $ \ (Comb a ae) -> Comb (unsignedX a) $ entity1 (Prim "unsigned") ae

----------------------------------------------------------------------------

append :: forall sig a b c . (Signal sig, Rep a, Rep b, Rep c, W c ~ ADD (W a) (W b)) => sig a -> sig b -> sig c
append x y = coerce (pack (x,y) :: sig (a,b))



