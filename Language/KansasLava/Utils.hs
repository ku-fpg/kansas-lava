{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ParallelListComp, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses  #-}

module Language.KansasLava.Utils where
	
import Language.KansasLava.Entity as E
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Data.Sized.Matrix	as M
import Data.Sized.Unsigned	as U

import Control.Applicative
import Control.Monad
import Data.Word
import Data.Bits
import Data.Ratio
import Debug.Trace

-----------------------------------------------------------------------------------------------

instance Constant Bool where
  pureS v = liftS0 $ Comb (pureX v) $ D $ Lit $ fromIntegral $ U.fromMatrix $ fromWireRep v

instance Constant Int where
  pureS v = liftS0 $ Comb (pureX v) $ D $ Lit $ fromIntegral v

instance Constant Word32 where
  pureS v = liftS0 $ Comb (pureX v) $ D $ Lit $ fromIntegral v

instance Constant Integer where
  pureS v = liftS0 $ Comb (pureX v) $ D $ Lit v

instance (Enum ix, Size ix) => Constant (Unsigned ix) where
  pureS v = liftS0 $ Comb (pureX v) $ D $ error "Unsigned IX"


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

-- TODO: maCombe over Signal
(.!.) :: (Size x, Wire a, Wire x) => Comb (Matrix x a) -> Comb x -> Comb a
(.!.) = fun2 "!" (!)

-----------------------------------------------------------------------------------------------

instance (Constant a, Show a, RepWire a, Num a) => Num (Comb a) where
    s1 + s2 = fun2 "+" (+) s1 s2
    s1 - s2 = fun2 "-" (-) s1 s2
    s1 * s2 = fun2 "*" (*) s1 s2
    negate s = fun1 "negate" (negate) s
    abs s    = fun1 "abs"    (abs)    s
    signum s = fun1 "signum" (signum) s
    fromInteger n = pureS (fromInteger n)

instance (Constant a, Show a, RepWire a, Num a) => Num (Seq a) where
    (+) = liftS2 (+)
    (-) = liftS2 (-)
    (*) = liftS2 (*)
    negate = liftS1 negate
    abs = liftS1 abs
    signum = liftS1 signum
    fromInteger n = pureS (fromInteger n)

instance (Constant a, Show a, Bits a, RepWire a) 
	=> Bits (Comb a) where
    s1 .&. s2 = fun2 ".&." (.&.) s1 s2
    s1 .|. s2 = fun2 ".|." (.|.) s1 s2
    s1 `xor` s2 = fun2 "xor" (xor) s1 s2
    s1 `shift` n = fun2 "shift" (shift) s1 (fromIntegral n)
    s1 `rotate` n = fun2 "rotate" (rotate) s1 (fromIntegral n)
    complement s = fun1 "complement" (complement) s
    bitSize s                       = baseTypeLength (bitTypeOf s)
    isSigned s                      = baseTypeIsSigned (bitTypeOf s)

instance (Constant a, Show a, Bits a, RepWire a) 
	=> Bits (Seq a) where
    (.&.)   = liftS2 (.&.)
    (.|.)  = liftS2 (.|.)
    xor    = liftS2 (xor)
    shift s n = liftS1 (flip shift n) s
    rotate s n = liftS1 (flip rotate n) s
    complement = liftS1 complement
    bitSize s                       = baseTypeLength (bitTypeOf s)
    isSigned s                      = baseTypeIsSigned (bitTypeOf s)

instance (Constant a, Eq a, Show a, Fractional a, RepWire a) => Fractional (Comb a) where
    s1 / s2 = fun2 "/" (/) s1 s2
    recip s1 = fun1 "recip" (recip) s1 
    fromRational r = fun2 "fromRational" (\ x y -> fromRational (x % y)) (pureS $ numerator r) (pureS $ denominator r)

instance (Constant a, Eq a, Show a, Fractional a, RepWire a) => Fractional (Seq a) where
    (/) = liftS2 (/)
    recip = liftS1 recip
    fromRational r = fun2 "fromRational" (\ x y -> fromRational (x % y)) (pureS $ numerator r) (pureS $ denominator r)
   
	
-----------------------------------------------------------------------------------------------
-- And the utilties that get this done.

fun0 :: forall a sig . (Signal sig, Wire a) => String -> a -> sig a
fun0 nm a = liftS0 $ Comb (optX $ Just $ a) $ entity0 (Name (wireName (error "fun1" :: a)) nm)

fun1 :: forall a b sig . (Signal sig, Wire a, Wire b) => String -> (a -> b) -> sig a -> sig b
fun1 nm f = liftS1 $ \ (Comb a ae) -> Comb (optX $ liftA f (unX a)) $ entity1 (Name (wireName (error "fun1" :: b)) nm) ae

fun2 :: forall a b c sig . (Signal sig, Wire a, Wire b, Wire c) => String -> (a -> b -> c) -> sig a -> sig b -> sig c
fun2 nm f = liftS2 $ \ (Comb a ae) (Comb b be) -> Comb (optX $ liftA2 f (unX a) (unX b)) 
	  $ entity2 (Name (wireName (error "fun2" :: c)) nm) ae be

table :: forall sig a b . (Enum (WIDTH a), Enum (WIDTH b), Size (WIDTH a), Size (WIDTH b), Signal sig, RepWire a, RepWire b) => [(a,b)] -> sig a -> sig b
table tab = liftS1 $ \ (Comb a (D ae))
				-> Comb (case unX (a :: X a) :: Maybe a of
					Nothing -> optX (Nothing :: Maybe b) :: X b
					Just v -> 
					  case lookup v tab of
					    Nothing -> optX (Nothing :: Maybe b) :: X b
					    Just b -> optX (Just b :: Maybe b) :: X b
				     ) 
				     (D $ Port (Var "o0") 
					$ E 
					$ Table (Var "o0",tA)
						(Var "i0",tB,ae)
						[( fromIntegral $ U.fromMatrix $ fromWireRep a
						 , showRepWire a $ optX $ Just a
						 , fromIntegral $ U.fromMatrix $ fromWireRep b
						 , showRepWire b $ optX $ Just b
						 )
						| (a,b) <- tab
						]
				     )
	where tA = wireType (error "table" :: a)
	      tB = wireType (error "table" :: b)

entity0 :: forall o . (Wire o) => Name -> D o
entity0 nm = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  []
		  []
   where oTy = wireType (error "entity0" :: o)

entity1 :: forall a o . (Wire a, Wire o) => Name -> D a -> D o
entity1 nm (D w1) = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  [(inp,ty,val) | inp <- map Var ["i0","i1"] 
				| ty <- [aTy] 
				| val <- [w1]
		  ] []
   where aTy = wireType (error "entity1" :: a)
         oTy = wireType (error "entity1" :: o)

entity2 :: forall a b o . (Wire a, Wire b, Wire o) => Name -> D a -> D b -> D o
entity2 nm (D w1) (D w2) = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  [(inp,ty,val) | inp <- map Var ["i0","i1"]
				| ty <- [aTy,bTy] 
				| val <- [w1,w2]
		  ] []
   where aTy = wireType (error "entity2" :: a)
         bTy = wireType (error "entity2" :: b)
         oTy = wireType (error "entity2" :: o)

entity3 :: forall a b c o . (Wire a, Wire b, Wire c, Wire o) => Name -> D a -> D b -> D c -> D o
entity3 nm (D w1) (D w2) (D w3) = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  [(inp,ty,val) | inp <- map Var ["i0","i1","i2"]
				| ty <- [aTy,bTy,cTy] 
				| val <- [w1,w2,w3]
		  ] []
   where aTy = wireType (error "entity3" :: a)
         bTy = wireType (error "entity3" :: b)
         cTy = wireType (error "entity3" :: c)
         oTy = wireType (error "entity3" :: o)

entityN :: forall a b o . (Wire a, Wire o) => Name -> [D a] -> D o
entityN nm ds = D $ Port (Var "o0") $ E $
 	Entity nm [(Var "o0",oTy)]
		  [(inp,ty,val) | inp <- map Var ["i" ++ show n | n <- [0..]]
				| ty <- repeat aTy
				| val <- [w | D w <- ds]
		  ] []
   where aTy = wireType (error "entity2" :: a)
         oTy = wireType (error "entity2" :: o)



-----------------------------------------------------------------------------------------------
class (Signal sig) => Pack sig a where
 type Unpacked sig a
 pack :: Unpacked sig a -> sig a
 unpack :: sig a -> Unpacked sig a

instance (Wire a, Wire b, Signal sig) => Pack sig (a,b) where 
	type Unpacked sig (a,b) = (sig a, sig b)
	pack ~(a,b) = liftS2 (\ ~(Comb a ae) ~(Comb b be) -> Comb (a,b) (entity2 (Name "Lava" "pair") ae be))
			    a b
	unpack ab = ( liftS1 (\ (Comb (~(a,b)) abe) -> Comb a (entity1 (Name "Lava" "fst") abe)) ab
		    , liftS1 (\ (Comb (~(a,b)) abe) -> Comb b (entity1 (Name "Lava" "snd") abe)) ab
		    )

instance (Wire a, Signal sig, Integral ix, Num ix, Size ix) => Pack sig (Matrix ix a) where 
	type Unpacked sig (Matrix ix a) = Matrix ix (sig a)
	pack m = liftSL (\ ms -> let sh = M.fromList [ m | Comb m  _ <- ms ] 
				     de = entityN (Name "Lava" "concat") [ d | Comb _ d <- ms ]
				 in Comb sh de) (M.toList m) 
	unpack s = forAll $ \ ix -> 
			liftS1 (\ (Comb s d) -> Comb (s ! ix) 
					       (entity2 (Name "Lava" "index") 
							(D $ Lit $ fromIntegral ix :: D Integer)
							d
					       )
			        ) s

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


-- Assumping that the codomain is finite (beacause of RepWire), and *small* (< ~256 values).
funMap :: forall sig a b .
	  (Show a, Show b
	  , Signal sig, Enum (WIDTH a),
                      Enum (WIDTH b),
                      Size (WIDTH a),
                      Size (WIDTH b),
                      RepWire a,
                      RepWire b) => (a -> Maybe b) -> sig a -> sig b
funMap f a = {- trace (show count) $ -} table ({-trace (show $ map fst tab)-} tab) a
  where
   tab = [ (a,b)
	 | v <- [0..(2^count)-1]	-- all possible reps
--	 , trace (show v) False
	 , Just a <- [toWireRep $ U.toMatrix $ fromIntegral $ v]
	 , Just b <- [f a]
	 ]

   count :: Integer
   count = fromIntegral $ size (undefined :: WIDTH a)



-----------------------------------------------------------------------------------------------     
{-
liftS3 :: forall a b c d sig . (Signal sig, Wire a, Wire b, Wire c, Wire d)
       => (K a -> K b -> K c -> K d) -> sig a -> sig b -> sig c -> sig d
liftS3 f a b c = liftS2 (\ ab c -> uncurry f (unpack ab) c) (pack (a,b) :: sig (a,b)) c
-}
-----------------------------------------------------------------------------------------------     

--mux2 :: forall sig a . (Signal sig, Wire a) => sig Bool -> (sig a,sig a) -> sig a
mux2 :: forall sig a . (Signal sig, sig ~ Seq, Wire a) => sig Bool -> (sig a,sig a) -> sig a
mux2 i (t,e)
	= liftS3 (\ ~(Comb i ei) 
	 	    ~(Comb t et)
	 	    ~(Comb e ee)
			-> Comb (case unX i :: Maybe Bool of
			          Nothing -> optX (Nothing :: Maybe a)
				  Just True -> t
				  Just False -> e
			     ) 
			     undefined
--			     (entity3 (Name "Lava" "mux2") ei et ee)
	         ) i t e
---------------------------------------------------------------------


boolOp :: forall a sig . (Wire a, Signal sig) => String -> (a -> a -> Bool) -> sig a -> sig a -> sig Bool
boolOp nm fn = 
	liftS2 $ \ (Comb a ea) (Comb b eb) ->
		    Comb (optX $ do a' <- unX a :: Maybe a
			            b' <- unX b :: Maybe a
			            return $ a' `fn` b')
		      (entity2 (Name "Bool" nm) ea eb)

(.==.) :: forall a sig . (Wire a, Eq a, Signal sig) => sig a -> sig a -> sig Bool
(.==.) = boolOp ".==." (==)

(.>=.) :: forall a sig . (Wire a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.>=.) = boolOp ".>=." (>=)

(.<.) :: forall a sig . (Wire a, Ord a, Signal sig) => sig a -> sig a -> sig Bool
(.<.) = boolOp ".<." (<)

