{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Signal where


import Data.Word
import Data.Int
import Data.Bits
import Data.List

import Data.Reify
import qualified Data.Traversable as T
import Language.KansasLava.Type

import Language.KansasLava.Entity
import Language.KansasLava.Stream as S

import Control.Applicative
import Data.Sized.Unsigned as UNSIGNED
import Data.Sized.Signed as SIGNED
import Data.Sized.Sampled as SAMPLED
import Data.Sized.Arith as Arith
import Data.Sized.Ix as X

import Language.KansasLava.K
import Language.KansasLava.E
import Language.KansasLava.Wire

-----------------------------------------------------------------------------------------------

data Signal a = Signal (Seq (X a)) (D a)

signalValue :: Signal a -> Seq (X a)
signalValue (Signal a d) = a

signalDriver :: Signal a -> D a
signalDriver (Signal a d) = d

instance forall a . (RepWire a, Show a) => Show (Signal a) where
	show (Signal (Constant a) _) = showRepWire (undefined :: a) a
	show (Signal vs _)
         	= unwords [ showRepWire (undefined :: a) x ++ " :~ "
                          | x <- take 20 $ toList vs
                          ] ++ "..."

instance forall a . (Wire a, Eq a) => Eq (Signal a) where
	-- Silly question; never True; can be False.
	(Signal x _) == (Signal y _) = (fmap unX x :: Seq (Maybe a)) == (fmap unX y :: Seq (Maybe a))

deepSignal :: D a -> Signal a
deepSignal d = Signal (error "incorrect use of shallow signal") d

shallowSignal :: Seq (X a) -> Signal a
shallowSignal s = Signal s (error "incorrect use of deep signal")

instance SIGNAL Signal where
  liftS0 (K a e) = Signal (pure a) e

  liftS1 f (Signal a ea) = Signal (fmap f' a) eb
      where
	K _ eb = f (deepK ea)
	f' a = let (K b _) = f (shallowK a) 
	       in b
  liftS2 f (Signal a ea) (Signal b eb) = Signal (S.zipWith f' a b) ec
      where
	K _ ec = f (deepK ea) (deepK eb)
	f' a b = let (K c _) = f (shallowK a) (shallowK b) 
	         in c


liftS3 :: (K a -> K b -> K c -> K d) -> Signal a -> Signal b -> Signal c -> Signal d
liftS3 f ~(Signal a ea) ~(Signal b eb) ~(Signal c ec) = Signal (pure f' <*> a <*> b <*> c) ex
      where
	K _ ex = f (deepK ea) (deepK eb) (deepK ec)
	f' a b c = let ~(K x _) = f (shallowK a) (shallowK b) (shallowK c)
	           in x

--  liftSL f sigs = undefined

-- 	(K (error "liftD1, f's arg, Signal") ea)
{-
  liftS2 f (Signal a ea) (Signal b eb) = Signal (liftA2 (apply2 f) a b) ec
      where
	K _ ec = f (K (error "liftD2, f's arg, Signal") ea)
		   (K (error "liftD2, f's arg, Signal") eb)
-}

{-
--------------------------------------------------------------------------------------------
-- an obserable, konstant(sic.) value. Not a functor, applicative functor, or monad.
data K a = K a (Driver E)
	deriving Show

deepK :: Driver E -> K a
deepK e = K (error "shallow argument being used incorrectly") e

shallowK :: a -> K a
shallowK a = K a (error "deep argument being used incorrectly")

apply0 :: K a -> a
apply0 (K a _) = a 

apply1 :: (K a -> K b) -> a -> b
apply1 f a = b
   where K b _ = f (K a (error "deep embedding problem in apply1"))
	       
apply2 :: (K a -> K b -> K c) -> a -> b -> c
apply2 f a b = c
   where K c _ = f (K a (error "deep embedding problem in apply2"))
	           (K b (error "deep embedding problem in apply2"))
	       

-----------------------------------------------------------------------------------------------

k :: (Konstant a) => a -> K a
k = pureD
	
class Konstant a where
  pureD :: (Deliver f) => a -> f a

-- Not sure about liftD.
class Deliver f where
  liftD0 :: K a -> f a
  liftD1 :: (K a -> K b) -> f a -> f b
  liftD2 :: (K a -> K b -> K c) ->  f a -> f b -> f c


instance Deliver K where
  liftD0 a = a
  liftD1 f a = f a
  liftD2 f a b = f a b

-----------------------------------------------------------------------------------------------
{-
instance Konstant Bool where
  pureD True = liftD0 $ true
  pureD False = liftD0 $ false

true :: K Bool
true = K True (Lit 1)
	
false :: K Bool
false = K True (Lit 0)
-}

instance Enum x => Konstant x where
  pureD n = liftD0 $ K n (Lit $ toInteger (fromEnum n))

-----------------------------------------------------------------------------------------------

{-
instance Deliver Signal where
  liftD0 (K a e) = Signal (pure a) e
  liftD1 f (Signal a ea) = Signal (fmap (apply1 f) a) eb
      where
	K _ eb = f (K (error "liftD1, f's arg, Signal") ea)
  liftD2 f (Signal a ea) (Signal b eb) = Signal (liftA2 (apply2 f) a b) ec
      where
	K _ ec = f (K (error "liftD2, f's arg, Signal") ea)
		   (K (error "liftD2, f's arg, Signal") eb)

{-
entity0 :: Name -> BaseTy -> Driver E
entity0 nm ty = Port (Var "o0") $ E $ Entity nm
					[(Var "o0",ty)] 
					[]
					[]

entity1 :: Name -> BaseTy -> Driver E -> BaseTy -> Driver E
entity1 nm ty1 e ty2 = Port (Var "o0") $ E $ Entity nm
							[(Var "o0",ty2)] 
							[(Var "i0",ty1,e)]
							[]

-}

-}
-----------------------------------------------------------------------------------------------
-- AJG: to consider, adding AF and Functor to this type.

data Signal a = Signal (Seq (X a)) (Driver E)
-- newtype Wire = Wire (Driver E)

newtype E = E (Entity BaseTy E)

-- internal, special use only (when defining entities, for example).
data ESignal a = ESignal (Seq a) E

-- You want to observe
instance MuRef E where
  type DeRef E = Entity BaseTy
  mapDeRef f (E s) = T.traverse f s

-- not sure about this, should this use the shallow part?
instance Eq (Signal a) where
   (Signal _ s1) == (Signal _ s2) = s1 == s2

{-
instance (Show a, OpType a) => Show (Signal a) where
    show (Signal v _) = showSeq 20 v

showSignal :: (Show (X a), OpType a) => Int -> Signal a -> String
showSignal n (Signal v _) = showSeq n v
-}
instance Show E where
    show (E s) = show s

instance Eq E where
   (E s1) == (E s2) = s1 == s2

signalDriver :: Signal a -> Driver E
signalDriver (Signal _ d) = d

{-
------------------------------------------------------------------

-- Should be type
{-
data WireType
	= Bit
	| Wires Int		-- number of wires (vector)
	deriving (Show, Eq, Ord)
-}

------------------------------------------------------------------

{- ideas:
    -- About representation
    toOp :: Integer -> a
    fromOp :: a -> Integer
    -- About boundedness
    allValues :: [a]
-}

-}

-- TODO: Rename as LavaRep 
class OpType a where
    -- This is a structure that introduces 'X', or unknown.
    type X a

    unX :: X a -> Maybe a

    -- op generates the 'module' name for a given type
    op :: Signal a -> (String -> Name)
    -- bitTypeOf returns the base netlist type of signal. Good for
    -- when you have a witness value.
    bitTypeOf :: Signal a -> BaseTy

    -- tyRep returns the base netlist type of a value. Good for when
    -- you don't have a witness value.
    tyRep :: a -> BaseTy
    tyRep _ = bitTypeOf (error "OpType.tyRep" :: Signal a)

    -- A 'default' Signal for a value type.
    initVal :: Signal a		-- todo, remove this


unX' = undefined
unX' :: (OpType a) => X a -> Maybe a


    -- A show that gets n items of a sequence
--showSeq :: (OpType a, b ~ X a) => Int -> Seq b -> String
showSeq :: forall a . (Show a, OpType a) => Int -> Seq (X a) -> String
showSeq n strm =
	case strm of
	   Constant v -> show ((unX' v) :: Maybe a)
{-
	   _ -> unwords [ showX x ++ " :~ "
                        | x <- take n $ toList strm
                        ] ++ "..."
:-}

{-

a

instance OpType Int    where op _ nm = Name "Int" nm
                             bitTypeOf _ = S 32
                             initVal = Signal (pure 0) $ Lit 0
instance OpType Float  where op _ nm = Name "Float" nm
                             bitTypeOf _ = S 32
                             initVal = Signal (pure 0) $ Lit 0
instance OpType Double where op _ nm = Name "Double" nm
                             bitTypeOf _ = S 64
                             initVal = Signal (pure 0) $ Lit 0
instance OpType Int32 where op _  nm = Name "Signed" nm
                            bitTypeOf _ = S 32
                            initVal = Signal (pure 0) $ Lit 0
instance OpType Int16 where op _  nm = Name "Signed" nm
                            bitTypeOf _ = S 16
                            initVal = Signal (pure 0) $ Lit 0
instance OpType Word32 where op _ nm = Name "Unsigned" nm
                             bitTypeOf _ = U 32
                             initVal = Signal (pure 0) $ Lit 0
instance OpType Word16 where op _ nm = Name "Unsigned" nm
                             bitTypeOf _ = U 16
                             initVal = Signal (pure 0) $ Lit 0
-}
instance OpType Bool where op _  nm = Name "Bool" nm
			   type X Bool = Maybe Bool
                           bitTypeOf _ = B
                           initVal = Signal (pure $ Just $ False) $ Lit 0
{-
			   showSeq _ (Constant Nothing) = "?"
			   showSeq _ (Constant (Just True)) = "high"
			   showSeq _ (Constant (Just False)) = "low"
			   showSeq n other =
				unwords [ showV x ++ " :~ "
                        		| x <- take n $ toList other
                        		] ++ "..."
-}
{-
instance OpType ()   where op _  nm = Name "()" nm
                           bitTypeOf _ = U 0
                           initVal = Signal (pure ()) $ Lit 0

instance (Enum a, Bounded a, Size a) => OpType (Unsigned a)
                     where op _  nm = Name "Unsigned" nm
                           bitTypeOf _ = U (1 + fromEnum (maxBound :: a))
                           initVal = Signal (pure 0) $ Lit 0
instance (Enum a, Bounded a, Size a) => OpType (Signed a)
                     where op _  nm = Name "Signed" nm
                           bitTypeOf _ = S (1 + fromEnum (maxBound :: a))
                           initVal = Signal (pure 0) $ Lit 0
instance (Enum a, Size a, Size m) => OpType (Sampled m a)
                     where op _  nm = Name "Sampled" nm
				-- really is a different type than S
				-- TODO: fix
                           bitTypeOf _ = S (size (undefined :: a))
                           initVal = Signal (pure 0) $ Lit 0

instance (OpType a, OpType b) => OpType (a,b) where
 op _ _ = error "unimplemented OpType.op (a,b)"
 bitTypeOf _ = error "unimplemented OpType.bitTypeOf (a,b)"
 initVal = error "unimplemented OpType.initVal (a,b)"



-- find the name of the type of the entity arguments.
findEntityTyModName :: (OpType a) => Entity ty a -> String
findEntityTyModName e = nm
  where
    (Name nm _) = fn e (error "findEntityTyModName")
    fn :: (OpType a) => c a -> Signal a -> Name
    fn _ s = s `op` ""

-------------------------------------------


--fun1 nm f s     = entity1 (op s nm) inputs f s
--fun2 nm f s1 s2 = entity2 (op s1 nm) inputs f s1 s2
-- | 'inpus' is a default list of input port names
defaultInputs :: [Var]
defaultInputs = [Var $ "i" ++ show (n :: Int) | n <- [0..]]

entity0 :: OpType a => Name -> [Var] -> a -> ESignal a
entity0 nm outs f
        = ESignal (pure f)
        $ E
        $ Entity nm [(o,ty) | o <- outs] [] []
  where ty = tyRep f

entity1 :: forall a b . (OpType a, OpType b) =>
           Name -> [Var] -> [Var]  -> (a -> b) -> Signal a -> ESignal b
entity1 nm ins outs f  (~(Signal vs1 w1))
        = ESignal (pure f <*> vs1)
        $ E
        $ Entity nm [(o,bTy) | o <- outs] [(inp,ty,val) | inp <- ins | ty <- [aTy] | val <- [w1]] []
   where aTy = tyRep (error "entity1" :: a)
         bTy = tyRep (error "entity1" :: b)

entity2 :: forall a b c . (OpType a, OpType b,OpType c) =>
           Name -> [Var] -> [Var]  -> (a -> b -> c) -> Signal a -> Signal b -> ESignal c
entity2 nm ins outs f (~(Signal vs1 w1)) ~(Signal vs2 w2)
        = ESignal (pure f <*> vs1 <*> vs2)
        $ E
        $ Entity nm [(o,cTy) | o <- outs] [(inp,ty,val) | inp <- ins | ty <- [aTy,bTy] | val <- [w1,w2]] []
   where aTy = tyRep (error "entity2" :: a)
         bTy = tyRep (error "entity2" :: b)
         cTy = tyRep (error "entity2" :: c)

entity3 :: forall a b c d . (OpType a, OpType b,OpType c,OpType d) =>
           Name -> [Var] -> [Var]  -> (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> ESignal d
entity3 nm ins outs f  (~(Signal vs1 w1)) ~(Signal vs2 w2) ~(Signal vs3 w3)
        = ESignal (pure f <*> vs1 <*> vs2 <*> vs3)
        $ E
        $ Entity nm [(o,dTy) | o <- outs] [(inp,ty,val) | inp <- ins | ty <- [aTy,bTy,cTy] | val <- [w1,w2,w3]] []
   where aTy = tyRep (error "entity3" :: a)
         bTy = tyRep (error "entity3" :: b)
         cTy = tyRep (error "entity3" :: c)
         dTy = tyRep (error "entity3" :: d)


mapperEntity :: forall a b . (OpType a, OpType b, Bounded a, Enum a, Enum b, Show a, Show b) => Var -> Var -> (a -> Maybe b) -> Signal a -> ESignal b
mapperEntity vIn vOut f (Signal a w) = 
	  ESignal (fmapWithFail f a)
	$ E 
	$ Table (vOut,oTy) (vIn,iTy,w) mapping
   where
	mapping = [ (fromEnum a,show a,fromEnum r, show r) | a <- [minBound..maxBound], Just r <- [f a]]
	iTy = tyRep (error "" :: a)
	oTy = tyRep (error "" :: b)

-- because this is *bounded*, it can *become* a lookup table in VHDL.
fullMap :: (Bounded a, OpType a, OpType b,  Enum a, Enum b, Show a, Show b) => (a -> b) -> Signal a -> Signal b
fullMap f sig = o0 $ mapperEntity (Var "i0") (Var "o0") (Just . f) sig

fullMapWithMaybe :: (Bounded a, OpType a, OpType b,  Enum a, Enum b, Show a, Show b) => (a -> Maybe b) -> Signal a -> Signal b
fullMapWithMaybe f sig = o0 $ mapperEntity (Var "i0") (Var "o0") f sig


{-
entityM :: Name -> [Var] -> [Var] -> [[Ty Var]] -> (Matrix ix a -> b) -> Signal (Matrix ix a) -> Signal b
entityM nm ins outs tyeqs f  s@(~(Signal vs1 w1))
        = ESignal (pure f <*> vs1)
        $ E
        $ Entity nm outs (zip ins [w1,w2,w3]) tyeqs
-}

o0 :: ESignal a -> Signal a
o0 ~(ESignal v e) = Signal v (Port (Var "o0") e)

proj :: Var -> (a -> b) -> ESignal a -> Signal b
proj var f ~(ESignal v e) = Signal (fmap f v) (Port var e)

fun1 :: (OpType a1, OpType a) => String -> (a1 -> a) -> Signal a1 -> Signal a
fun1 nm f s     = o0 $ entity1 (op s nm) defaultInputs [Var "o0"]  f s

fun2 :: (OpType a1, OpType b, OpType a) =>
        String -> (a1 -> b -> a) -> Signal a1 -> Signal b -> Signal a
fun2 nm f s1 s2 = o0 $ entity2 (op s1 nm) defaultInputs [Var "o0"]  f s1 s2




instance (Num a, OpType a) => Num (Signal a) where
    s1 + s2 = fun2 "+" (+) s1 s2
    s1 - s2 = fun2 "-" (-) s1 s2
    s1 * s2 = fun2 "*" (*) s1 s2
    negate s = fun1 "negate" (negate) s
    abs s    = fun1 "abs"    (abs)    s
    signum s = fun1 "signum" (signum) s
    fromInteger n               = s
            where s = Signal (pure (fromInteger n))
                    $ Lit $ n

instance (Bits a, OpType a) => Bits (Signal a) where
    s1 .&. s2 = fun2 ".&." (.&.) s1 s2
    s1 .|. s2 = fun2 ".|." (.|.) s1 s2
    s1 `xor` s2 = fun2 "xor" (xor) s1 s2
    s1 `shift` n = fun2 "shift" (shift) s1 (fromIntegral n)
    s1 `rotate` n = fun2 "rotate" (rotate) s1 (fromIntegral n)
    complement s = fun1 "complement" (complement) s
    bitSize s                       = baseTypeLength (bitTypeOf s)
    isSigned s                      = baseTypeIsSigned (bitTypeOf s)

{-
-- TODO: represent in terms of new fun/entity interface.

instance (Fractional a, OpType a) => Fractional (Signal a) where
    s@(Signal s1) / (Signal s2) = Signal $ Wire $ Entity (op s "/")     [s1,s2]
    recip s@(Signal s1)         = Signal $ Wire $ Entity (op s "recip") [s1]
    fromRational r              = s
            where s = Signal $ Wire $ Entity (op s "fromRational")
                                                -- The :% arguments are tupled here
                                    [ Wire $ Lit $ numerator r
                                    , Wire $ Lit $ denominator r
                                    ]


instance (Floating a, OpType a) => Floating (Signal a) where
    pi 				= s where s = Signal $ Wire $ Pad (op s "pi")
    exp s@(Signal s1)         = Signal $ Wire $ Entity (op s "exp") [s1]
    sqrt s@(Signal s1)         = Signal $ Wire $ Entity (op s "sqrt") [s1]
    log s@(Signal s1)         = Signal $ Wire $ Entity (op s "log") [s1]

    sin s@(Signal s1)         = Signal $ Wire $ Entity (op s "sin") [s1]
    tan s@(Signal s1)         = Signal $ Wire $ Entity (op s "tan") [s1]
    cos s@(Signal s1)         = Signal $ Wire $ Entity (op s "cos") [s1]
    asin s@(Signal s1)         = Signal $ Wire $ Entity (op s "asin") [s1]
    atan s@(Signal s1)         = Signal $ Wire $ Entity (op s "atan") [s1]
    acos s@(Signal s1)         = Signal $ Wire $ Entity (op s "acos") [s1]
    sinh s@(Signal s1)         = Signal $ Wire $ Entity (op s "sinh") [s1]
    tanh s@(Signal s1)         = Signal $ Wire $ Entity (op s "tanh") [s1]
    cosh s@(Signal s1)         = Signal $ Wire $ Entity (op s "cosh") [s1]
    asinh s@(Signal s1)         = Signal $ Wire $ Entity (op s "asinh") [s1]
    atanh s@(Signal s1)         = Signal $ Wire $ Entity (op s "atanh") [s1]
    acosh s@(Signal s1)         = Signal $ Wire $ Entity (op s "acosh") [s1]
-}

class Explode e where
  type Ex e
  explode :: ESignal e -> Ex e
  portsByType :: Signal e -> [String]           -- depends on the *type*, not the contents

class Explode e => Implode e where
  join :: Ex e -> Signal e

-- TODO: somehow wire in better names than o1 and o2.
instance Explode (a,b) where
  type Ex (a,b) = (Signal a,Signal b)
  explode ~(ESignal v w) =
        ( Signal (fmap fst v) $ Port (Var "o1") w
        , Signal (fmap snd v) $ Port (Var "o2") w
        )
  portsByType _ = ["1","2"]


instance (OpType a, OpType b, OpType (a,b)) => Implode (a,b) where
  join (s1,s2) = o0 $ entity2 (Name "$POLY" "join")
                           [Var "s1",Var "s2"]
                           [Var "o0"]
                           (,)
                           s1 s2

split :: (OpType e, Explode e) => Signal e -> Ex e
split e = explode entity
  where entity = entity1 (Name "$POLY" "split")
                         [Var "i0"]
                         (map Var (portsByType e))
                         id
                         e


-- Signal (a -> b) -> Signal a -> Signal b



{-
  implode (~(Signal v1 w1),~(Signal v2 w2)) =
        Signal ((,) <$> v1 <*> v2) $ Wire $ Entity (Name "$" "implode") [w1,w2]
-}
{-
implode2 :: (Signal a, Signal b) -> Signal (a,b)
implode2 = implode
-}


{-

entity JK_FF is
port (	clock:		in std_logic;
	J, K:		in std_logic;
	reset:		in std_logic;
	Q, Qbar:	out std_logic
);
end JK_FF;


	$(entity [ "clock : in std_logic     // Signal Bool"
		 , "J, K : std_vector(31..0) // Signal Int"
		 ])
-}

class CLONE a where
  clone :: a -> a -> a		-- take the shallow from the first, and the deep from the second

instance CLONE (Signal a) where
  clone ~(Signal s _) ~(Signal _ d) = Signal s d

-- AJG: why does this not need CLONE a?
instance (CLONE b) => CLONE (a -> b) where
  clone f1 f2 = \ a -> clone (f1 a) (f2 a)

instance (CLONE a,CLONE b) => CLONE (a,b) where
  clone ~(a1,b1) ~(a2,b2) = (clone a1 a2,clone b1 b2)

-}

-}