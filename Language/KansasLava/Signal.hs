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
import Language.KansasLava.Seq as S

import Control.Applicative
import Data.Sized.Unsigned as UNSIGNED
import Data.Sized.Signed as SIGNED
import Data.Sized.Ix as X

import Test.QuickCheck hiding ((.&.))

-- AJG: to consider, adding AF and Functor to this type.

data Signal a = Signal (Seq a) (Driver E)
-- newtype Wire = Wire (Driver E)

newtype E = E (Entity BaseTy E)

-- internal, special use only (when defining entities, for example).
data ESignal a = ESignal (Seq a) E

-- You want to observe
instance MuRef E where
  type DeRef E = Entity BaseTy
  mapDeRef f (E s) = T.traverse f s

-- not sure about this, should this use the shallow part?
-- ACF: calling them equal if either embedding is equal for the moment
instance (Eq a, Show a) => Eq (Signal a) where
   (Signal s1 d1) == (Signal s2 d2) = (s1 == s2) || (d1 == d2)

instance (Show a, OpType a) => Show (Signal a) where
    show (Signal v _) = showSeq 20 v

showSignal :: (Show a, OpType a) => Int -> Signal a -> String
showSignal n (Signal v _) = showSeq n v

instance Show E where
    show (E s) = show s

instance Eq E where
   (E s1) == (E s2) = s1 == s2

------------------------------------------------------------------

-- Should be type
{-
data WireType
	= Bit
	| Wires Int		-- number of wires (vector)
	deriving (Show, Eq, Ord)
-}

------------------------------------------------------------------

class OpType a where
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
    showSeq :: (Show a) => Int -> Seq a -> String
    showSeq n strm =
	case strm of
	   Constant v -> showV v
	   _ -> unwords [ showV x ++ " :~ "
                        | x <- take n $ toList strm
                        ] ++ "..."





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
instance OpType Bool where op _  nm = Name "Bool" nm
                           bitTypeOf _ = B
                           initVal = Signal (pure False) $ Lit 0
			   showSeq _ (Constant Nothing) = "?"
			   showSeq _ (Constant (Just True)) = "high"
			   showSeq _ (Constant (Just False)) = "low"
			   showSeq n other =
				unwords [ showV x ++ " :~ "
                        		| x <- take n $ toList other
                        		] ++ "..."
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

-- For QuickCheck
instance (Arbitrary a) => Arbitrary (Signal a) where
    -- TODO: figure out how to actually handle the deep embedding here
    arbitrary = Signal <$> arbitrary <*> (return (Lit 1))

