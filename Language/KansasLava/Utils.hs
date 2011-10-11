{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ParallelListComp, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, RankNTypes  #-}
-- | This module contains a number of primitive circuits, and instance
-- definitions for standard type classes for circuits.
module Language.KansasLava.Utils where

import Control.Monad
import Data.Bits

import Language.KansasLava.Rep
import Language.KansasLava.Signal
--import Language.KansasLava.Signal
-- import Language.KansasLava.Interp
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types

import Data.Sized.Matrix	as M
import Data.Sized.Signed	as SI

-----------------------------------------------------------------------------------------------

-- | The 'Signal' representing True.
high :: (sig ~ Signal i) => sig Bool
high = pureS True

-- | The 'Signal' representing False.
low :: (sig ~ Signal i) => sig Bool
low  = pureS False

{-
-- | The constant combinational values for True.
true :: Comb Bool
true = high

-- | The constant combinational values for False.
false :: Comb Bool
false = low
-}

-----------------------------------------------------------------------------------------------
-- | 1-bit and gate.
and2 :: ( sig ~ Signal i) => sig Bool -> sig Bool -> sig Bool
and2 s1 s2 = primXS2 (\ a b -> case (unX a,unX b) of
	     (Just True,Just True) -> optX $ Just True
	     (Just False,_)        -> optX $ Just False
	     (_,Just False)        -> optX $ Just False
	     _                     -> optX $ Nothing) "and2"
         s1
         s2

-- | 1-bit or gate.
or2 :: ( sig ~ Signal i) => sig Bool -> sig Bool -> sig Bool
or2 s1 s2 = primXS2 (\ a b -> case (unX a,unX b) of
	     (Just False,Just False) -> optX $ Just False
	     (Just True,_)           -> optX $ Just True
	     (_,Just True)           -> optX $ Just True
             _                       -> optX $ Nothing ) "or2"
         s1
         s2
        
-- | 1-bit xor gate.
xor2 :: ( sig ~ Signal i) => sig Bool -> sig Bool -> sig Bool
xor2 s1 s2 = primXS2 (\ a b -> case (unX a,unX b) of
	     (Just a',Just b') -> optX $ Just (a' /= b')
             _                 -> optX $ Nothing ) "or2"
         s1
         s2
        
-- | 1 bit inverter.
bitNot :: ( sig ~ Signal i) => sig Bool -> sig Bool
bitNot s1 = primS1 not "not"  s1

-- | Extract the n'th bit of a signal that can be represented as Bits.
testABit :: forall sig a i . (Bits a, Rep a,  sig ~ Signal i) => sig a -> Int -> sig Bool
testABit (Signal a ae) i = Signal (fmap (liftX (flip testBit i)) a)
                            (entityD2 "testBit"  ae
                                                 (pureD (fromIntegral i :: Integer)))

-- | Predicate to see if a Signed value is positive.
isPositive :: forall sig i ix . (sig ~ Signal i, Size ix, Integral ix) => sig (Signed ix) -> sig Bool
isPositive a = bitNot $ testABit a msb
    where msb = bitSize a - 1

infixr 3 .&&.
infixr 2 .||.
infixr 2 .^.

-- | Alias for 'and2'.
(.&&.) :: ( sig ~ Signal i) => sig Bool -> sig Bool -> sig Bool
(.&&.) = and2

-- | Alias for 'or2'.
(.||.) :: ( sig ~ Signal i) => sig Bool -> sig Bool -> sig Bool
(.||.) = or2

-- | Alias for 'xor2'.
(.^.) :: ( sig ~ Signal i) => sig Bool -> sig Bool -> sig Bool
(.^.)  = xor2



-----------------------------------------------------------------------------------------------
-- Map Ops


-- Assumping that the domain is finite (beacause of Rep), and *small* (say, < ~256 values).

-- | Given a function over a finite domain, generate a ROM representing the
-- function. To make this feasible to implement, we assume that the domain is
-- small (< 2^8 values).
funMap :: forall sig a b i . (sig ~ Signal i, Rep a, Rep b) => (a -> Maybe b) -> sig a -> sig b
funMap fn (Signal a ae) = Signal (fmap fn' a) 
                            (D $ Port ("o0")
			       $ E
			       $ Entity (Prim "asyncRead")
					         [("o0",tB)]
						 [ ("i0",tMB,rom)
						 , ("i1",tA,unD ae)
						 ])

	where tA = repType (Witness :: Witness a)
	      tB = repType (Witness :: Witness b)
              tMB = MatrixTy (Prelude.length all_a_bitRep) tB

              undefB = unknownRepValue (Witness :: Witness b)

              fn' a' = case unX a' of
			 Nothing -> optX Nothing
			 Just v -> optX (fn v)

	      all_a_bitRep :: [RepValue]
	      all_a_bitRep = allReps (Witness :: Witness a)

              rom = Port "o0" $ E $ Entity (Prim "rom") [("o0",tMB)] [("defs",RomTy (Prelude.length all_a_bitRep),Lits lits)]

              -- assumes in order table generation
	      lits :: [RepValue]
	      lits = [ case unX (fromRep w_a) of
				 Nothing -> undefB
				 Just a' -> case fn a' of
			                    Nothing -> undefB
			                    Just b -> toRep (pureX b)
		    | w_a <- all_a_bitRep
		    ]




-----------------------------------------------------------------------------------------------

-- | Multiplexer with a 1-bit selector and arbitrary width data inputs.
-- zero (false) selects the first argument of the tuple, one (true)
-- selects the second.
mux :: forall sig a i . ( sig ~ Signal i, Rep a) => sig Bool -> (sig a,sig a) -> sig a
mux iSig (eSig,tSig) = primXS3 mux2shallow "mux2"  iSig  tSig  eSig

-- mux2 uses a hack around liftS3 to eliminate an unnecessary (unpack . pack) arising from
-- the use of liftS3. This is safe, because we know the kind of node that we're building.
-- | Multiplexer with a 1-bit selector and arbitrary width data inputs.
mux2 :: forall sig a i . ( sig ~ Signal i, Rep a) => sig Bool -> (sig a,sig a) -> sig a
mux2 iSig (tSig,eSig) = mux iSig (eSig,tSig)

-- | Shallow definition of a multiplexer. Deals with 3-value logic.
mux2shallow :: forall a . (Rep a) => X Bool -> X a -> X a -> X a
mux2shallow i e t =
   case unX i of
       Nothing -> optX Nothing
       Just True -> e
       Just False -> t



-------------------------------------------------------------------------------------------------
-- | TODO: Document this. And move it.
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
	:: forall sig x a i
	 . ( sig ~ Signal i, Size x, Rep x, Rep a)
	=> sig (Matrix x a)
	-> sig x
	-> sig a
muxMatrix = (.!.)

-- | Extract the n'th element of a vector.
(.!.)	:: forall sig x a i
	 . ( sig ~ Signal i, Size x, Rep x, Rep a)
	=> sig (Matrix x a)
	-> sig x
	-> sig a
(.!.) mSig xSig = primS2 (flip (M.!)) "index" xSig mSig
        -- order reversed on purpose

-------------------------------------------------------------------------------------------------

-- | Lift a (named) binary function over bools to be over 'Signal's.
boolOp :: forall a i sig . (Rep a,  sig ~ Signal i) => (a -> a -> Bool) -> String -> sig a -> sig a -> sig Bool
boolOp fn nm a b = primS2 fn nm  a  b

infix 4 .==., .>=., .<=., .<., .>.

-- | N-bit equality.
(.==.) :: forall a i sig . (Rep a, Eq a,  sig ~ Signal i) => sig a -> sig a -> sig Bool
(.==.) = boolOp (==) ".==."

-- | N-bit not-equals.
(./=.) :: forall a i sig . (Rep a, Eq a,  sig ~ Signal i) => sig a -> sig a -> sig Bool
(./=.) xs ys = bitNot (xs .==. ys) -- TODO: consider making this a primitive

-- | N-bit greater-than-or-equals.
(.>=.) :: forall a i sig . (Rep a, Ord a,  sig ~ Signal i) => sig a -> sig a -> sig Bool
(.>=.) = boolOp (>=) ".>=."

-- | N-bit less-than-or-equals.
(.<=.) :: forall a i sig . (Rep a, Ord a,  sig ~ Signal i) => sig a -> sig a -> sig Bool
(.<=.) = boolOp (<=) ".<=."

-- | N-bit less-than.
(.<.) :: forall a i sig . (Rep a, Ord a,  sig ~ Signal i) => sig a -> sig a -> sig Bool
(.<.) = boolOp (<) ".<."

-- | N-bit greater-than.
(.>.) :: forall a i sig . (Rep a, Ord a,  sig ~ Signal i) => sig a -> sig a -> sig Bool
(.>.) = boolOp (>) ".>."


-------------------------------------------------------------------------------

{-

-- This is the funny one, needed for our application
--instance (Enum ix, Size ix, Integral m, Size m) => StdLogic (Sampled.Sampled m ix) where
--	type WIDTH (Sampled.Sampled m ix) = m

-- Move this to a better place.

-------------------------------------------------------------------------------------


{-
	   ,  sig ~ Signal i, Rep a2, Rep a1
	   , StdLogic a, StdLogic a1, StdLogic a2) => sig a -> sig (a1,a2)
factor a = pack ( fromStdLogicVector $ extractStdLogicVector 0 vec
		 , fromStdLogicVector $ extractStdLogicVector (size (error "witness" :: WIDTH a1)) vec
		 )
	 where vec :: sig (StdLogicVector (WIDTH a))
	       vec = toStdLogicVector a
-}

-------------------------------------------------------------------------------------
-}

-- | The identity function, lifted to 'Signal's.
lavaId :: ( sig ~ Signal i, Rep a) => sig a -> sig a
lavaId a = primS1 id "id"  a



-------------------------------------------------------------------------------------

-- | 'ignoring' is used to make sure a value is reified.
-- TODO: is this used?
ignoring :: ( sig ~ Signal i, Rep a, Rep b) => sig a -> sig b -> sig a
ignoring a b = primS2 const "const" a  b


-------------------------------------------------------------------------------------

-- | Given a representable value for a discirminant and a list of input signals, generate a n-ary mux.
cASE :: (Rep b,  sig ~ Signal i) => [(sig Bool,sig b)] -> sig b -> sig b
cASE [] def = def
cASE ((p,e):pes) def = mux2 p (e,cASE pes def)


-------------------------------------------------------------------------------------


-- | translate using raw underlying bits, Width *must* be the same.
bitwise :: forall sig a b i . ( sig ~ Signal i, Rep a, Rep b, W a ~ W b) => sig a -> sig b
bitwise a = primXS1 (fromRep . toRep) "coerce"  a

-- | translate using raw underlying bits for deep, but given function for shallow, Width *must* be the same.
coerce :: forall sig a b i . ( sig ~ Signal i, Rep a, Rep b, W a ~ W b) => (a -> b) -> sig a -> sig b
coerce f a = primXS1 g "coerce"  a
  where
       g :: X a -> X b
       g x = y'
          where 
            y = optX $ liftM f $ unX x
	    y' | toRep x == toRep y = y
	       | otherwise          = error "coerce fails to preserve bit pattern"


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
signed :: (Rep a, Rep b, Num b,  sig ~ Signal i)  => sig a -> sig b
signed a = primXS1 signedX "signed"  a

-- | Consider the value as an unsigned value.
unsignedX :: forall a b . (Rep a, Rep b) => X a -> X b
unsignedX = id
       . fromRep
       . RepValue
       . (\ m -> take (repWidth (Witness :: Witness b)) (m ++ repeat (Just False)))  -- not signed extended!
       . unRepValue
       . toRep

-- | consider the bits an unsigned number (zero extend)
unsigned :: (Rep a, Rep b, Num b,  sig ~ Signal i)  => sig a -> sig b
unsigned a = primXS1 unsignedX "unsigned"  a


----------------------------------------------------------------------------
-- | translate using raw underlying bits, type  *must* be the same, but is not statically checked.
unsafeId :: forall sig a b i . ( sig ~ Signal i, Rep a, Rep b) => sig a -> sig b
unsafeId a = primXS1 (fromRep . toRep) "coerce"  a

----------------------------------------------------------------------------
-- | given a signal of a1 + a2 width, yield a signal with a pair of values of width a1 and a2 respectively.
{-
factor :: forall a a1 a2 sig . ( sig ~ Signal i, Rep a, Rep a1, Rep a2, W a ~ ADD (W a1) (W a2)) => sig a -> (sig a1, sig a2)
factor a = unpack (bitwise a :: sig (a1,a2))

-- | given two signals of a1 and a2 width, respectively, pack them into a signal of a1 + a2 width.
append :: forall sig a b c . ( sig ~ Signal i, Rep a, Rep b, Rep c, W c ~ ADD (W a) (W b)) => sig a -> sig b -> sig c
append x y = bitwise (pack (x,y) :: sig (a,b))
-}


----------------------------------------------------------------------------
-- | The first argument is the value is our value under test;
-- the second is our reference value.
-- If the reference is undefined, then the VUT *can* also be under test.
-- This only works for shallow circuits, and is used when creating test benches.
refinesFrom :: forall sig a i . ( sig ~ Signal i, Rep a) => sig a -> sig a -> sig Bool
refinesFrom a b = shallowSignal (S.zipWith fn (seqValue a) (seqValue b))
   where
           fn a' b' = let res =  and  [ case (vut,ref) of
                                           (_,Nothing)     -> True
                                           (Just x,Just y) -> x == y
                                           _               -> False
                                      | (vut,ref) <- zip (unRepValue (toRep a'))
                                                         (unRepValue (toRep b'))
                                      ]
                      in optX (Just res)

--------------------------------------------------------------------------------
-- | Create a register, pass the output of the register through some
-- combinational logic, then pass the result back into the register input.
iterateS :: (Rep a, Clock c, seq ~ Signal c)
         => (forall j . Signal j a -> Signal j a) 
         -> a -> seq a
iterateS f start = out where
        out = register start (f out)

---------------------------------------------------------------------

-- These varients of succ/pred can handle bounded values and do proper looping.
loopingInc :: (Bounded a, Num a, Rep a, sig ~ Signal i) => sig a -> sig a
loopingInc a = mux2 (a .==. maxBound) (pureS 0,a + 1)

loopingDec :: (Bounded a, Num a, Rep a, sig ~ Signal i) => sig a -> sig a
loopingDec a = mux2 (a .==. 0) (pureS maxBound,a - 1)

