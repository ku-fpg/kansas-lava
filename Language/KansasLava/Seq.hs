{-# LANGUAGE TypeFamilies, ExistentialQuantification,
    FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses #-}

module Language.KansasLava.Seq where

import System.Random
import Control.Applicative
import Data.List

import Language.KansasLava.Comb
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types

-----------------------------------------------------------------------------------------------
-- For lack of a better place at the moment.
-- Places that don't work (needs Seq and Comb): Types, Entity, Signal, Comb

-- | Select the shallow embedding from one circuit, and the deep embedding from another.
class Dual a where
    -- | Take the shallow value from the first argument, and the deep value from the second.
    dual :: a -> a -> a

instance Dual (CSeq c a) where
    dual ~(Seq a _) ~(Seq _ eb) = Seq a eb

instance Dual (Comb a) where
    -- dual ~(Comb a _) ~(Comb _ eb) = Comb a eb
    dual c d = Comb (combValue c) (combDriver d)

instance (Dual a, Dual b) => Dual (a,b) where
	dual ~(a1,b1) ~(a2,b2) = (dual a1 a2,dual b1 b2)

instance (Dual a, Dual b,Dual c) => Dual (a,b,c) where
	dual ~(a1,b1,c1) ~(a2,b2,c2) = (dual a1 a2,dual b1 b2,dual c1 c2)

instance (Dual b) => Dual (a -> b) where
	dual f1 f2 x = dual (f1 x) (f2 x)

-----------------------------------------------------------------------------------------------

-- These are sequences of values over time.
-- We assume edge triggered logic (checked at (typically) rising edge of clock)
-- This clock is assumed known, based on who is consuming the list.
-- Right now, it is global, but we think we can support multiple clocks with a bit of work.

data CSeq c a = Seq (S.Stream (X a)) (D a)

type Clocked c a = CSeq c a	-- new name, start using
type Seq a = CSeq () a

seqValue :: CSeq c a -> S.Stream (X a)
seqValue (Seq a _) = a

seqDriver :: CSeq c a -> D a
seqDriver (Seq _ d) = d

instance (Rep a, Show a) => Show (CSeq c a) where
	show (Seq vs _)
         	= concat [ showRep x ++ " "
                         | x <- take 20 $ S.toList vs
                         ] ++ "..."

instance (Rep a, Eq a) => Eq (CSeq c a) where
	-- Silly question; never True; can be False.
	(Seq _ _) == (Seq _ _) = error "undefined: Eq over a Seq"

deepSeq :: D a -> CSeq c a
deepSeq = Seq (error "incorrect use of shallow Seq")

shallowSeq :: S.Stream (X a) -> CSeq c a
shallowSeq s = Seq s (D $ Error "incorrect use of deep Seq")

undefinedSeq ::  forall a c . (Rep a) => CSeq c a
undefinedSeq = liftS0 undefinedComb

seqAll :: forall w. (Rep w) => Seq w
seqAll = toSeqX $ cycle [fromRep rep | rep <- allReps (Witness :: Witness w) ]

instance Signal (CSeq c) where
  liftS0 c = Seq (pure (combValue c)) (combDriver c)

  liftS1 f (Seq a ea) = {-# SCC "liftS1Seq" #-}
    let deep = combDriver (f (deepComb ea))
	f' = combValue . f . shallowComb
   in Seq (fmap f' a) deep

  -- We can not replace this with a version that uses packing,
  -- because *this* function is used by the pack/unpack stuff.
  liftS2 f (Seq a ea) (Seq b eb) = Seq (S.zipWith f' a b) ec
      where
	ec = combDriver $ f (deepComb ea) (deepComb eb)
	f' x y = combValue (f (shallowComb x) (shallowComb y))

  liftSL f ss = Seq (S.fromList
		    [ combValue $ f [ shallowComb x | x <- xs ]
		    | xs <- transpose [ S.toList x | Seq x _ <- ss ]
		    ])
		    (combDriver (f (map (deepComb . seqDriver) ss)))

  deepS (Seq _ d) = d

----------------------------------------------------------------------------------------------------

-- Small DSL's for declaring signals

toSeq :: (Rep a) => [a] -> CSeq c a
toSeq xs = shallowSeq (S.fromList (map optX (map Just xs ++ repeat Nothing)))

toSeq' :: (Rep a) => [Maybe a] -> CSeq c a
toSeq' xs = shallowSeq (S.fromList (map optX (xs ++ repeat Nothing)))

toSeqX :: forall a c . (Rep a) => [X a] -> CSeq c a
toSeqX xs = shallowSeq (S.fromList (xs ++ map (optX :: Maybe a -> X a) (repeat Nothing)))

takeThenSeq :: Int -> CSeq c a -> CSeq c a -> CSeq c a
takeThenSeq n sq1 sq2 = shallowSeq (S.fromList (take n (S.toList (seqValue sq1)) ++
                                                S.toList (seqValue sq2)))

encSeq :: (Rep a) =>  (Char -> Maybe a) -> String -> CSeq c a
encSeq enc xs = shallowSeq (S.fromList (map optX (map enc xs ++ repeat Nothing)))

encSeqBool :: String -> CSeq c Bool
encSeqBool = encSeq enc
	where enc 'H' = return True
	      enc 'L' = return False
	      enc  _   = Nothing

showStreamList :: forall a c . (Rep a) => CSeq c a -> [String]
showStreamList ss =
	[ showRep  x
	| x <- S.toList (seqValue ss)
	]

fromSeq :: (Rep a) => CSeq c a -> [Maybe a]
fromSeq = fmap unX . S.toList . seqValue

fromSeqX :: (Rep a) => CSeq c a -> [X a]
fromSeqX = S.toList . seqValue

cmpSeqRep :: forall a c . (Rep a) => Int -> CSeq c a -> CSeq c a -> Bool
cmpSeqRep depth s1 s2 = and $ take depth $ S.toList $ S.zipWith cmpRep
								(seqValue s1)
								(seqValue s2)

-----------------------------------------------------------------------------------

-- Do we use this any more?
-- Monomorphic box round wires.
data IsRepWire = forall a c . (Rep a) => IsRepWire (CSeq c a)

-- The raw data
showBitfile :: [IsRepWire] -> [String]
showBitfile streams =
	      [ concat bits
	      | bits <- bitss
	      ]
	where	bitss = transpose $ map (\ (IsRepWire a) -> showSeqBits a) streams


showBitfileInfo :: [IsRepWire] -> [String]
showBitfileInfo streams =
	      [  "(" ++ show n ++ ") " ++ joinWith " -> "
		 [ v ++ "/" ++ b
	         | (b,v) <- zip bits vals
	         ]
	      | (n::Integer,bits,vals) <- zip3 [0..] bitss valss
	      ]
	where
		joinWith _ [] = []
		joinWith sep xs = foldr1 (\ a b -> a ++ sep ++ b) xs
		bitss = transpose $ map (\ (IsRepWire a) -> showSeqBits a) streams
		valss = transpose $ map (\ (IsRepWire a) -> showSeqVals a) streams

showSeqBits :: forall a c . (Rep a) => CSeq c a -> [String]
showSeqBits ss = [ show $ toRep (i :: X a)
		 | i <- fromSeqX (ss :: CSeq c a)
       	         ]
       -- where showX b = case unX b of
       --  		Nothing -> 'X'
       --  		Just True -> '1'
       --  		Just False -> '0'
       --       witness = error "witness" :: a

showSeqVals :: forall a c . (Rep a) => CSeq c a -> [String]
showSeqVals ss = [ showRep i
	 	 | i <- fromSeqX (ss :: CSeq c a)
       	         ]

----------------------------------------------------------------------------------

-- | generate a (shallow) stream of random boolean. Used for testing
-- shallow circuits.

randomBools :: (Clock c, sig ~ CSeq c) => StdGen -> (Float -> Float) -> sig Bool
randomBools stdGen cut = toSeq [ c < cut t | (c,t) <- zip (randoms stdGen) [1..] ]
