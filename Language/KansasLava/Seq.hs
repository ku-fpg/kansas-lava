{-# LANGUAGE TypeFamilies, ExistentialQuantification,
    FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses #-}

-- | The CSeq module serves as a representation for the combined shallow and
-- deep embeddings of sequential circuits. The shallow portion is reprented as a
-- stream, the deep portion as a (typed) entity.  To allow for multiple clock
-- domains, the CSeq type includes an extra type parameter. The type alias 'Seq'
-- is for sequential logic in some implicit global clock domain.
module Language.KansasLava.Seq where

import Control.Applicative
import Data.List

import Language.KansasLava.Comb
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types



-----------------------------------------------------------------------------------------------

-- | These are sequences of values over time.
-- We assume edge triggered logic (checked at (typically) rising edge of clock)
-- This clock is assumed known, based on who is consuming the list.
-- Right now, it is global, but we think we can support multiple clocks with a bit of work.
data CSeq c a = Seq (S.Stream (X a)) (D a)

-- | Type alias for CSeq
type Clocked c a = CSeq c a	-- new name, start using

-- | CSeq in some implicit clock domain.
type Seq a = CSeq () a

-- | Extract the shallow portion of a CSeq.
seqValue :: CSeq c a -> S.Stream (X a)
seqValue (Seq a _) = a

-- | Extract the deep portion of a CSeq.
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

-- | Inject a deep value into a CSeq. The shallow portion of the CSeq will be an
-- error, if it is every used.
deepSeq :: D a -> CSeq c a
deepSeq = Seq (error "incorrect use of shallow Seq")

-- | Inject a shallow value into a CSeq. The deep portion of the CSeq will be an
-- Error if it is ever used.
shallowSeq :: S.Stream (X a) -> CSeq c a
shallowSeq s = Seq s (D $ Error "incorrect use of deep Seq")

-- | Create a CSeq with undefined for both the deep and shallow elements.
undefinedSeq ::  forall a c . (Rep a) => CSeq c a
undefinedSeq = liftS0 undefinedComb

-- | Create a Seq which will repeatedly enumerates all of the possible values of
-- the witness data type in the shallow portion.
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

  liftS3 f (Seq a ea) (Seq b eb) (Seq c ec) = Seq (S.zipWith3 f' a b c) ed
      where
	ed = combDriver $ f (deepComb ea) (deepComb eb) (deepComb ec)
	f' x y z = combValue (f (shallowComb x) (shallowComb y) (shallowComb z))

  liftSL f ss = Seq (S.fromList
		    [ combValue $ f [ shallowComb x | x <- xs ]
		    | xs <- transpose [ S.toList x | Seq x _ <- ss ]
		    ])
		    (combDriver (f (map (deepComb . seqDriver) ss)))

  deepS (Seq _ d) = d

----------------------------------------------------------------------------------------------------

-- Small DSL's for declaring signals

-- | Convert a list of values into a CSeq. The shallow portion of the resulting
-- CSeq will begin with the input list, then an infinite stream of X unknowns.
toSeq :: (Rep a) => [a] -> CSeq c a
toSeq xs = shallowSeq (S.fromList (map optX (map Just xs ++ repeat Nothing)))

-- | Convert a list of values into a CSeq. The input list is wrapped with a
-- Maybe, and any Nothing elements are mapped to X's unknowns.
toSeq' :: (Rep a) => [Maybe a] -> CSeq c a
toSeq' xs = shallowSeq (S.fromList (map optX (xs ++ repeat Nothing)))

-- | Convert a list of X values to a Seq. Pad the end with an infinite list of X unknowns.
toSeqX :: forall a c . (Rep a) => [X a] -> CSeq c a
toSeqX xs = shallowSeq (S.fromList (xs ++ map (optX :: Maybe a -> X a) (repeat Nothing)))

-- | Take the first n elements of one sequence, then append the second sequence.
takeThenSeq :: Int -> CSeq c a -> CSeq c a -> CSeq c a
takeThenSeq n sq1 sq2 = shallowSeq (S.fromList (take n (S.toList (seqValue sq1)) ++
                                                S.toList (seqValue sq2)))

-- | Apply a function from characters to a given type across a String, then lift
-- it into a CSeq padded by an infinite list of X unknowns.
encSeq :: (Rep a) =>  (Char -> Maybe a) -> String -> CSeq c a
encSeq enc xs = shallowSeq (S.fromList (map optX (map enc xs ++ repeat Nothing)))

-- | Convert a string, with 'H' representing True, 'L' representing False, into a CSeq.
encSeqBool :: String -> CSeq c Bool
encSeqBool = encSeq enc
	where enc 'H' = return True
	      enc 'L' = return False
	      enc  _   = Nothing

-- | Convert a CSeq into a list of Strings, one string for each time step.
showStreamList :: forall a c . (Rep a) => CSeq c a -> [String]
showStreamList ss =
	[ showRep  x
	| x <- S.toList (seqValue ss)
	]

-- | Convert a CSeq of values into a list of Maybe values.
fromSeq :: (Rep a) => CSeq c a -> [Maybe a]
fromSeq = fmap unX . S.toList . seqValue

-- | Convret a CSeq of values into a list of representable values.
fromSeqX :: (Rep a) => CSeq c a -> [X a]
fromSeqX = S.toList . seqValue

-- | Compare the first depth elements of two CSeqs.
cmpSeqRep :: forall a c . (Rep a) => Int -> CSeq c a -> CSeq c a -> Bool
cmpSeqRep depth s1 s2 = and $ take depth $ S.toList $ S.zipWith cmpRep
								(seqValue s1)
								(seqValue s2)

-----------------------------------------------------------------------------------

-- Do we use this any more?
-- | Monomorphic box round wires.
data IsRepWire = forall a c . (Rep a) => IsRepWire (CSeq c a)

-- | The raw data from a representable wire.
showBitfile :: [IsRepWire] -> [String]
showBitfile streams =
	      [ concat bits
	      | bits <- bitss
	      ]
	where	bitss = transpose $ map (\ (IsRepWire a) -> showSeqBits a) streams

-- | Show a list of representable wires, along with metainfo.
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

-- | Convert a CSeq to a list of bitvectors, then print them out.
showSeqBits :: forall a c . (Rep a) => CSeq c a -> [String]
showSeqBits ss = [ show $ toRep (i :: X a)
		 | i <- fromSeqX (ss :: CSeq c a)
       	         ]
       -- where showX b = case unX b of
       --  		Nothing -> 'X'
       --  		Just True -> '1'
       --  		Just False -> '0'
       --       witness = error "witness" :: a
-- | Convert a CSeq to a list of values, using the showRep for the type contained in the CSeq.
showSeqVals :: forall a c . (Rep a) => CSeq c a -> [String]
showSeqVals ss = [ showRep i
	 	 | i <- fromSeqX (ss :: CSeq c a)
       	         ]

----------------------------------------------------------------------------------

instance Dual (CSeq c a) where
    dual c d = Seq (seqValue c) (seqDriver d)

-- | Extract the Lava type of a Seq.
typeOfSeq :: forall w  . (Rep w) => Seq w -> Type
typeOfSeq _ = repType (Witness :: Witness w)

{-
  - TODO: TO BE MOVED

-- | Generate a (shallow) stream of random boolean. Used for testing
-- shallow circuits. The function is a mapping from clock cycle number (starting
-- at 0) to (0..1), which is the likelihood of returning a True.
-- The clock cycle count allows for periodic tests.

randomBools :: (Clock c, sig ~ CSeq c) => StdGen -> (Integer -> Float) -> sig Bool
randomBools stdGen cut = toSeq [ c < cut t | (c,t) <- zip (randoms stdGen) [0..] ]
-}
