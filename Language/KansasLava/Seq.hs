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

instance Dual (CSeq c a) where
    dual c d = Seq (seqValue c) (seqDriver d)

-- | Extract the Lava type of a Seq.
typeOfSeq :: forall w  . (Rep w) => Seq w -> Type
typeOfSeq _ = repType (Witness :: Witness w)

