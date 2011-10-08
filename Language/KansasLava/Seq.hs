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
import Data.List as List

import Data.Sized.Ix
import Data.Sized.Matrix as M

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
data CSeq (c :: *) a = Seq (S.Stream (X a)) (D a)

-- | CSeq in some implicit clock domain.
type Seq a = CSeq () a

apS :: (Rep a, Rep b) => CSeq c (a -> b) -> CSeq c a -> CSeq c b
apS (Seq f fe) (Seq a ae) = Seq (S.zipWith apX f a) (fe `apD` ae)

-- wrong location (To Move)
entityD :: forall a . (Rep a) => String -> D a
entityD nm = D $ Port "o0" $ E $ Entity (Prim nm) [("o0",repType (Witness :: Witness a))] []

pureD :: (Rep a) => a -> D a
pureD a = pureXD (pureX a)

pureXD :: (Rep a) => X a -> D a
pureXD a = D $ Lit $ toRep a

apD :: D (a -> b) -> D a -> D b
apD (D (Port "o0" (E (Entity nm [("o0",FunctionTy t1 t2)] xs)))) (D e) 
        = D $ Port "o0" $ E $ Entity nm [("o0",t2)] (xs ++ [ (i,t1,e) ])
      where i = "i" ++ show (List.length xs)
apD other f = error $ show ("apI",other,f)


-- | Extract the shallow portion of a CSeq.
seqValue :: CSeq c a -> S.Stream (X a)
seqValue (Seq a _) = a

-- | Extract the deep portion of a CSeq.
seqDriver :: CSeq c a -> D a
seqDriver (Seq _ d) = d

pureS :: (Rep a) => a -> CSeq i a
pureS a = Seq (pure (pureX a)) (D $ Lit $ toRep $ pureX a)

primS :: (Rep a) => a -> String -> CSeq i a
primS a nm = Seq (pure (pureX a)) (entityD nm)

primXS :: (Rep a) => X a -> String -> CSeq i a
primXS a nm = Seq (pure a) (entityD nm)

instance (Rep a, Show a) => Show (CSeq c a) where
	show (Seq vs _)
         	= concat [ showRep x ++ " "
                         | x <- take 20 $ S.toList vs
                         ] ++ "..."

instance (Rep a, Eq a) => Eq (CSeq c a) where
	-- Silly question; never True; can be False.
	(Seq _ _) == (Seq _ _) = error "undefined: Eq over a Seq"

instance (Num a, Rep a) => Num (CSeq i a) where
    s1 + s2 = primS (+) "+" `apS` s1 `apS` s2
    s1 - s2 = primS (-) "-" `apS` s1 `apS` s2
    s1 * s2 = primS (*) "*" `apS` s1 `apS` s2
    negate s1 = primS (negate) "negate" `apS` s1
    abs s1    = primS (abs)    "abs"    `apS` s1
    signum s1 = primS (signum) "signum" `apS` s1
    fromInteger n = pureS (fromInteger n)


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
		    | xs <- List.transpose [ S.toList x | Seq x _ <- ss ]
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

-- | The Pack class allows us to move between signals containing compound data
-- and signals containing the elements of the compound data. This is done by
-- commuting the signal type constructor with the type constructor representing
-- the compound data.  For example, if we have a value x :: Signal sig => sig
-- (a,b), then 'unpack x' converts this to a (sig a, sig b). Dually, pack takes
-- (sig a,sig b) to sig (a,b).

class Pack clk a where
 type Unpacked clk a
 -- ^ Pull the sig type *out* of the compound data type.
 pack :: Unpacked clk a -> CSeq clk a
 -- ^ Push the sign type *into* the compound data type.
 unpack :: CSeq clk a -> Unpacked clk a


-- | Given a function over unpacked (composite) signals, turn it into a function
-- over packed signals.
mapPacked :: (Pack i a, Pack i b, sig ~ CSeq i) => (Unpacked i a -> Unpacked i b) -> sig a -> sig b
mapPacked f = pack . f . unpack

-- | Lift a binary function operating over unpacked signals into a function over a pair of packed signals.
zipPacked :: (Pack i a, Pack i b, Pack i c, sig ~ CSeq i) 
          => (Unpacked i a -> Unpacked i b -> Unpacked i c) 
          -> sig a -> sig b -> sig c
zipPacked f x y = pack $ f (unpack x) (unpack y)

instance (Rep a, Rep b) => Pack i (a,b) where
	type Unpacked i (a,b) = (CSeq i a,CSeq i b)
	pack (a,b) = primS (,) "pair" `apS` a `apS` b
	unpack ab = ( primS (fst) "fst" `apS` ab
		    , primS (snd) "snd" `apS` ab
		    )


instance (Rep a) => Pack i (Maybe a) where
	type Unpacked i (Maybe a) = (CSeq i Bool, CSeq i a)

	pack (a,b) = primXS (unapX $ \ a' -> 
	                     unapX $ \ b' -> case unX a' of
	                                  Nothing    -> optX Nothing
					  Just False -> optX $ Just Nothing
					  Just True  -> optX (Just (unX b')))
                             "pair" 
                 `apS` a `apS` b
	unpack ma = ( primXS (unapX $ \ a -> case unX a of
					Nothing -> optX Nothing
					Just Nothing -> optX (Just False)
					Just (Just _) -> optX (Just True))
                             "fst" `apS` ma
		    , primXS (unapX $ \ a -> case unX a of
					Nothing -> optX Nothing
					Just Nothing -> optX Nothing
					Just (Just v) -> optX (Just v))
                              "snd" `apS` ma
		    )


{-
instance (Rep a, Rep b, Rep c, Signal sig) => Pack sig (a,b,c) where
	type Unpacked sig (a,b,c) = (sig a, sig b,sig c)
	pack (a,b,c) = liftS3 (\ (Comb a' ae) (Comb b' be) (Comb c' ce) ->
				Comb (XTriple (a',b',c'))
				     (entity3 (Prim "triple") ae be ce))
			    a b c
	unpack abc = ( liftS1 (\ (Comb (XTriple (a,_b,_)) abce) -> Comb a (entity1 (Prim "fst3") abce)) abc
		    , liftS1 (\ (Comb (XTriple (_,b,_)) abce) -> Comb b (entity1 (Prim "snd3") abce)) abc
		    , liftS1 (\ (Comb (XTriple (_,_,c)) abce) -> Comb c (entity1 (Prim "thd3") abce)) abc
		    )
-}

unpackMatrix :: (Rep a, Size x, sig ~ CSeq clk) => sig (M.Matrix x a) -> M.Matrix x (sig a)
unpackMatrix a = unpack a

packMatrix :: (Rep a, Size x, sig ~ CSeq clk) => M.Matrix x (sig a) -> sig (M.Matrix x a)
packMatrix a = pack a


instance (Rep a, Size ix) => Pack clk (Matrix ix a) where
	type Unpacked clk (Matrix ix a) = Matrix ix (CSeq clk a)
        pack = error "pack/Matrix"
        unpack = error "pack/Matrix"
{-
	pack m = liftSL (\ ms -> let sh = M.fromList [ m' | Comb m' _ <- ms ]
				     de = entityN (Prim "concat") [ d | Comb _ d <- ms ]
				 in Comb (XMatrix sh) de) (M.toList m)
        -- unpack :: sig (Matrix ix a) -> Matrix ix (sig a)
	unpack s = forAll $ \ ix ->
			liftS1 (\ (Comb (XMatrix s') d) -> Comb (s' ! ix)
					       (entity2 (Prim "index")
							(D $ Generic (mx ! ix) :: D Integer)
							d
					       )
			        ) s
	   where mx :: (Size ix) => Matrix ix Integer
		 mx = matrix (Prelude.zipWith (\ _ b -> b) (M.indices mx) [0..])

-}