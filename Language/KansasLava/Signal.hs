{-# LANGUAGE TypeFamilies, ExistentialQuantification,
    FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses #-}

-- | The Signal module serves as a representation for the combined shallow and
-- deep embeddings of sequential circuits. The shallow portion is reprented as a
-- stream, the deep portion as a (typed) entity.  To allow for multiple clock
-- domains, the Signal type includes an extra type parameter. The type alias 'Seq'
-- is for sequential logic in some implicit global clock domain.
module Language.KansasLava.Signal where

import Control.Applicative
import Control.Monad (liftM, liftM2, liftM3)
import Data.List as List
import Data.Bits

import Data.Sized.Ix
import Data.Sized.Matrix as M

-- import Language.KansasLava.Comb
import Language.KansasLava.Rep
--import Language.KansasLava.Signal
import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Types

-----------------------------------------------------------------------------------------------

-- | These are sequences of values over time.
-- We assume edge triggered logic (checked at (typically) rising edge of clock)
-- This clock is assumed known, based on who is consuming the list.
-- Right now, it is global, but we think we can support multiple clocks with a bit of work.
data Signal (c :: *) a = Signal (S.Stream (X a)) (D a)

-- | Signal in some implicit clock domain.
type Seq a = Signal () a

--apS :: (Rep a, Rep b) => Signal c (a -> b) -> Signal c a -> Signal c b
--apS (Signal f fe) (Signal a ae) = Signal (S.zipWith apX f a) (fe `apD` ae)

idD :: forall a sig clk . (Rep a, sig ~ Signal clk) => Id -> sig a -> sig a
idD id' (Signal a ae) = Signal a $ D $ Port "o0" $ E 
                     $ Entity id'
                         [("o0",repType (Witness :: Witness a))]
                         [("i0",repType (Witness :: Witness a),unD $ ae)]

-- wrong location (To Move)
entityD :: forall a . (Rep a) => String -> D a
entityD nm = D $ Port "o0" $ E $ Entity (Prim nm) [("o0",repType (Witness :: Witness a))] 
                                                  []

entityD1 :: forall a1 a . (Rep a, Rep a1) => String -> D a1 -> D a
entityD1 nm (D a1) 
        = D $ Port "o0" $ E $ Entity (Prim nm) [("o0",repType (Witness :: Witness a))] 
                                               [("i0",repType (Witness :: Witness a1),a1)]

entityD2 :: forall a1 a2 a . (Rep a, Rep a1, Rep a2) => String -> D a1 -> D a2 -> D a
entityD2 nm (D a1) (D a2) 
        = D $ Port "o0" $ E $ Entity (Prim nm) [("o0",repType (Witness :: Witness a))]
                                               [("i0",repType (Witness :: Witness a1),a1)
                                               ,("i1",repType (Witness :: Witness a2),a2)]
                                               
entityD3 :: forall a1 a2 a3 a . (Rep a, Rep a1, Rep a2, Rep a3) => String -> D a1 -> D a2 -> D a3 -> D a
entityD3 nm (D a1) (D a2) (D a3) 
        = D $ Port "o0" $ E $ Entity (Prim nm) [("o0",repType (Witness :: Witness a))]
                                               [("i0",repType (Witness :: Witness a1),a1)
                                               ,("i1",repType (Witness :: Witness a2),a2)
                                               ,("i2",repType (Witness :: Witness a3),a3)]

pureD :: (Rep a) => a -> D a
pureD a = pureXD (pureX a)

pureXD :: (Rep a) => X a -> D a
pureXD a = D $ Lit $ toRep a

apD :: D (a -> b) -> D a -> D b
apD (D (Port "o0" (E (Entity nm [("o0",FunctionTy t1 t2)] xs)))) (D e) 
        = D $ Port "o0" $ E $ Entity nm [("o0",t2)] (xs ++ [ (i,t1,e) ])
      where i = "i" ++ show (List.length xs)
apD other f = error $ "internal error with apD: " ++ show (other,f)


-- | Extract the shallow portion of a Signal.
seqValue :: Signal c a -> S.Stream (X a)
seqValue (Signal a _) = a

-- | Extract the deep portion of a Signal.
seqDriver :: Signal c a -> D a
seqDriver (Signal _ d) = d

pureS :: (Rep a) => a -> Signal i a
pureS a = Signal (pure (pureX a)) (D $ Lit $ toRep $ pureX a)

-- | Inject a deep value into a Signal. The shallow portion of the Signal will be an
-- error, if it is every used.
deepSignal :: D a -> Signal c a
deepSignal = Signal (error "incorrect use of shallow Signal")

-- | Inject a shallow value into a Signal. The deep portion of the Signal will be an
-- Error if it is ever used.
shallowSignal :: S.Stream (X a) -> Signal c a
shallowSignal s = Signal s (D $ Error "incorrect use of deep Signal")

-- | Create a Signal with undefined for both the deep and shallow elements.
undefinedSignal ::  forall a sig clk . (Rep a, sig ~ Signal clk) => sig a
undefinedSignal = Signal (pure $ optX Nothing)
		      (D $ Lit $ toRep (optX (Nothing :: Maybe a)))

comment :: forall a sig clk . (Rep a, sig ~ Signal clk) => String -> sig a -> sig a
comment msg = idD (Comment [msg])


primS :: (Rep a) => a -> String -> Signal i a
primS a nm = primXS (pureX a) nm

primS1 :: (Rep a, Rep b) => (a -> b) -> String -> Signal i a -> Signal i b
primS1 f nm = primXS1 (\ a -> optX $ liftM f (unX a)) nm

primS2 :: (Rep a, Rep b, Rep c) => (a -> b -> c) -> String -> Signal i a -> Signal i b ->  Signal i c
primS2 f nm = primXS2 (\ a b -> optX $ liftM2 f (unX a) (unX b)) nm

primS3 :: (Rep a, Rep b, Rep c, Rep d) => (a -> b -> c -> d) -> String -> Signal i a -> Signal i b -> Signal i c -> Signal i d
primS3 f nm = primXS3 (\ a b c -> optX $ liftM3 f (unX a) (unX b) (unX c)) nm

primXS :: (Rep a) => X a -> String -> Signal i a
primXS a nm = Signal (pure a) (entityD nm)

primXS1 :: forall a b i . (Rep a, Rep b) => (X a -> X b) -> String -> Signal i a -> Signal i b
primXS1 f nm (Signal a1 ae1) = Signal (fmap f a1) (entityD1 nm  ae1)

primXS2 :: forall a b c i . (Rep a, Rep b, Rep c) => (X a -> X b -> X c) -> String -> Signal i a -> Signal i b ->  Signal i c
primXS2 f nm (Signal a1 ae1) (Signal a2 ae2) 
        = Signal (S.zipWith f a1 a2) 
              (entityD2 nm ae1 ae2)

primXS3 :: forall a b c d i . (Rep a, Rep b, Rep c, Rep d)
        => (X a -> X b -> X c -> X d) -> String ->  Signal i a -> Signal i b -> Signal i c -> Signal i d
primXS3 f nm (Signal a1 ae1) (Signal a2 ae2)  (Signal a3 ae3)  = Signal (S.zipWith3 f a1 a2 a3)
              (entityD3 nm  ae1  ae2  ae3)

instance (Rep a, Show a) => Show (Signal c a) where
	show (Signal vs _)
         	= concat [ showRep x ++ " "
                         | x <- take 20 $ S.toList vs
                         ] ++ "..."

instance (Rep a, Eq a) => Eq (Signal c a) where
	-- Silly question; never True; can be False.
	(Signal _ _) == (Signal _ _) = error "undefined: Eq over a Signal"

instance (Num a, Rep a) => Num (Signal i a) where
    s1 + s2 = primS2 (+) "+" s1 s2
    s1 - s2 = primS2 (-) "-" s1 s2
    s1 * s2 = primS2 (*) "*" s1 s2
    negate s1 = primS1 (negate) "negate" s1
    abs s1    = primS1 (abs)    "abs"    s1
    signum s1 = primS1 (signum) "signum" s1
    fromInteger n = pureS (fromInteger n)

instance (Bounded a, Rep a) => Bounded (Signal i a) where
    minBound = pureS $ minBound
    maxBound = pureS $ maxBound

instance (Show a, Bits a, Rep a) => Bits (Signal i a) where
    s1 .&. s2      = primS2 (.&.) ".&."   s1  s2
    s1 .|. s2      = primS2 (.|.) ".|."   s1  s2
    s1 `xor` s2    = primS2 (xor) ".^."   s1  s2
    s1 `shiftL` n  = primS2 (shiftL) "shiftL"    s1  (pureS n)
    s1 `shiftR` n  = primS2 (shiftR) "shiftR"    s1  (pureS n)
    s1 `rotateL` n = primS2 (rotateL) "rotateL"  s1  (pureS n)
    s1 `rotateR` n = primS2 (rotateR) "rotateR"  s1  (pureS n)
    complement s   = primS1 (complement) "complement"  s
    bitSize s      = typeWidth (bitTypeOf s)
    isSigned s     = isTypeSigned (bitTypeOf s)

instance (Eq a, Show a, Fractional a, Rep a) => Fractional (Signal i a) where
    s1 / s2 = primS2 (/) "/"  s1  s2
    recip s1 = primS1 (recip) "recip"  s1
    -- This should just fold down to the raw bits.
    fromRational r = pureS (fromRational r :: a)

instance (Rep a, Enum a) => Enum (Signal i a) where
	toEnum   = error "toEnum not supported"
	fromEnum = error "fromEnum not supported"

instance (Ord a, Rep a) => Ord (Signal i a) where
  compare _ _ = error "compare not supported for Comb"
  (<) _ _     = error "(<) not supported for Comb"
  (>=) _ _    = error "(>=) not supported for Comb"
  (>) _ _     = error "(>) not supported for Comb"
  (<=)_ _     = error "(<=) not supported for Comb"
  s1 `max` s2 = primS2 max "max"  s1  s2
  s1 `min` s2 = primS2 max "min"  s1  s2

instance (Rep a, Real a) => Real (Signal i a) where
	toRational = error "toRational not supported for Comb"

instance (Rep a, Integral a) => Integral (Signal i a) where
	quot num dom = primS2 quot "quot"  num  dom
	rem num dom  = primS2 rem "rem"    num  dom
	div num dom  = primS2 div "div"    num  dom
	mod num dom  = primS2 mod "mod"    num  dom

        quotRem num dom = (quot num dom, rem num dom)
        divMod num dom  = (div num dom, mod num dom)
        toInteger = error "toInteger (Signal {})"


{-
instance Signal (Signal c) where
  liftS0 c = Signal (pure (combValue c)) (combDriver c)

  liftS1 f (Signal a ea) = {-# SCC "liftS1Signal" #-}
    let deep = combDriver (f (deepComb ea))
	f' = combValue . f . shallowComb
   in Signal (fmap f' a) deep

  -- We can not replace this with a version that uses packing,
  -- because *this* function is used by the pack/unpack stuff.
  liftS2 f (Signal a ea) (Signal b eb) = Signal (S.zipWith f' a b) ec
      where
	ec = combDriver $ f (deepComb ea) (deepComb eb)
	f' x y = combValue (f (shallowComb x) (shallowComb y))

  liftS3 f (Signal a ea) (Signal b eb) (Signal c ec) = Signal (S.zipWith3 f' a b c) ed
      where
	ed = combDriver $ f (deepComb ea) (deepComb eb) (deepComb ec)
	f' x y z = combValue (f (shallowComb x) (shallowComb y) (shallowComb z))

  liftSL f ss = Signal (S.fromList
		    [ combValue $ f [ shallowComb x | x <- xs ]
		    | xs <- List.transpose [ S.toList x | Signal x _ <- ss ]
		    ])
		    (combDriver (f (map (deepComb . seqDriver) ss)))

  deepS (Signal _ d) = d
-}
----------------------------------------------------------------------------------------------------

-- Small DSL's for declaring signals

-- | Convert a list of values into a Signal. The shallow portion of the resulting
-- Signal will begin with the input list, then an infinite stream of X unknowns.
toSignal :: (Rep a) => [a] -> Signal c a
toSignal xs = shallowSignal (S.fromList (map optX (map Just xs ++ repeat Nothing)))

-- | Convert a list of values into a Signal. The input list is wrapped with a
-- Maybe, and any Nothing elements are mapped to X's unknowns.
toSignal' :: (Rep a) => [Maybe a] -> Signal c a
toSignal' xs = shallowSignal (S.fromList (map optX (xs ++ repeat Nothing)))

-- | Convert a list of X values to a Signal. Pad the end with an infinite list of X unknowns.
toSignalX :: forall a c . (Rep a) => [X a] -> Signal c a
toSignalX xs = shallowSignal (S.fromList (xs ++ map (optX :: Maybe a -> X a) (repeat Nothing)))

-- | Convert a Signal of values into a list of Maybe values.
fromSignal :: (Rep a) => Signal c a -> [Maybe a]
fromSignal = fmap unX . S.toList . seqValue

-- | Convret a Signal of values into a list of representable values.
fromSignalX :: (Rep a) => Signal c a -> [X a]
fromSignalX = S.toList . seqValue

-- | Compare the first depth elements of two Signals.
cmpSignalRep :: forall a c . (Rep a) => Int -> Signal c a -> Signal c a -> Bool
cmpSignalRep depth s1 s2 = and $ take depth $ S.toList $ S.zipWith cmpRep
								(seqValue s1)
								(seqValue s2)

-----------------------------------------------------------------------------------

instance Dual (Signal c a) where
    dual c d = Signal (seqValue c) (seqDriver d)

-- alias
bitTypeOf :: forall w clk sig . (Rep w, sig ~ Signal clk) => sig w -> Type 
bitTypeOf = typeOfSignal

-- | Return the Lava type of a representable signal.
typeOfSignal :: forall w clk sig . (Rep w, sig ~ Signal clk) => sig w -> Type 
typeOfSignal _ = repType (Witness :: Witness w)

-- | The Pack class allows us to move between signals containing compound data
-- and signals containing the elements of the compound data. This is done by
-- commuting the signal type constructor with the type constructor representing
-- the compound data.  For example, if we have a value x :: Signal sig => sig
-- (a,b), then 'unpack x' converts this to a (sig a, sig b). Dually, pack takes
-- (sig a,sig b) to sig (a,b).

class Pack clk a where
 type Unpacked clk a
 -- ^ Pull the sig type *out* of the compound data type.
 pack :: Unpacked clk a -> Signal clk a
 -- ^ Push the sign type *into* the compound data type.
 unpack :: Signal clk a -> Unpacked clk a


-- | Given a function over unpacked (composite) signals, turn it into a function
-- over packed signals.
mapPacked :: (Pack i a, Pack i b, sig ~ Signal i) => (Unpacked i a -> Unpacked i b) -> sig a -> sig b
mapPacked f = pack . f . unpack

-- | Lift a binary function operating over unpacked signals into a function over a pair of packed signals.
zipPacked :: (Pack i a, Pack i b, Pack i c, sig ~ Signal i) 
          => (Unpacked i a -> Unpacked i b -> Unpacked i c) 
          -> sig a -> sig b -> sig c
zipPacked f x y = pack $ f (unpack x) (unpack y)

instance (Rep a, Rep b) => Pack i (a,b) where
	type Unpacked i (a,b) = (Signal i a,Signal i b)
	pack (a,b) = primS2 (,) "pair"  a  b
	unpack ab = ( primS1 (fst) "fst"  ab
		    , primS1 (snd) "snd"  ab
		    )


instance (Rep a) => Pack i (Maybe a) where
	type Unpacked i (Maybe a) = (Signal i Bool, Signal i a)

	pack (a,b) = primXS2 (\ a' b' -> case unX a' of
	                                  Nothing    -> optX Nothing
					  Just False -> optX $ Just Nothing
					  Just True  -> optX (Just (unX b')))
                             "pair" a b
	unpack ma = ( primXS1 (\ a -> case unX a of
					Nothing -> optX Nothing
					Just Nothing -> optX (Just False)
					Just (Just _) -> optX (Just True))
                             "fst" ma
		    , primXS1 (\ a -> case unX a of
					Nothing -> optX Nothing
					Just Nothing -> optX Nothing
					Just (Just v) -> optX (Just v))
                              "snd" ma
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

unpackMatrix :: (Rep a, Size x, sig ~ Signal clk) => sig (M.Matrix x a) -> M.Matrix x (sig a)
unpackMatrix a = unpack a

packMatrix :: (Rep a, Size x, sig ~ Signal clk) => M.Matrix x (sig a) -> sig (M.Matrix x a)
packMatrix a = pack a

instance (Rep a, Size ix) => Pack clk (Matrix ix a) where
	type Unpacked clk (Matrix ix a) = Matrix ix (Signal clk a)
        pack m = Signal shallow
                     deep
          where
                shallow :: (S.Stream (X (Matrix ix a)))
                shallow = id
                        $ S.fromList            -- Stream (X (Matrix ix a))
                        $ fmap XMatrix          -- [(X (Matrix ix a))]
                        $ fmap M.fromList       -- [Matrix ix (X a)]
                        $ List.transpose        -- [[X a]]
                        $ fmap S.toList         -- [[X a]]
                        $ fmap seqValue         -- [Stream (X a)]
                        $ M.toList              -- [sig a]
                        $ m                     -- Matrix ix (sig a)

                deep :: D (Matrix ix a)
                deep = D 
                     $ Port "o0" 
                     $ E 
                     $ Entity (Prim "concat")
                                 [("o0",repType (Witness :: Witness (Matrix ix a)))]
                                 [ ("i" ++ show i,repType (Witness :: Witness a),unD $ seqDriver $ x)
                                 | (x,i) <- zip (M.toList m) ([0..] :: [Int])
                                 ]

        unpack ms = forAll $ \ i -> Signal (shallow i) (deep i)
        
	   where mx :: (Size ix) => Matrix ix Integer
		 mx = matrix (Prelude.zipWith (\ _ b -> b) (M.indices mx) [0..])

                 deep i = D 
                        $ Port "o0" 
                        $ E 
                        $ Entity (Prim "index")
                                 [("o0",repType (Witness :: Witness a))]
                                 [("i0",GenericTy,Generic (mx ! i))
                                 ,("i1",repType (Witness :: Witness (Matrix ix a)),unD $ seqDriver ms)
                                 ]

                 shallow i = fmap (liftX (M.! i)) (seqValue ms)

----------------------------------------------------------------

-- | a delay is a register with no defined default / initial value.
delay :: forall a clk . (Rep a, Clock clk) => Signal clk a -> Signal clk a
delay ~(Signal line eline) = res
   where
        def = optX $ Nothing

        -- rep = toRep def
	res = Signal sres1 (D $ Port ("o0") $ E $ entity)

	sres0 = line
	sres1 = S.Cons def sres0

        entity = Entity (Prim "delay")
                    [("o0", bitTypeOf res)]
                    [("i0", bitTypeOf res, unD eline),
		     ("clk",ClkTy, Pad "clk"),
		     ("rst",B,     Pad "rst")
		    ]
-- | delays generates a serial sequence of n delays.
delays :: forall a clk .  (Rep a, Clock clk) => Int -> Signal clk a -> Signal clk a
delays n ss = iterate delay ss !! n


-- | A register is a state element with a reset. The reset is supplied by the clock domain in the Signal.
register :: forall a clk .  (Rep a, Clock clk) => a -> Signal clk a -> Signal clk a
register first  ~(Signal line eline) = res
   where
        def = optX $ Just first

        rep = toRep def
	res = Signal sres1 (D $ Port ("o0") $ E $ entity)

	sres0 = line
	sres1 = S.Cons def sres0

        entity = Entity (Prim "register")
                    [("o0", bitTypeOf res)]
                    [("i0", bitTypeOf res, unD eline),
                     ("def",GenericTy,Generic (fromRepToInteger rep)),
		     ("clk",ClkTy, Pad "clk"),
		     ("rst",B,     Pad "rst")
		    ]
-- | registers generates a serial sequence of n registers, all with the same initial value.
registers :: forall a clk .  (Rep a, Clock clk) => Int -> a -> Signal clk a -> Signal clk a
registers n def ss = iterate (register def) ss !! n
