{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts, DeriveDataTypeable,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp, TypeSynonymInstances  #-}

module Language.KansasLava.Rep where

import Language.KansasLava.Types
import Control.Monad (liftM)
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Sized.Matrix hiding (S)
import qualified Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Data.Sized.Signed as S
import Data.Word
import qualified Data.Maybe as Maybe
import Data.Traversable(sequenceA)
import qualified Data.Sized.Sampled as Sampled


-- | A 'Rep a' is an 'a' value that we 'Rep'resent, aka we can push it over a wire.
class {- (Size (W w)) => -} Rep w where
    -- | the width of the represented value.
    type W w

    -- | X are lifted inputs to this wire.
    data X w

    -- | check for bad things.
    unX :: X w -> Maybe w

    -- | and, put the good or bad things back.
    optX :: Maybe w -> X w

    -- | convert to binary (rep) format
    toRep   :: X w -> RepValue

    -- | convert from binary (rep) format
    fromRep :: RepValue -> X w

    -- | Each wire has a known type.
    repType :: Witness w -> Type

    -- show the value (in its Haskell form, default is the bits)
    showRep :: X w -> String
    showRep x = show (toRep x)

-- Give me all possible (non-X) representations (2^n of them).
allReps :: (Rep w) => Witness w -> [RepValue]
allReps w = [ RepValue (fmap WireVal count) | count <- counts n ]
   where
    n = repWidth w
    counts :: Int -> [[Bool]]
    counts 0 = [[]]
    counts num = [ x : xs |  xs <- counts (num-1), x <- [False,True] ]

-- | Figure out the width in bits of a type.

repWidth :: (Rep w) => Witness w -> Int
repWidth w = typeWidth (repType w)


-- | unknownRepValue returns a RepValue that is completely filled with 'X'.
unknownRepValue :: (Rep w) => Witness w -> RepValue
unknownRepValue w = RepValue [ WireUnknown | _ <- [1..repWidth w]]

allOkayRep :: (Size w) => Matrix w (X Bool) -> Maybe (Matrix w Bool)
allOkayRep m = sequenceA $ fmap prj m
  where prj (XBool WireUnknown) = Nothing
        prj (XBool (WireVal v)) = Just v

-- | return a 'w' inside X.
pureX :: (Rep w) => w -> X w
pureX = optX . Just

unknownX :: forall w . (Rep w) => X w
unknownX = optX (Nothing :: Maybe w)

liftX :: (Rep a, Rep b) => (a -> b) -> X a -> X b
liftX f = optX . liftM f . unX

-- This is not wired into the class because of the extra 'Show' requirement.

showRepDefault :: forall w. (Show w, Rep w) => X w -> String
showRepDefault v = case unX v :: Maybe w of
            Nothing -> "?"
            Just v' -> show v'

toRepFromIntegral :: forall v . (Rep v, Integral v) => X v -> RepValue
toRepFromIntegral v = case unX v :: Maybe v of
                 Nothing -> unknownRepValue (Witness :: Witness v)
                 Just v' -> RepValue
                    $ take (repWidth (Witness :: Witness v))
                    $ map WireVal
                    $ map odd
                    $ iterate (`div` (2::Int))
                    $ fromIntegral v'

fromRepToIntegral :: forall v . (Rep v, Integral v) => RepValue -> X v
fromRepToIntegral r =
    optX (fmap (\ xs ->
        sum [ n
                | (n,b) <- zip (iterate (* 2) 1)
                       xs
                , b
                ])
          (getValidRepValue r) :: Maybe v)
-- always a +ve number, unknowns defin
fromRepToInteger :: RepValue -> Integer
fromRepToInteger (RepValue xs) =
        sum [ n
                | (n,b) <- zip (iterate (* 2) 1)
                       xs
                , case b of
            WireUnknown -> False
            WireVal True -> True
            WireVal False -> False
                ]


-- | compare a golden value with a generated value.
cmpRep :: (Rep a) => X a -> X a -> Bool
cmpRep g v = toRep g `cmpRepValue` toRep v

------------------------------------------------------------------------------------

instance Rep Bool where
    type W Bool     = X1
    data X Bool     = XBool (WireVal Bool)
    optX (Just b)   = XBool $ return b
    optX Nothing    = XBool $ fail "Wire Bool"
    unX (XBool (WireVal v))  = return v
    unX (XBool WireUnknown) = fail "Wire Bool"
    repType _  = B
    toRep (XBool v)   = RepValue [v]
    fromRep (RepValue [v]) = XBool v
    fromRep rep    = error ("size error for Bool : " ++ (show $ Prelude.length $ unRepValue rep) ++ " " ++ show rep)

instance Rep Int where
    type W Int     = X32
    data X Int  = XInt (WireVal Int)
    optX (Just b)   = XInt $ return b
    optX Nothing    = XInt $ fail "Wire Int"
    unX (XInt (WireVal v))  = return v
    unX (XInt WireUnknown) = fail "Wire Int"
--  wireName _  = "Int"
    repType _  = S 32      -- hmm. Not really on 64 bit machines.

    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

instance Rep Word8 where
    type W Word8     = X8
    data X Word8    = XWord8 (WireVal Word8)
    optX (Just b)   = XWord8 $ return b
    optX Nothing    = XWord8 $ fail "Wire Word8"
    unX (XWord8 (WireVal v))  = return v
    unX (XWord8 WireUnknown) = fail "Wire Word8"
    repType _  = U 8
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

instance Rep Word32 where
    type W Word32     = X32
    data X Word32   = XWord32 (WireVal Word32)
    optX (Just b)   = XWord32 $ return b
    optX Nothing    = XWord32 $ fail "Wire Word32"
    unX (XWord32 (WireVal v)) = return v
    unX (XWord32 WireUnknown) = fail "Wire Word32"
    repType _  = U 32
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

instance Rep () where
    type W ()     = X0
    data X ()   = XUnit (WireVal ())
    optX (Just b)   = XUnit $ return b
    optX Nothing    = XUnit $ fail "Wire ()"
    unX (XUnit (WireVal v))  = return v
    unX (XUnit WireUnknown) = fail "Wire ()"
--  wireName _  = "Unit"
    repType _  = V 1   -- should really be V 0 TODO
    toRep _ = RepValue []
    fromRep _ = XUnit $ return ()
    showRep _ = "()"

data IntegerWidth = IntegerWidth

instance Rep Integer where
    type W Integer  = IntegerWidth
    data X Integer  = XInteger Integer   -- No fail/unknown value
    optX (Just b)   = XInteger b
    optX Nothing    = XInteger $ error "Generic failed in optX"
    unX (XInteger a)       = return a
    repType _  = GenericTy
    toRep = error "can not turn a Generic to a Rep"
    fromRep = error "can not turn a Rep to a Generic"
    showRep (XInteger v) = show v

-------------------------------------------------------------------------------------
-- Now the containers

instance (Rep a, Rep b) => Rep (a,b) where
    type W (a,b)  = ADD (W a) (W b)
    data X (a,b)        = XTuple (X a, X b)
    optX (Just (a,b))   = XTuple (pureX a, pureX b)
    optX Nothing        = XTuple (optX (Nothing :: Maybe a), optX (Nothing :: Maybe b))
    unX (XTuple (a,b)) = do x <- unX a
                            y <- unX b
                            return (x,y)
--  wireName _ = "Tuple_2"

    repType Witness = TupleTy [repType (Witness :: Witness a), repType (Witness :: Witness b)]

    toRep (XTuple (a,b)) = RepValue (avals ++ bvals)
        where (RepValue avals) = toRep a
              (RepValue bvals) = toRep b
    fromRep (RepValue vs) = XTuple ( fromRep (RepValue (take size_a vs))
                  , fromRep (RepValue (drop size_a vs))
                  )
        where size_a = typeWidth (repType (Witness :: Witness a))
    showRep (XTuple (a,b)) = "(" ++ showRep a ++ "," ++ showRep b ++ ")"

instance (Rep a, Rep b, Rep c) => Rep (a,b,c) where
    type W (a,b,c) = ADD (W a) (ADD (W b) (W c))
    data X (a,b,c)      = XTriple (X a, X b, X c)
    optX (Just (a,b,c))     = XTriple (pureX a, pureX b,pureX c)
    optX Nothing        = XTriple ( optX (Nothing :: Maybe a),
                    optX (Nothing :: Maybe b),
                    optX (Nothing :: Maybe c) )
    unX (XTriple (a,b,c))
          = do x <- unX a
               y <- unX b
               z <- unX c
               return (x,y,z)

    repType Witness = TupleTy [repType (Witness :: Witness a), repType (Witness :: Witness b),repType (Witness :: Witness c)]
    toRep (XTriple (a,b,c)) = RepValue (avals ++ bvals ++ cvals)
        where (RepValue avals) = toRep a
              (RepValue bvals) = toRep b
              (RepValue cvals) = toRep c
    fromRep (RepValue vs) = XTriple ( fromRep (RepValue (take size_a vs))
				  , fromRep (RepValue (take size_b (drop size_a vs)))
                  , fromRep (RepValue (drop (size_a + size_b) vs))
                  )
        where size_a = typeWidth (repType (Witness :: Witness a))
              size_b = typeWidth (repType (Witness :: Witness b))
    showRep (XTriple (a,b,c)) = "(" ++ showRep a ++
                "," ++ showRep b ++
                "," ++ showRep c ++ ")"

instance (Rep a) => Rep (Maybe a) where
    type W (Maybe a) = ADD (W a) X1
    -- not completely sure about this representation
    data X (Maybe a) = XMaybe (X Bool, X a)
    optX b      = XMaybe ( case b of
                  Nothing -> optX (Nothing :: Maybe Bool)
                  Just Nothing   -> optX (Just False :: Maybe Bool)
                  Just (Just {}) -> optX (Just True :: Maybe Bool)
              , case b of
                Nothing       -> optX (Nothing :: Maybe a)
                Just Nothing  -> optX (Nothing :: Maybe a)
                Just (Just a) -> optX (Just a :: Maybe a)
              )
    unX (XMaybe (a,b))   = case unX a :: Maybe Bool of
                Nothing    -> Nothing
                Just True  -> Just $ unX b
                Just False -> Just Nothing
--  wireName _  = "Maybe<" ++ wireName (error "witness" :: a) ++ ">"
    repType _  = TupleTy [ B, repType (Witness :: Witness a)]

    toRep (XMaybe (a,b)) = RepValue (avals ++ bvals)
        where (RepValue avals) = toRep a
              (RepValue bvals) = toRep b
    fromRep (RepValue vs) = XMaybe ( fromRep (RepValue (take 1 vs))
                  , fromRep (RepValue (drop 1 vs))
                  )
    showRep (XMaybe (XBool WireUnknown,_a)) = "?"
    showRep (XMaybe (XBool (WireVal True),a)) = "Just " ++ showRep a
    showRep (XMaybe (XBool (WireVal False),_)) = "Nothing"

instance (Size ix, Rep a) => Rep (Matrix ix a) where
    type W (Matrix ix a) = MUL ix (W a)
    data X (Matrix ix a) = XMatrix (Matrix ix (X a))
    optX (Just m)   = XMatrix $ fmap (optX . Just) m
    optX Nothing    = XMatrix $ forAll $ \ _ -> optX (Nothing :: Maybe a)
    unX (XMatrix m) = liftM matrix $ sequence (map (\ i -> unX (m ! i)) (indices m))
--  wireName _  = "Matrix"
    repType Witness = MatrixTy (size (error "witness" :: ix)) (repType (Witness :: Witness a))
    toRep (XMatrix m) = RepValue (concatMap (unRepValue . toRep) $ M.toList m)
    fromRep (RepValue xs) = XMatrix $ M.matrix $ fmap (fromRep . RepValue) $ unconcat xs
	    where unconcat [] = []
		  unconcat ys = take len ys : unconcat (drop len ys)

		  len = Prelude.length xs `div` size (error "witness" :: ix)

--  showWire _ = show
instance (Size ix) => Rep (Unsigned ix) where
    type W (Unsigned ix) = ix
    data X (Unsigned ix) = XUnsigned (WireVal (Unsigned ix))
    optX (Just b)       = XUnsigned $ return b
    optX Nothing        = XUnsigned $ fail "Wire Int"
    unX (XUnsigned (WireVal a))     = return a
    unX (XUnsigned WireUnknown)   = fail "Wire Int"
    repType _          = U (size (error "Wire/Unsigned" :: ix))
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

instance (Size ix) => Rep (Signed ix) where
    type W (Signed ix) = ix
    data X (Signed ix) = XSigned (WireVal (Signed ix))
    optX (Just b)       = XSigned $ return b
    optX Nothing        = XSigned $ fail "Wire Int"
    unX (XSigned (WireVal a))     = return a
    unX (XSigned WireUnknown)   = fail "Wire Int"
--  wireName _      = "Signed"
    repType _          = S (size (error "Wire/Signed" :: ix))
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

-----------------------------------------------------------------------------
-- The grandfather of them all, functions.

instance (Size ix, Rep a, Rep ix) => Rep (ix -> a) where
    type W (ix -> a) = MUL ix (W a)
    data X (ix -> a) = XFunction (ix -> X a)

    optX (Just f) = XFunction $ \ ix -> optX (Just (f ix))
    optX Nothing    = XFunction $ const (optX Nothing)

    -- assumes total function
    unX (XFunction f) = return (Maybe.fromJust . unX . f)

    repType Witness = MatrixTy (size (error "witness" :: ix)) (repType (Witness :: Witness a))

    -- reuse the matrix encodings here
    -- TODO: work out how to remove the Size ix constraint,
    -- and use Rep ix somehow instead.
    toRep (XFunction f) = toRep (XMatrix $ M.forAll f)
    fromRep (RepValue xs) = XFunction $ \ ix ->
	case fromRep (RepValue xs) of
	   XMatrix m -> m M.! ix

-----------------------------------------------------------------------------

log2 :: (Integral a) => a -> a
log2 0 = 0
log2 1 = 1
log2 n = log2 (n `div` 2) + 1

-- Perhaps not, because what does X0 really mean over a wire, vs X1.
instance Rep X0 where
    type W X0 = X0
    data X X0 = X0'
    optX _ = X0'
    unX X0' = return X0
    repType _  = V 0
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

instance (Integral x, Size x) => Rep (X0_ x) where
    type W (X0_ x) = LOG (SUB (X0_ x) X1)
    data X (X0_ x)  = XX0 (WireVal (X0_ x))
    optX (Just x)   = XX0 $ return x
    optX Nothing    = XX0 $ fail "X0_"
    unX (XX0 (WireVal a)) = return a
    unX (XX0 WireUnknown) = fail "X0_"
--  wireName _  = "X" ++ show (size (error "wireName" :: X0_ x))
    repType _  = U (log2 $ (size (error "repType" :: X0_ x) - 1))
    toRep = toRepFromIntegral
    fromRep = sizedFromRepToIntegral
    showRep = showRepDefault

instance (Integral x, Size x) => Rep (X1_ x) where
    type W (X1_ x)  = LOG (SUB (X1_ x) X1)
    data X (X1_ x)  = XX1 (WireVal (X1_ x))
    optX (Just x)   = XX1 $ return x
    optX Nothing    = XX1 $ fail "X1_"
    unX (XX1 (WireVal a)) = return a
    unX (XX1 WireUnknown) = fail "X1_"
--  wireName _  = "X" ++ show (size (error "wireName" :: X1_ x))
    repType _  = U (log2 $ (size (error "repType" :: X1_ x) - 1))
    toRep = toRepFromIntegral
    fromRep = sizedFromRepToIntegral
    showRep = showRepDefault

-- This is a version of fromRepToIntegral that
-- check to see if the result is inside the size bounds.

sizedFromRepToIntegral :: forall w . (Rep w, Integral w, Size w) => RepValue -> X w
sizedFromRepToIntegral w
        | val_integer >= toInteger (size (error "witness" :: w)) = unknownX
        | otherwise                                             = val
  where
        val_integer :: Integer
        val_integer = fromRepToInteger w

        val :: X w
        val = fromRepToIntegral w

-----------------------------------------------------------------

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

