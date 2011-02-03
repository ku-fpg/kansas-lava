{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts, DeriveDataTypeable,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp, TypeSynonymInstances  #-}

module Language.KansasLava.Shallow where

import Language.KansasLava.Stream as Stream
import Language.KansasLava.Types
import Control.Monad (liftM)
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Sized.Matrix hiding (S)
import qualified Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Data.Sized.Signed as S
import qualified Data.Sized.Sampled as Sampled
import Data.Word
import Data.Bits
import qualified Data.Traversable as T
import qualified Data.Maybe as Maybe

-- | A 'Rep a' is an 'a' value that we 'Rep'resent, aka we can push it over a wire.
class {- (Size (W w)) => -} Rep w where
    type W w

    -- | a way of adding unknown inputs to this wire.
    data X w

    -- | check for bad things
    unX :: X w -> Maybe w

    -- | and, put the good or bad things back.
    optX :: Maybe w -> X w

    -- | Each wire has a known type.
    -- TODO: call repTupe
    repType :: Witness w -> Type

    -- | convert to binary (rep) format
    toRep   :: X w -> RepValue
    -- | convert from binary (rep) format
    fromRep :: RepValue -> X w

    -- show the value (in its Haskell form, default is the bits)
    -- TODO: remove Witness, its not needed any longer
    showRep :: Witness w -> X w -> String
    showRep w x = show (toRep x)


-- Give me all possible (non-X) representations (2^n of them).
allReps :: (Rep w) => Witness w -> [RepValue]
allReps w = [ RepValue (fmap WireVal count) | count <- counts n ]
   where
    n = repWidth w
    counts :: Int -> [[Bool]]
    counts 0 = [[]]
    counts n = [ x : xs |  xs <- counts (n-1), x <- [False,True] ]

-- | Figure out the width in bits of a type.

repWidth :: (Rep w) => Witness w -> Int
repWidth w = typeWidth (repType w)


-- | unknownRepValue returns a RepValue that is completely filled with 'X'.
unknownRepValue :: (Rep w) => Witness w -> RepValue
unknownRepValue w = RepValue [ WireUnknown | _ <- [1..repWidth w]]

allOkayRep :: (Size w) => Matrix w (X Bool) -> Maybe (Matrix w Bool)
allOkayRep m | okay      = return (fmap (\ (XBool (WireVal a)) -> a) m)
         | otherwise = Nothing
    where okay = and (map (\ (XBool x) -> case x of
                       WireUnknown -> False
                       _ -> True) (M.toList m))

-- | return a 'w' inside X.
pureX :: (Rep w) => w -> X w
pureX = optX . Just

unknownX :: forall w . (Rep w) => X w
unknownX = optX (Nothing :: Maybe w)

-- This is not wired into the class because of the extra 'Show' requirement.

showRepDefault :: forall w. (Show w, Rep w) => Witness w -> X w -> String
showRepDefault w v = case unX v :: Maybe w of
            Nothing -> "?"
            Just v' -> show v'


-- TODO: consider not using the sized types here, and just using standard counters.
{-
toRepFromSigned :: forall w v . (Rep v, Integral v, Size w) => w -> v -> X v -> RepValue
toRepFromSigned w1 w2 v =
        RepValue $ M.toList $ case unX v :: Maybe v of
                Nothing -> (forAll $ \ i -> WireUnknown) :: M.Matrix w (WireVal Bool)
                Just v' -> fmap WireVal $ S.toMatrix (fromIntegral v')

fromRepToSigned :: forall w v . (Rep v, Integral v, Size w) => w -> v -> RepValue -> X v
fromRepToSigned w1 w2 r = optX (fmap (\ xs -> fromIntegral $ S.fromMatrix (M.matrix xs :: Matrix w Bool))
                     (getValidRepValue r) :: Maybe v)

toRepFromUnsigned :: forall w v . (Rep v, Integral v, Size w) => w -> v -> X v -> RepValue
toRepFromUnsigned w1 w2 v =
        RepValue $ M.toList $ case unX v :: Maybe v of
                Nothing -> (forAll $ \ i -> WireUnknown) :: M.Matrix w (WireVal Bool)
                Just v' -> fmap WireVal $ U.toMatrix (fromIntegral v')

fromRepToUnsigned :: forall w v . (Rep v, Integral v, Size w) => w -> v -> RepValue -> X v
fromRepToUnsigned w1 w2 r = optX (fmap (\ xs -> fromIntegral $ U.fromMatrix (M.matrix xs :: Matrix w Bool))
                     (getValidRepValue r) :: Maybe v)
-}

toRepFromIntegral :: forall v . (Rep v, Integral v) => X v -> RepValue
toRepFromIntegral v = case unX v :: Maybe v of
                 Nothing -> unknownRepValue (Witness :: Witness v)
                 Just v' -> RepValue
                    $ take (repWidth (Witness :: Witness v))
                    $ map WireVal
                    $ map odd
                    $ iterate (`div` 2)
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
--
cmpRep :: (Rep a) => Witness a -> X a -> X a -> Bool
cmpRep Witness g v = toRep g `cmpRepValue` toRep v

-- basic conversion to trace representation
toTrace :: forall w . (Rep w) => Stream (X w) -> TraceStream
toTrace stream = TraceStream (repType (Witness :: Witness w)) [toRep xVal | xVal <- Stream.toList stream ]

fromTrace :: (Rep w) => TraceStream -> Stream (X w)
fromTrace (TraceStream _ list) = Stream.fromList [fromRep val | val <- list]

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

{-
instance RepWire Bool where
    type WIDTH Bool = X1
    toWireRep m = return $ m ! 0
    fromWireRep v   = matrix [v]
    showRepWire _ (WireUnknown) = "?"
    showRepWire _ (WireVal True) = "T"
    showRepWire _ (WireVal False) = "F"
-}

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

{-
instance RepWire Int where
    type WIDTH Int  = X32
    toWireRep = return . fromIntegral . U.fromMatrix
    fromWireRep = U.toMatrix . fromIntegral
    showRepWire _ = show
-}

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

{-
instance RepWire Word8 where
    type WIDTH Word8 = X8
    toWireRep = return . fromIntegral . U.fromMatrix
    fromWireRep = U.toMatrix . fromIntegral
    showRepWire _ = show
-}

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

{-
instance RepWire Word32 where
    type WIDTH Word32 = X32
    toWireRep = return . fromIntegral . U.fromMatrix
    fromWireRep = U.toMatrix . fromIntegral
    showRepWire _ = show
-}

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
    showRep _ _ = "()"

{-
instance RepWire () where
    type WIDTH () = X1  -- should really be X0
    toWireRep _ = return ()
    fromWireRep () = M.matrix [True]
    showRepWire _ = show
-}

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
    showRep _ (XInteger v) = show v

{-
instance RepWire Integer where
    type WIDTH Integer = X0
    showRepWire _   = show
-}

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
{-
    wireCapture (D d) = [ (repType (error "wireCapture (a,)" :: a),Port (Var "o0") $ E $ eFst)
                , (repType (error "wireCapture (,b)" :: b),Port (Var "o0") $ E $ eSnd)
                ]
           where
        eFst = Entity (Name "Lava" "fst")
                  [(Var "o0",repType (error "wireGenerate (a,)" :: a))]
                  [(Var "i0",repType (error "wireGenerate (a,b)" :: (a,b)),d)]
                  []
        eSnd = Entity (Name "Lava" "snd")
                  [(Var "o0",repType (error "wireGenerate (,b)" :: b))]
                  [(Var "i0",repType (error "wireGenerate (a,b)" :: (a,b)),d)]
                  []


    wireGenerate vs0 = (D (Port (Var "o0") $ E ePair),vs2)
       where
        (D p1,vs1) = wireGenerate vs0 :: (D a,[String])
        (D p2,vs2) = wireGenerate vs1 :: (D b,[String])
        ePair = Entity (Name "Lava" "pair")
                  [(Var "o0",repType (error "wireGenerate (a,b)" :: (a,b)))]
                  [(Var "i0",repType (error "wireGenerate (a,)" :: a),p1)
                  ,(Var "i1",repType (error "wireGenerate (,b)" :: b),p2)
                  ]
                  []
-}

    toRep (XTuple (a,b)) = RepValue (avals ++ bvals)
        where (RepValue avals) = toRep a
              (RepValue bvals) = toRep b
    fromRep (RepValue vs) = XTuple ( fromRep (RepValue (take size_a vs))
                  , fromRep (RepValue (drop size_a vs))
                  )
        where size_a = typeWidth (repType (Witness :: Witness a))
    showRep _ (XTuple (a,b)) = "(" ++ showRep (Witness :: Witness a) a ++ "," ++ showRep (Witness :: Witness b) b ++ ")"

{-
instance (t ~ ADD (WIDTH a) (WIDTH b), Size t, Enum t, RepWire a, RepWire b) => RepWire (a,b) where
    type WIDTH (a,b)    = ADD (WIDTH a) (WIDTH b)
--  toWireRep m         = return $ m ! 0
    fromWireRep (a,b)   = M.matrix (M.toList (fromWireRep a) ++ M.toList (fromWireRep b))
    fromWireXRep w (a,b)    = M.matrix (M.toList (fromWireXRep (error "witness" :: a) a) ++
                        M.toList (fromWireXRep (error "witness" :: b) b))
    showRepWire ~(a,b) (x,y) = "(" ++ showRepWire a x ++ "," ++ showRepWire b y ++ ")"
-}

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
{-
--  TO ADD
    toRep _ (a,b) = RepValue (avals ++ bvals)
        where (RepValue avals) = toRep (Witness :: Witness a) a
              (RepValue bvals) = toRep (Witness :: Witness b) b
    fromRep w (RepValue vs) = ( fromRep (Witness :: Witness a) (RepValue (take size_a vs))
                  , fromRep (Witness :: Witness b) (RepValue (drop size_a vs))
                  )
        where size_a = typeWidth (repType w)
-}

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
    showRep _ (XTriple (a,b,c)) = "(" ++ showRep (Witness :: Witness a) a ++
                "," ++ showRep (Witness :: Witness b) b ++
                "," ++ showRep (Witness :: Witness c) c ++ ")"




{-
instance (t ~ ADD (WIDTH a) (ADD (WIDTH b) (WIDTH c)), Size t, Enum t, RepWire a, RepWire b,RepWire c) => RepWire (a,b,c) where
    type WIDTH (a,b,c)  = ADD (WIDTH a) (ADD (WIDTH b) (WIDTH c))
--  toWireRep m         = return $ m ! 0
    fromWireRep (a,b,c)     = M.matrix (M.toList (fromWireRep a) ++ M.toList (fromWireRep b) ++ M.toList (fromWireRep c))
    fromWireXRep w (a,b,c)  = M.matrix (M.toList (fromWireXRep (error "witness" :: a) a) ++
                        M.toList (fromWireXRep (error "witness" :: b) b) ++
                        M.toList (fromWireXRep (error "witness" :: c) c))
    showRepWire ~(a,b,c) (x,y,z) = "(" ++ showRepWire a x ++ "," ++ showRepWire b y ++ "," ++ showRepWire c z ++ ")"
-}

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
    showRep w (XMaybe (XBool WireUnknown,a)) = "?"
    showRep w (XMaybe (XBool (WireVal True),a)) = "Just " ++ showRep (Witness :: Witness a) a
    showRep w (XMaybe (XBool (WireVal False),a)) = "Nothing"

--instance Size (ADD X1 a) => Size a where
{-
instance (RepWire a, Size (ADD X1 (WIDTH a))) => RepWire (Maybe a) where
    type WIDTH (Maybe a) = WIDTH (Bool,a)
--  toWireRep = return . fromIntegral . U.fromMatrix
    fromWireRep Nothing  = M.matrix $ take sz $ repeat False
        where sz = 1 + size (error "witness" :: (WIDTH a))
    fromWireRep (Just a) = M.matrix (True : M.toList (fromWireRep a))

    fromWireXRep w (en,val) = M.matrix (M.toList (fromWireXRep (error "witness" :: Bool) en) ++
                        M.toList (fromWireXRep (error "witness" :: a) val))

    showRepWire w (WireUnknown,a) = "?"
    showRepWire w (WireVal True,a) = "Just " ++ showRepWire (error "witness" :: a) a
    showRepWire w (WireVal False,a) = "Nothing"
-}

instance (Size ix, Rep a) => Rep (Matrix ix a) where
    type W (Matrix ix a) = MUL ix (W a)
    data X (Matrix ix a) = XMatrix (Matrix ix (X a))
    optX (Just m)   = XMatrix $ fmap (optX . Just) m
    optX Nothing    = XMatrix $ forAll $ \ ix -> optX (Nothing :: Maybe a)
    unX (XMatrix m) = liftM matrix $ sequence (map (\ i -> unX (m ! i)) (indices m))
--  wireName _  = "Matrix"
    repType Witness = MatrixTy (size (error "witness" :: ix)) (repType (Witness :: Witness a))
    toRep (XMatrix m) = RepValue (concatMap (unRepValue . toRep) $ M.toList m)
    fromRep (RepValue xs) = XMatrix $ M.matrix $ fmap (fromRep . RepValue) $ unconcat xs
	    where unconcat [] = []
		  unconcat xs = take len xs : unconcat (drop len xs)
		
		  len = Prelude.length xs `div` size (error "witness" :: ix)

--  showWire _ = show



{-
instance forall a ix t . (t ~ WIDTH a, Size t, Size (MUL ix t), Enum (MUL ix t), RepWire a, Size ix, Rep a) => RepWire (Matrix ix a) where
    type WIDTH (Matrix ix a) = MUL ix (WIDTH a)

--  toWireRep :: Matrix (WIDTH w) Bool -> Matrix ix a
    toWireRep = T.traverse toWireRep
          . columns
          . squash

--  fromWireRep :: Matrix ix a -> Matrix (WIDTH w) Bool
    fromWireRep = squash . joinColumns . T.traverse fromWireRep

--  fromWireRep :: Matrix ix (X a) -> Matrix (WIDTH w) (X Bool)
    fromWireXRep w = squash . joinColumns . T.traverse (fromWireXRep (error "witness" :: a))


    showRepWire _ = show . M.toList . fmap (M.S . showRepWire (error "show/Matrix" :: a))
-}
instance (Size ix) => Rep (Unsigned ix) where
    type W (Unsigned ix) = ix
    data X (Unsigned ix) = XUnsigned (WireVal (Unsigned ix))
    optX (Just b)       = XUnsigned $ return b
    optX Nothing        = XUnsigned $ fail "Wire Int"
    unX (XUnsigned (WireVal a))     = return a
    unX (XUnsigned WireUnknown)   = fail "Wire Int"
    repType x          = U (size (error "Wire/Unsigned" :: ix))
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault
{-
instance (Enum ix, Size ix) => RepWire (Unsigned ix) where
    type WIDTH (Unsigned ix) = ix
    fromWireRep a = U.toMatrix a
    toWireRep = return . U.fromMatrix
    showRepWire _ = show
-}
instance (Size ix) => Rep (Signed ix) where
    type W (Signed ix) = ix
    data X (Signed ix) = XSigned (WireVal (Signed ix))
    optX (Just b)       = XSigned $ return b
    optX Nothing        = XSigned $ fail "Wire Int"
    unX (XSigned (WireVal a))     = return a
    unX (XSigned WireUnknown)   = fail "Wire Int"
--  wireName _      = "Signed"
    repType x          = S (size (error "Wire/Signed" :: ix))
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault
{-
instance (Size ix) => RepWire (Signed ix) where
    type WIDTH (Signed ix) = ix
    fromWireRep a = S.toMatrix a
    toWireRep = return . S.fromMatrix
    showRepWire _ = show
-}
{-
instance (Size m, Enum ix, Enum m, Size ix) => RepWire (Sampled.Sampled m ix) where
    type WIDTH (Sampled.Sampled m ix) = ix
    fromWireRep a = Sampled.toMatrix a
    toWireRep = return . Sampled.fromMatrix
    showRepWire _ = show
-}

{-
instance (Size ix) => RepWire (StdLogicVector ix) where
    type WIDTH (StdLogicVector ix) = ix
    fromWireRep (StdLogicVector a) = a
    toWireRep = return . StdLogicVector
    showRepWire _ = show
-}

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

log2 :: Int -> Int
log2 0 = 0
log2 1 = 1
log2 n = log2 (n `shiftR` 1) + 1

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
    fromRep = fromRepToIntegral
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
    fromRep = fromRepToIntegral
    showRep = showRepDefault


-----------------------------------------------------------------------------------------
{-
-- Wire used in simulation *only*, to show ideas.
data ALPHA = ALPHA String
    deriving (Eq, Ord)

instance Show ALPHA where
    show (ALPHA str) = str

instance Rep ALPHA where
    data X ALPHA    = XAlpha (WireVal ALPHA)
    optX (Just b)   = XAlpha $ return b
    optX Nothing    = XAlpha $ fail "Wire ALPHA"
    unX (XAlpha (WireVal v))  = return v
    unX (XAlpha WireUnknown) = fail "Wire ALPHA"
--  wireName _  = "ABC"
    repType _  = U 0

-}
{-
instance RepWire ALPHA where
    type WIDTH ALPHA    = X0
    toWireRep m         = return $ ALPHA ""
    fromWireRep v       = matrix []
    showRepWire _ = show
-}

-----------------------------------------------------------------------------

{-
-- New ideas
-- Only total functions, for now
instance (Rep a, Rep b) => Rep (a -> b) where
	data X (a -> b) = XFunction (a -> b)
			| XFunctionUnknown
	optX (Just f)   = XFunction f
	optX Nothing	= XFunctionUnknown
	unX (XFunction f)      = return $ f
	unX (XFunctionUnknown) = fail "Rep (->)"

	toRep = error "Can not find the rep for functions"
	fromRep = error "Can not find the rep for functions"

	showRep _ _ = "<function>"
-}
	
---applyRep :: (Rep a, Rep b, Signal sig) => sig a -> sig (a -> b) -> sig b
--applyRep = undefined

{-
class (Rep a) => RepIntegral a where
        toRepInteger   :: a -> Integer
        fromRepInteger :: Integer -> a
-}        



