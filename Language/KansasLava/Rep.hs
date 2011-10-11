{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts, DeriveDataTypeable,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp, EmptyDataDecls, TypeSynonymInstances, TypeOperators, TemplateHaskell  #-}
-- | KansasLava is designed for generating hardware circuits. This module
-- provides a 'Rep' class that allows us to model, in the shallow embedding of
-- KL, two important features of hardware signals. First, all signals must have
-- some static width, as they will be synthsized to a collection of hardware
-- wires. Second, a value represented by a signal may be unknown, in part or in
-- whole.
module Language.KansasLava.Rep 
	( module Language.KansasLava.Rep
	, module Language.KansasLava.Rep.TH
	, module Language.KansasLava.Rep.Class
	) where

import Language.KansasLava.Types
import Control.Monad (liftM)
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Sized.Matrix hiding (S)
import qualified Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Data.Sized.Signed as S
import Data.Word
--import qualified Data.Maybe as Maybe
import Data.Traversable(sequenceA)
import qualified Data.Sized.Sampled as Sampled

import Language.KansasLava.Rep.TH
import Language.KansasLava.Rep.Class


-- | Check to see if all bits in a bitvector (represented as a Matrix) are
-- valid. Returns Nothing if any of the bits are unknown.
allOkayRep :: (Size w) => Matrix w (X Bool) -> Maybe (Matrix w Bool)
allOkayRep m = sequenceA $ fmap prj m
  where prj (XBool Nothing) = Nothing
        prj (XBool (Just v)) = Just v


------------------------------------------------------------------------------------

instance Rep Bool where
    type W Bool     = X1
    data X Bool     = XBool (Maybe Bool)
    optX (Just b)   = XBool $ return b
    optX Nothing    = XBool $ fail "Wire Bool"
    unX (XBool (Just v))  = return v
    unX (XBool Nothing) = fail "Wire Bool"
    repType _  = B
    toRep (XBool v)   = RepValue [v]
    fromRep (RepValue [v]) = XBool v
    fromRep rep    = error ("size error for Bool : " ++ show (Prelude.length $ unRepValue rep) ++ " " ++ show rep)
    showRep (XBool Nothing)      = "?"
    showRep (XBool (Just True))  = "H"
    showRep (XBool (Just False)) = "L"

$(repIntegral ''Int     (S 32)) -- a lie on 64-bit machines??

$(repIntegral ''Word8   (U  8))
$(repIntegral ''Word32  (U 32))

instance Rep () where
    type W ()     = X0
    data X ()   = XUnit (Maybe ())
    optX (Just b)   = XUnit $ return b
    optX Nothing    = XUnit $ fail "Wire ()"
    unX (XUnit (Just v))  = return v
    unX (XUnit Nothing) = fail "Wire ()"
    repType _  = V 1   -- should really be V 0 TODO
    toRep _ = RepValue []
    fromRep _ = XUnit $ return ()
    showRep _ = "()"

-- | Integers are unbounded in size. We use the type 'IntegerWidth' as the
-- associated type representing this size in the instance of Rep for Integers.
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

-- TODO: fix this to use :> as the basic internal type.

instance (Rep a, Rep b) => Rep (a :> b) where
    type W (a :> b)  = ADD (W a) (W b)
    data X (a :> b)     = XCell (X a, X b)
    optX (Just (a :> b))   = XCell (pureX a, pureX b)
    optX Nothing        = XCell (optX (Nothing :: Maybe a), optX (Nothing :: Maybe b))
    unX (XCell (a,b)) = do x <- unX a
                           y <- unX b
                           return (x :> y)

    repType Witness = TupleTy [repType (Witness :: Witness a), repType (Witness :: Witness b)]

    toRep (XCell (a,b)) = RepValue (avals ++ bvals)
        where (RepValue avals) = toRep a
              (RepValue bvals) = toRep b
    fromRep (RepValue vs) = XCell ( fromRep (RepValue (take size_a vs))
                  , fromRep (RepValue (drop size_a vs))
                  )
        where size_a = typeWidth (repType (Witness :: Witness a))
    showRep (XCell (a,b)) = showRep a ++ " :> " ++ showRep b


instance (Rep a, Rep b) => Rep (a,b) where
    type W (a,b)  = ADD (W a) (W b)
    data X (a,b)        = XTuple (X a, X b)
    optX (Just (a,b))   = XTuple (pureX a, pureX b)
    optX Nothing        = XTuple (optX (Nothing :: Maybe a), optX (Nothing :: Maybe b))
    unX (XTuple (a,b)) = do x <- unX a
                            y <- unX b
                            return (x,y)

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
    repType _  = TupleTy [ B, repType (Witness :: Witness a)]

    toRep (XMaybe (a,b)) = RepValue (avals ++ bvals)
        where (RepValue avals) = toRep a
              (RepValue bvals) = toRep b
    fromRep (RepValue vs) = XMaybe ( fromRep (RepValue (take 1 vs))
                  , fromRep (RepValue (drop 1 vs))
                  )
    showRep (XMaybe (XBool Nothing,_a)) = "?"
    showRep (XMaybe (XBool (Just True),a)) = "Just " ++ showRep a
    showRep (XMaybe (XBool (Just False),_)) = "Nothing"

instance (Size ix, Rep a) => Rep (Matrix ix a) where
    type W (Matrix ix a) = MUL ix (W a)
    data X (Matrix ix a) = XMatrix (Matrix ix (X a))
    optX (Just m)   = XMatrix $ fmap (optX . Just) m
    optX Nothing    = XMatrix $ forAll $ \ _ -> optX (Nothing :: Maybe a)
    unX (XMatrix m) = liftM matrix $ mapM (\ i -> unX (m ! i)) (indices m)
    repType Witness = MatrixTy (size (error "witness" :: ix)) (repType (Witness :: Witness a))
    toRep (XMatrix m) = RepValue (concatMap (unRepValue . toRep) $ M.toList m)
    fromRep (RepValue xs) = XMatrix $ M.matrix $ fmap (fromRep . RepValue) $ unconcat xs
	    where unconcat [] = []
		  unconcat ys = take len ys : unconcat (drop len ys)

		  len = Prelude.length xs `div` size (error "witness" :: ix)

instance (Size ix) => Rep (Unsigned ix) where
    type W (Unsigned ix) = ix
    data X (Unsigned ix) = XUnsigned (Maybe (Unsigned ix))
    optX (Just b)       = XUnsigned $ return b
    optX Nothing        = XUnsigned $ fail "Wire Int"
    unX (XUnsigned (Just a))     = return a
    unX (XUnsigned Nothing)   = fail "Wire Int"
    repType _          = U (size (error "Wire/Unsigned" :: ix))
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

instance (Size ix) => Rep (Signed ix) where
    type W (Signed ix) = ix
    data X (Signed ix) = XSigned (Maybe (Signed ix))
    optX (Just b)       = XSigned $ return b
    optX Nothing        = XSigned $ fail "Wire Int"
    unX (XSigned (Just a))     = return a
    unX (XSigned Nothing)   = fail "Wire Int"
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
    optX Nothing  = XFunction $ const $ unknownX

    unX (XFunction f) = return (\ a -> 
        let fromJust' (Just x) = x
            fromJust' _ = error $ show ("X",repType (Witness :: Witness (ix -> a)), showRep (optX (Just a) :: X ix))
        in (fromJust' . unX . f) a)

    repType Witness = MatrixTy (size (error "witness" :: ix)) (repType (Witness :: Witness a))

    -- reuse the matrix encodings here
    -- TODO: work out how to remove the Size ix constraint,
    -- and use Rep ix somehow instead.
    toRep (XFunction f) = toRep (XMatrix $ M.forAll f)
    fromRep (RepValue xs) = XFunction $ \ ix ->
        case fromRep (RepValue xs) of
           XMatrix m -> m M.! ix

{-
infixl 4 `apX`

-- The applicative functor style 'ap'.
apX :: (Rep a, Rep b) => X (a -> b) -> X a -> X b
apX (XFunction f) a = f a

-- The apX-1 function. Useful when building applicative functor style things
-- on top of 'X'.
unapX :: (Rep a, Rep b) => (X a -> X b) -> X (a -> b) 
unapX f = XFunction f
-} 

-----------------------------------------------------------------------------

-- | Calculate the base-2 logrithim of a integral value.
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
    data X (X0_ x)  = XX0 (Maybe (X0_ x))
    optX (Just x)   = XX0 $ return x
    optX Nothing    = XX0 $ fail "X0_"
    unX (XX0 (Just a)) = return a
    unX (XX0 Nothing) = fail "X0_"
    repType _  = U (log2 (size (error "repType" :: X0_ x) - 1))
    toRep = toRepFromIntegral
    fromRep = sizedFromRepToIntegral
    showRep = showRepDefault

instance (Integral x, Size x) => Rep (X1_ x) where
    type W (X1_ x)  = LOG (SUB (X1_ x) X1)
    data X (X1_ x)  = XX1 (Maybe (X1_ x))
    optX (Just x)   = XX1 $ return x
    optX Nothing    = XX1 $ fail "X1_"
    unX (XX1 (Just a)) = return a
    unX (XX1 Nothing) = fail "X1_"
    repType _  = U (log2 (size (error "repType" :: X1_ x) - 1))
    toRep = toRepFromIntegral
    fromRep = sizedFromRepToIntegral
    showRep = showRepDefault

-- | This is a version of fromRepToIntegral that
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
	data X (Sampled.Sampled m ix) = XSampled (Maybe (Sampled.Sampled m ix))
	optX (Just b)	    = XSampled $ return b
	optX Nothing	    = XSampled $ fail "Wire Sampled"
	unX (XSampled (Just a))     = return a
	unX (XSampled Nothing)   = fail "Wire Sampled"
	repType _   	    = SampledTy (size (error "witness" :: m)) (size (error "witness" :: ix))
	toRep (XSampled Nothing) = unknownRepValue (Witness :: Witness (Sampled.Sampled m ix))
	toRep (XSampled (Just a))   = RepValue $ fmap Just $ M.toList $ Sampled.toMatrix a
	fromRep r = optX (liftM (Sampled.fromMatrix . M.fromList) $ getValidRepValue r)
	showRep = showRepDefault

