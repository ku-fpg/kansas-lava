{-# LANGUAGE CPP, TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts, DeriveDataTypeable,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp, EmptyDataDecls, TypeSynonymInstances, TypeOperators,
    TemplateHaskell, DataKinds #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

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
import Data.Sized.Fin
import Data.Sized.Matrix hiding (S)
import qualified Data.Sized.Matrix as M
import Data.Sized.Unsigned as U
import Data.Sized.Signed as S
import Data.Word
import Data.Char
import Data.Bits
import Data.Array.Base
import Data.Array.IArray

import GHC.TypeLits

import qualified Data.Sized.Sampled as Sampled

import Language.KansasLava.Rep.TH
import Language.KansasLava.Rep.Class



------------------------------------------------------------------------------------

instance Rep Bool where
    type W Bool     = 1
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
    showRep (XBool (Just True))  = "high"
    showRep (XBool (Just False)) = "low"

$(repIntegral ''Int     (S $ bitSize $ (error "witness" :: Int)))

$(repIntegral ''Word8   (U  8))
$(repIntegral ''Word16  (U 16))
$(repIntegral ''Word32  (U 32))
$(repIntegral ''Word64  (U 64))

instance Rep () where
    type W ()     = 0
    data X ()   = XUnit (Maybe ())
    optX (Just ())   = XUnit $ return () -- we need this strict, so we can use it for consumption management
    optX Nothing    = XUnit $ fail "Wire ()"
    unX (XUnit (Just v))  = return v
    unX (XUnit Nothing) = fail "Wire ()"
    repType _  = V 0
    toRep (XUnit (Just ())) = RepValue []
    toRep (XUnit Nothing) = RepValue []
    fromRep (RepValue []) = XUnit $ return ()
    showRep _ = "()"

-------------------------------------------------------------------------------------
-- Now the containers


instance (Rep a, Rep b) => Rep (a,b) where
    type W (a,b)  = (W a) + (W b)
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
    type W (a,b,c) = (W a) + ((W b) + (W c))
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
    type W (Maybe a) = 1 + W a
    -- not completely sure about this representation
    data X (Maybe a) = XMaybe (X Bool, X a)
    optX b      = XMaybe ( case b of
                  Nothing        -> optX (Nothing :: Maybe Bool)
                  Just Nothing   -> optX (Just False :: Maybe Bool)
                  Just (Just {}) -> optX (Just True :: Maybe Bool)
              , case b of
                Nothing       -> optX (Nothing :: Maybe a)
                Just Nothing  -> optX (Nothing :: Maybe a)
                Just (Just a) -> optX (Just a :: Maybe a)
              )
    unX (XMaybe (a,b)) = case unX a :: Maybe Bool of
                Nothing    -> Nothing
                Just True  -> case unX b of
                                Nothing -> Nothing
                                Just v -> Just (Just v)
                Just False -> Just Nothing
    repType _  = TupleTy [ B, repType (Witness :: Witness a)]

    toRep (XMaybe (a,b)) = RepValue (avals ++ bvals)
        where (RepValue avals) = toRep a
              (RepValue bvals) = toRep b
    fromRep (RepValue vs) = XMaybe
                  ( fromRep (RepValue (take 1 vs))
                  , fromRep (RepValue (drop 1 vs))
                  )
    showRep (XMaybe (XBool Nothing,_a))      = "?"
    showRep (XMaybe (XBool (Just True),a))   = "Just " ++ showRep a
    showRep (XMaybe (XBool (Just False),_)) = "Nothing"

type family Log (n :: Nat) :: Nat

type instance (Log 0) = 0
type instance (Log 1) = 0
type instance (Log 2) = 1
type instance (Log 3) = 2
type instance (Log 4) = 2

instance (SingI x) => Rep (Fin x) where
    -- TODO.  FIXME:  W (Fin x) should be LOG (SUB x 1)
    type W (Fin x) = Log x
    data X (Fin x)  = XSized (Maybe (Fin x))
    optX (Just x)   = XSized $ return x
    optX Nothing    = XSized $ fail "Sized"
    unX (XSized (Just a)) = return a
    unX (XSized Nothing) = fail "Sized"
#if MIN_VERSION_singletons(2, 4, 0)
    repType _  = U (log2 (fromIntegral (fromSing (sing :: Sing x)) - 1))
#else
    repType _  = U (log2 (fromInteger(fromNat (sing :: Sing x)) - 1))
#endif
    toRep = toRepFromIntegral
    fromRep = sizedFromRepToIntegral
    showRep = showRepDefault

instance (SingI ix, Rep a) => Rep (Vector ix a) where
    type W (Vector ix a) = ix * (W a)
    data X (Vector ix a) = XMatrix (Vector ix (X a))
    optX (Just m)   = XMatrix $ fmap (optX . Just) m
    optX Nothing    = XMatrix $ forAll $ \ _ -> optX (Nothing :: Maybe a)
    unX (XMatrix m) = liftM matrix $ mapM (\ i -> unX (m ! i)) (indices m)
    repType Witness = MatrixTy (size (error "witness" :: (Fin ix))) (repType (Witness :: Witness a))
    toRep (XMatrix m) = RepValue (concatMap (unRepValue . toRep) $ elems m)
    fromRep (RepValue xs) = XMatrix $ M.matrix $ fmap (fromRep . RepValue) $ unconcat xs
	    where unconcat [] = []
		  unconcat ys = take len ys : unconcat (drop len ys)

		  len = w_a -- Prelude.length xs `div` size (error "witness" :: ix)

                  w_a = typeWidth (repType (Witness :: Witness a))

instance (SingI ix) => Rep (Unsigned ix) where
    type W (Unsigned ix) = ix
    data X (Unsigned ix) = XUnsigned (Maybe (Unsigned ix))
    optX (Just b)       = XUnsigned $ return b
    optX Nothing        = XUnsigned $ fail "Wire Int"
    unX (XUnsigned (Just a))     = return a
    unX (XUnsigned Nothing)   = fail "Wire Int"
#if MIN_VERSION_singletons(2, 4, 0)
    repType _          = U (fromIntegral (fromSing (sing :: Sing ix)))
#else
    repType _          = U (fromInteger(fromNat (sing :: Sing ix)))
#endif
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

instance (SingI ix) => Rep (Signed ix) where
    type W (Signed ix) = ix
    data X (Signed ix) = XSigned (Maybe (Signed ix))
    optX (Just b)       = XSigned $ return b
    optX Nothing        = XSigned $ fail "Wire Int"
    unX (XSigned (Just a))     = return a
    unX (XSigned Nothing)   = fail "Wire Int"
#if MIN_VERSION_singletons(2, 4, 0)
    repType _          = S (fromIntegral (fromSing (sing :: Sing ix)))
#else
    repType _          = S (fromInteger (fromNat (sing :: Sing ix)))
#endif
    toRep = toRepFromIntegral
    fromRep = fromRepToIntegral
    showRep = showRepDefault

-----------------------------------------------------------------------------

-- | Calculate the base-2 logrithim of a integral value.
log2 :: (Integral a) => a -> a
log2 n | n <= 0 = 0
log2 1 = 1
log2 n = log2 (n `div` 2) + 1

-- | This is a version of fromRepToIntegral that
-- check to see if the result is inside the size bounds.
sizedFromRepToIntegral :: forall w . (Rep w, Integral w, Ix w, Bounded w) => RepValue -> X w
sizedFromRepToIntegral w
        | val_integer >= toInteger (size (error "witness" :: w)) = unknownX
        | otherwise                                             = val
  where
        val_integer :: Integer
        val_integer = fromRepToInteger w

        val :: X w
        val = fromRepToIntegral w

-----------------------------------------------------------------

instance (SingI m, SingI ix) => Rep (Sampled.Sampled m ix) where
        type W (Sampled.Sampled m ix) = ix
	data X (Sampled.Sampled m ix) = XSampled (Maybe (Sampled.Sampled m ix))
	optX (Just b)	    = XSampled $ return b
	optX Nothing	    = XSampled $ fail "Wire Sampled"
	unX (XSampled (Just a))     = return a
	unX (XSampled Nothing)   = fail "Wire Sampled"
#if MIN_VERSION_singletons(2, 4, 0)
        repType _           = SampledTy (fromIntegral (fromSing (sing :: Sing m)))
                                        (fromIntegral (fromSing (sing :: Sing ix)))
#else
	repType _   	    = SampledTy (fromInteger (fromNat (sing :: Sing m)))
                                        (fromInteger (fromNat (sing :: Sing ix)))
#endif
  	toRep (XSampled Nothing) = unknownRepValue (Witness :: Witness (Sampled.Sampled m ix))
	toRep (XSampled (Just a))   = RepValue $ fmap Just $ elems $ Sampled.toVector a
	fromRep r = optX (liftM (Sampled.fromVector . M.matrix) $ getValidRepValue r)
	showRep = showRepDefault
