{-# LANGUAGE CPP, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables,
             TypeFamilies, RankNTypes, DataKinds #-}

module Language.KansasLava.Universal where

import GHC.TypeLits

import Data.Sized.Fin

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils

import qualified Language.KansasLava.Stream as S


-- 'Uni' is a universal signal. Called Pad for now.
-- TODO: rename as Pad.

data Pad c = StdLogic (Signal c Bool)
         | forall a . (Rep a) => StdLogicVector (Signal c a)
         | GenericPad Integer
	 | TheClk
	 | TheRst
	 | TheClkEn

rank2MapPad :: (forall a c . Rep a => Signal c a -> Signal c a) -> Pad c -> Pad c
rank2MapPad f (StdLogic ss)       = StdLogic       (f ss)
rank2MapPad f (StdLogicVector ss) = StdLogicVector (f ss)
rank2MapPad _f other              = other


-- | Get the type of a pad.
padStdLogicType :: Pad c -> StdLogicType
padStdLogicType (StdLogic _)       = SL
padStdLogicType (StdLogicVector s) = SLV $ widthS s
padStdLogicType (GenericPad _)     = G
padStdLogicType (TheClk) 	   = SL
padStdLogicType (TheRst) 	   = SL
padStdLogicType (TheClkEn) 	   = SL

instance Show (Pad c) where
        show (StdLogic sq)       = "StdLogic " ++ show sq
        show (StdLogicVector sq) = "StdLogicVector " ++ show sq
        show (GenericPad i)      = "Generic " ++ show i
        show (TheClk)            = "Clk"
        show (TheRst)            = "Rst"
        show (TheClkEn)          = "ClkEn"

-- NOTE: (2) Also, we need to match on Boolean.
toUni :: (Rep a) => Signal c a -> Pad c
toUni = StdLogicVector

fromUni :: forall a c . (Rep a) => Pad c -> Maybe (Signal c a)
fromUni (StdLogicVector sig)
        | widthS sig == widthS (error "witness" :: Signal c a) =  return (unsafeId sig)
fromUni (StdLogic sig)
        | widthS sig == widthS (error "witness" :: Signal c a) =  return (unsafeId sig)
fromUni _ = Nothing

fromUni' :: forall a c . (Rep a) => Pad c -> Signal c a
fromUni' a = case fromUni a of
               Nothing -> error "fromUni' failed"
               Just x  -> x

--------------------------------------------------
newtype ExternalStdLogicVector (x :: Nat) = ExternalStdLogicVector RepValue
        deriving Show

instance (SingI ix) => Rep (ExternalStdLogicVector ix) where
    type W (ExternalStdLogicVector ix) = ix
    data X (ExternalStdLogicVector ix) = XExternalStdLogicVector (ExternalStdLogicVector ix)

    optX (Just b)       = XExternalStdLogicVector $ b
    optX Nothing        = XExternalStdLogicVector
                        $ ExternalStdLogicVector
                        $ RepValue
#if MIN_VERSION_singletons(2, 4, 0)
                        $ replicate (fromIntegral(fromSing (sing :: Sing ix))) Nothing
#else
                        $ replicate (fromInteger(fromNat (sing :: Sing ix))) Nothing
#endif
    unX (XExternalStdLogicVector a) = return a
#if MIN_VERSION_singletons(2, 4, 0)
    repType _          = V (fromIntegral(fromSing (sing :: Sing ix)))
#else
    repType _          = V (fromInteger(fromNat (sing :: Sing ix)))
#endif
    toRep (XExternalStdLogicVector (ExternalStdLogicVector a)) = a
    fromRep a = XExternalStdLogicVector (ExternalStdLogicVector a)
    showRep = showRepDefault

-- TODO: should be Stream
padToRepValues :: Pad c -> [RepValue]
padToRepValues (StdLogic s)             = S.toList $ fmap toRep $ shallowXS s
padToRepValues (StdLogicVector s)       = S.toList $ fmap toRep $ shallowXS s
padToRepValues other                    = error $ "can not find RepValue for " ++ show other

repValuesToPad :: (Clock c) => Pad c -> [RepValue] -> Pad c
repValuesToPad (StdLogic s) rep         = StdLogic (padToPad s rep)
repValuesToPad (StdLogicVector s) rep   = StdLogicVector (padToPad s rep)
repValuesToPad other _ = error $ "can not find Pad for " ++ show other

-- internal
padToPad :: forall a c . (Rep a, Clock c) => Signal c a -> [RepValue] -> Signal c a
padToPad _s rep = id
        $ mkShallowXS
        $ fmap fromRep
        $ S.fromList
        $ rep

