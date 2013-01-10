{-# LANGUAGE ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, TypeFamilies, RankNTypes, DataKinds #-}

module Language.KansasLava.Universal where

import GHC.TypeLits

import Data.Sized.Sized

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils

import qualified Language.KansasLava.Stream as S


-- 'Uni' is a universal signal. Called Pad for now.
-- TODO: rename as Pad.

data Pad = StdLogic (Seq Bool)
         | forall a . (Rep a) => StdLogicVector (Seq a)
         | GenericPad Integer
	 | TheClk
	 | TheRst
	 | TheClkEn

rank2MapPad :: (forall a . Rep a => Seq a -> Seq a) -> Pad -> Pad
rank2MapPad f (StdLogic ss)       = StdLogic       (f ss)
rank2MapPad f (StdLogicVector ss) = StdLogicVector (f ss)
rank2MapPad _f other              = other


-- | Get the type of a pad.
padStdLogicType :: Pad -> StdLogicType
padStdLogicType (StdLogic _)       = SL
padStdLogicType (StdLogicVector s) = SLV $ widthS s
padStdLogicType (GenericPad _)     = G
padStdLogicType (TheClk) 	   = SL
padStdLogicType (TheRst) 	   = SL
padStdLogicType (TheClkEn) 	   = SL

instance Show Pad where
        show (StdLogic sq)       = "StdLogic " ++ show sq
        show (StdLogicVector sq) = "StdLogicVector " ++ show sq
        show (GenericPad i)      = "Generic " ++ show i
        show (TheClk)            = "Clk"
        show (TheRst)            = "Rst"
        show (TheClkEn)          = "ClkEn"

-- NOTE: (2) Also, we need to match on Boolean.
toUni :: (Rep a) => Seq a -> Pad
toUni = StdLogicVector

fromUni :: forall a . (Rep a) => Pad -> Maybe (Seq a)
fromUni (StdLogicVector sig)
        | widthS sig == widthS (error "witness" :: Seq a) =  return (unsafeId sig)
fromUni (StdLogic sig)
        | widthS sig == widthS (error "witness" :: Seq a) =  return (unsafeId sig)
fromUni _ = Nothing

fromUni' :: forall a . (Rep a) => Pad -> Seq a
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
                        $ replicate (fromInteger(fromNat (sing :: Sing ix))) Nothing
    unX (XExternalStdLogicVector a) = return a

    repType _          = V (fromInteger(fromNat (sing :: Sing ix)))
    toRep (XExternalStdLogicVector (ExternalStdLogicVector a)) = a
    fromRep a = XExternalStdLogicVector (ExternalStdLogicVector a)
    showRep = showRepDefault

-- TODO: should be Stream
padToRepValues :: Pad -> [RepValue]
padToRepValues (StdLogic s)             = S.toList $ fmap toRep $ shallowS s
padToRepValues (StdLogicVector s)       = S.toList $ fmap toRep $ shallowS s
padToRepValues other                    = error $ "can not find RepValue for " ++ show other

repValuesToPad :: Pad -> [RepValue] -> Pad
repValuesToPad (StdLogic s) rep         = StdLogic (padToPad s rep)
repValuesToPad (StdLogicVector s) rep   = StdLogicVector (padToPad s rep)
repValuesToPad other _ = error $ "can not find Pad for " ++ show other

-- internal
padToPad :: forall a . (Rep a) => Seq a -> [RepValue] -> Seq a
padToPad _s rep = id
        $ mkShallowS
        $ fmap fromRep
        $ S.fromList
        $ rep

