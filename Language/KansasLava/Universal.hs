{-# LANGUAGE ExistentialQuantification, FlexibleContexts, ScopedTypeVariables #-}

module Language.KansasLava.Universal where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils

-- 'Uni' is a universal signal. Called Pad for now. 
-- TODO: rename as Pad.

data Pad = StdLogic (Seq Bool)
         | forall a . (Rep a) => StdLogicVector (Seq a)
         | GenericPad Integer
	 | TheClk
	 | TheRst
	 | TheClkEn


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
fromUni _ = Nothing

fromUni' :: forall a . (Rep a) => Pad -> Seq a
fromUni' a = case fromUni a of
               Nothing -> error "fromUni' failed"
               Just x  -> x