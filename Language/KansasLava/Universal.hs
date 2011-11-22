{-# LANGUAGE ExistentialQuantification, FlexibleContexts, ScopedTypeVariables #-}

module Language.KansasLava.Universal where

import Data.Sized.Ix

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils

-- 'Uni' is a universal signal. Called Pad for now. 
-- TODO: rename as Pad.

data Pad = StdLogic (Seq Bool)
         | forall a x 
                . (Size (W a), Show a, Rep a)
                => StdLogicVector (Seq a)
         | GenericPad Integer
	 | TheClk
	 | TheRst
	 | TheClkEn


-- | Get the type of a pad.
padStdLogicType :: Pad -> StdLogicType
padStdLogicType (StdLogic _)       = SL
padStdLogicType (StdLogicVector s) = SLV $ size (untype s)
    where untype :: (Size (W a)) => Seq a -> W a
          untype = error "untype"
padStdLogicType (GenericPad _)        = G
padStdLogicType (TheClk) 	      = SL
padStdLogicType (TheRst) 	      = SL
padStdLogicType (TheClkEn) 	      = SL

instance Show Pad where
        show (StdLogic sq)       = "StdLogic " ++ show sq
        show (StdLogicVector sq) = "StdLogicVector " ++ show sq
        show (GenericPad i)      = "Generic " ++ show i
        show (TheClk)            = "Clk"
        show (TheRst)            = "Rst"
        show (TheClkEn)          = "ClkEn"

-- NOTE: suspect that you do not need Size or Show here
-- NOTE: (2) Also, we need to match on Boolean.
toUni :: (Size (W a), Show a, Rep a) => Seq a -> Pad
toUni = StdLogicVector

fromUni :: forall a . (Size (W a), Show a, Rep a) => Pad -> Maybe (Seq a)
fromUni (StdLogicVector sig) 
        | size (untype sig) == size (error "witness" :: W a) =  return (unsafeId sig)
    where untype :: forall a .  (Size (W a)) => Seq a -> W a
          untype = error "untype"
fromUni _ = Nothing
