{-# LANGUAGE RankNTypes #-}
module Language.KansasLava.Type where

import qualified Data.Set as Set
import Data.Set (Set)


data BaseTy
	= B		-- Bit
	| CB		-- Control Bit (raw bit for clk or rst)
        | ClkTy | RstTy
	| S Int		-- Signed vector
	| U Int  	-- Unsigned vector

	| T		-- Time
			-- What about Float/Double, special, etc.
	deriving (Eq, Ord)

data Ty v = BaseTy BaseTy | TyVar v
	deriving (Eq, Ord)

-- I don't use "_" here, because additional constructors are likely to be defined
-- for BaseTy; and this function should be updated whenever this happens
baseTypeLength B  = 1
baseTypeLength CB = 1
baseTypeLength ClkTy = 1
baseTypeLength RstTy = 1
baseTypeLength (S x) = x
baseTypeLength (U x) = x
baseTypeLength T = 1

-- I don't use "_" here, because additional constructors are likely to be defined
-- for BaseTy; and this function should be updated whenever this happens
baseTypeIsSigned B     = False
baseTypeIsSigned CB    = False
baseTypeIsSigned ClkTy = False
baseTypeIsSigned RstTy = False
baseTypeIsSigned (S x) = True
baseTypeIsSigned (U _) = False
baseTypeIsSigned T     = False

instance Functor Ty where
	fmap f (BaseTy ty)	= BaseTy ty
	fmap f (TyVar v) 	= TyVar (f v)

instance Show BaseTy where
	show B 		= "B"
	show CB 	= "C"
        show ClkTy      = "CLK"
        show RstTy      = "RST"
	show (S i) 	= show i ++ "S"
	show (U i) 	= show i ++ "U"
	show T		= "T"

instance Show v => Show (Ty v) where
	show (BaseTy ty) = show ty
	show (TyVar v) 	 = show v

