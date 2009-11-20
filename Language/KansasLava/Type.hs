{-# LANGUAGE RankNTypes #-}
module Language.KansasLava.Type where

-- | BaseTy captures HDL-representable types.
data BaseTy
	= B		-- Bit
	| CB		-- Control Bit (raw bit for clk or rst)
        | ClkTy | RstTy
	| S Int		-- Signed vector
	| U Int  	-- Unsigned vector

	| T		-- Time
			-- What about Float/Double, special, etc.
	deriving (Eq, Ord)


-- I don't use "_" here, because additional constructors are likely to be defined
-- for BaseTy; and this function should be updated whenever this happens
-- | baseTypeLength returns the width of a type when represented in VHDL.
baseTypeLength :: BaseTy -> Int
baseTypeLength B  = 1
baseTypeLength CB = 1
baseTypeLength ClkTy = 1
baseTypeLength RstTy = 1
baseTypeLength (S x) = x
baseTypeLength (U x) = x
baseTypeLength T = 1

-- I don't use "_" here, because additional constructors are likely to be
-- defined for BaseTy; and this function should be updated whenever this happens
-- | 'baseTypeIsSigned' determines if a type has a signed representation. This is
-- necessary for the implementation of 'isSigned' in the 'Bits' type class.
baseTypeIsSigned :: BaseTy -> Bool
baseTypeIsSigned B     = False
baseTypeIsSigned CB    = False
baseTypeIsSigned ClkTy = False
baseTypeIsSigned RstTy = False
baseTypeIsSigned (S _) = True
baseTypeIsSigned (U _) = False
baseTypeIsSigned T     = False


instance Show BaseTy where
	show B 		= "B"
	show CB 	= "C"
        show ClkTy      = "CLK"
        show RstTy      = "RST"
	show (S i) 	= show i ++ "S"
	show (U i) 	= show i ++ "U"
	show T		= "T"


