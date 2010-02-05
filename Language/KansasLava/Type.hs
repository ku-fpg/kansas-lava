{-# LANGUAGE RankNTypes #-}
module Language.KansasLava.Type where

-- | BaseTy captures HDL-representable types.
data BaseTy
	= B		-- | Bit
	| CB		-- | Control Bit (currently unused)
        | ClkTy         -- | Clock Signal
        | RstTy         -- | Reset Signal
	| S Int		-- | Signed vector, with a width
	| U Int  	-- | Unsigned vector, with a width

	| T		-- Time
			-- What about Float/Double, special, etc.
	| TupleTy [BaseTy]
	| MatrixTy Int BaseTy
	| IntegerTy	-- holds whatever, not realizable in hw
	deriving (Eq, Ord)


-- | baseTypeLength returns the width of a type when represented in VHDL.
baseTypeLength :: BaseTy -> Int
baseTypeLength B  = 1
baseTypeLength CB = 1
baseTypeLength ClkTy = 1
baseTypeLength RstTy = 1
baseTypeLength (S x) = x
baseTypeLength (U x) = x
baseTypeLength T = 1
baseTypeLength (TupleTy tys) = sum (map baseTypeLength tys)

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
	show (TupleTy tys) = show tys


