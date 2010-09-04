{-# LANGUAGE RankNTypes #-}
module Language.KansasLava.Types where


-- | Type captures HDL-representable types.
data Type
	-- basic representations
	= B		-- | Bit
	| S Int		-- | Signed vector, with a width
	| U Int  	-- | Unsigned vector, with a width
	| V Int		-- | std_logic_vector

	-- type of bit, used only for clock (TODO: do we need this?)
        | ClkTy         -- | Clock Signal

	| GenericTy	-- | generics in VHDL, right now just Integer

	| TupleTy [Type]
			-- | Tuple, represented as a larget std_logic_vector
	| MatrixTy Int Type
			-- | Matrix, vhdl array.

	deriving (Eq, Ord)


-- | typeWidth returns the width of a type when represented in VHDL.
typeWidth :: Type -> Int
typeWidth B  = 1
typeWidth ClkTy = 1
typeWidth (S x) = x
typeWidth (U x) = x
typeWidth (V x) = x
typeWidth (TupleTy tys) = sum (map typeWidth tys)
typeWidth (MatrixTy i ty) = i * typeWidth ty
typeWidth other = error $ show ("typeWidth",other)

-- | 'isTypeSigned' determines if a type has a signed representation. This is
-- necessary for the implementation of 'isSigned' in the 'Bits' type class.
isTypeSigned :: Type -> Bool
isTypeSigned B     = False
isTypeSigned ClkTy = False
isTypeSigned (S _) = True
isTypeSigned (U _) = False
isTypeSigned (V _) = False

instance Show Type where
	show B 		= "B"
        show ClkTy      = "CLK"
	show (S i) 	= show i ++ "S"
	show (U i) 	= show i ++ "U"
	show (V i) 	= show i ++ "V"
	show GenericTy  = "G"
	show (TupleTy tys) = show tys
	show (MatrixTy i ty) = show i ++ "[" ++ show ty ++ "]"

