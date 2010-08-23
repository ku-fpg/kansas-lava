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
	| V Int		-- | std_logic_vector

	| T		-- Time	TODO: remove
			-- What about Float/Double, special, etc.
	| TupleTy [BaseTy]
			-- | Tuple, represented as a larget std_logic_vector
	| MatrixTy Int BaseTy
			-- | Matrix, vhdl array.
	| IntegerTy	-- holds whatever, not realizable in hw
			-- used to pass static arguments to
	deriving (Eq, Ord)


-- | baseTypeLength returns the width of a type when represented in VHDL.
baseTypeLength :: BaseTy -> Int
baseTypeLength B  = 1
baseTypeLength CB = 1
baseTypeLength ClkTy = 1
baseTypeLength RstTy = 1
baseTypeLength (S x) = x
baseTypeLength (U x) = x
baseTypeLength (V x) = x
baseTypeLength T = 1
baseTypeLength (TupleTy tys) = sum (map baseTypeLength tys)
baseTypeLength (MatrixTy i ty) = i * baseTypeLength ty
baseTypeLength other = error $ show ("baseTypeLength",other)

-- | 'baseTypeIsSigned' determines if a type has a signed representation. This is
-- necessary for the implementation of 'isSigned' in the 'Bits' type class.
baseTypeIsSigned :: BaseTy -> Bool
baseTypeIsSigned B     = False
baseTypeIsSigned CB    = False
baseTypeIsSigned ClkTy = False
baseTypeIsSigned RstTy = False
baseTypeIsSigned (S _) = True
baseTypeIsSigned (U _) = False
baseTypeIsSigned (V _) = False
baseTypeIsSigned T     = False

instance Show BaseTy where
	show B 		= "B"
	show CB 	= "C"
        show ClkTy      = "CLK"
        show RstTy      = "RST"
	show (S i) 	= show i ++ "S"
	show (U i) 	= show i ++ "U"
	show (V i) 	= show i ++ "V"
	show T		= "T"
	show (TupleTy tys) = show tys
	show (MatrixTy i ty) = show i ++ "[" ++ show ty ++ "]"
	show IntegerTy	= "Integer"

instance Read BaseTy where
    readsPrec _ "B" = [(B,"")]
    readsPrec _ "C" = [(CB,"")]
    readsPrec _ "T" = [(T,"")]
    readsPrec _ "CLK" = [(ClkTy,"")]
    readsPrec _ "RST" = [(RstTy,"")]
    readsPrec _ "Integer" = [(IntegerTy,"")]
    readsPrec _ str | (last str) `elem` ['U', 'S', 'V'] = [(con int,"")]
        where con = case last str of
                        'U' -> U
                        'S' -> S
                        'V' -> V
              int = read (init str) :: Int
    readsPrec _ _   = error "read BaseTy: no parse"
