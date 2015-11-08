{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | This module contains a number of utility functions useful for converting
-- Lava circuits to the Netlist AST.
module Language.KansasLava.Netlist.Utils
  (
   ToTypedExpr(..),
   ToStdLogicExpr(..), toStdLogicTy,
   AddNext(..),
   toIntegerExpr,
   sizedRange, sigName,
   isHigh,
   lookupInput, lookupInputType,
   -- Needed for Inst
   isMatrixStdLogicTy, isStdLogicTy, isStdLogicVectorTy,
   sanitizeName,
   active_high, stdLogicToMem, memToStdLogic,
   addNum, prodSlices, toMemIndex,
   mkExprConcat,
   assignStmt, assignDecl
  ) where

import Language.KansasLava.Types
import Language.Netlist.AST hiding (U)
import Language.Netlist.Util
import Language.KansasLava.Rep

import Data.Reify.Graph (Unique)
import Data.List(find,mapAccumL)

-- There are three type "classes" in our generated VHDL.
--  1. std_logic_vector
--  2. signed/unsigned
--  3. integer, as used to index into arrays, etc.


-- Turn a signal of std_logic[_vector], and
-- turn it into a Typed logic Expr. (signed, unsigned, or as is)
-- based on the given type.

-- | Convert a Lava value of a given type into a Netlist expression.
class ToTypedExpr v where
  -- | Given a type and a value, convert it to a netlist Expr.
  toTypedExpr :: Type -> v -> Expr

instance (Integral a, Show a) => ToTypedExpr (Driver a) where
	-- From a std_logic* into a typed Expr
	toTypedExpr ty (Lit n)           = toTypedExpr ty n
	toTypedExpr ty (Generic n)       = toTypedExpr ty n
	toTypedExpr ty (Port v n)        = toTypedExpr ty (sigName v (fromIntegral n))
	toTypedExpr ty (Pad nm) = toTypedExpr ty nm
        toTypedExpr _ other = error $ "toTypedExpr(Driver a): " ++ show other

instance ToTypedExpr String where
	-- From a std_logic* into a typed Expr
	toTypedExpr B      nm = 		       ExprVar nm
	toTypedExpr ClkTy  nm = 		       ExprVar nm
	toTypedExpr (V _)  nm = 	   	       ExprVar nm
	toTypedExpr (S _)  nm = 	signed $       ExprVar nm
	toTypedExpr (U _)  nm = 	unsigned $     ExprVar nm
	toTypedExpr (TupleTy _) nm =                   ExprVar nm
	toTypedExpr (MatrixTy _ _) nm =		       ExprVar nm
	toTypedExpr (SampledTy {}) nm =		       ExprVar nm
	toTypedExpr _other nm = error $ show ("toTypedExpr",_other,nm)

instance ToTypedExpr Integer where
	-- From a literal into a typed Expr
	toTypedExpr = fromIntegerToExpr

-- | Convert an integer represented by a given Lava type into a netlist Expr.
fromIntegerToExpr :: Type -> Integer -> Expr
fromIntegerToExpr t i =
	case toStdLogicTy t of
	     B   -> ExprLit Nothing (ExprBit (b (fromInteger i)))
	     V n -> ExprLit (Just n) (ExprNum i)
	     GenericTy   -> ExprLit Nothing  (ExprNum i)
	     ty -> error $ unwords ["fromIntegerToExpr: was expecting B or V from normalized number:", show ty, show i]
  where b :: Int -> Bit
        b 0 = F
        b 1 = T
        b _ = error "fromIntegerExpr: bit not of a value 0 or 1"

instance ToTypedExpr RepValue where
	-- From a literal into a typed Expr
	-- NOTE: We use Integer here as a natural, and assume overflow
	toTypedExpr (S n) r = ExprFunCall "to_signed"
	                        [ ExprLit Nothing $ ExprNum $ fromRepToInteger r
	                        , ExprLit Nothing $ ExprNum $ fromIntegral n
	                        ]
	toTypedExpr (U n) r = ExprFunCall "to_unsigned"
	                        [ ExprLit Nothing $ ExprNum $ fromRepToInteger r
	                        , ExprLit Nothing $ ExprNum $ fromIntegral n
	                        ]
        -- suspect generic call here
	toTypedExpr t r = toTypedExpr t (fromRepToInteger r)

-- | Type-directed converstion between Lava values and Netlist expressions.
class ToStdLogicExpr v where
	-- | Turn a value into a std_logic[_vector] Expr, given the appropriate type.
	toStdLogicExpr :: Type -> v -> Expr
	toStdLogicExpr' :: Type -> v -> Expr
	toStdLogicExpr' = toStdLogicExpr

	-- | Turn a value into an access of a specific element of an array.
	-- The Type is type of the element.
	toStdLogicEleExpr :: Int -> Type -> v -> Expr
	toStdLogicEleExpr = error "toStdLogicEleExpr"

instance (Integral a, Show a) => ToStdLogicExpr (Driver a) where
	-- From a std_logic* (because you are a driver) into a std_logic.
        toStdLogicExpr ty _
          | typeWidth ty == 0        = ExprVar "\"\""
	toStdLogicExpr ty (Lit n)          = toStdLogicExpr ty n
	toStdLogicExpr ty (Generic n)      = toStdLogicExpr ty n
	toStdLogicExpr (MatrixTy w ty') (Port v n)
					   = mkExprConcat
			[ (ty', memToStdLogic ty' $
			      ExprIndex (sigName v (fromIntegral n))
			                (ExprLit Nothing $ ExprNum $ fromIntegral (i)))
			| i <- reverse [0..(w-1)]
			]

	toStdLogicExpr _ (Port v n)        = ExprVar (sigName v (fromIntegral n))
	toStdLogicExpr _ (Pad v) = ExprVar v
	toStdLogicExpr _ other		   = error $ show other

	toStdLogicExpr' (MatrixTy 1 _) (Port v n) =
		memToStdLogic B $
			      ExprIndex (sigName v (fromIntegral n))
			                (ExprLit Nothing $ ExprNum $ 0)
	toStdLogicExpr' _ _ = error "missing pattern in toStdLogicExpr'"

	toStdLogicEleExpr i ty (Port v n) =
		memToStdLogic ty $
			      ExprIndex (sigName v (fromIntegral n))
			                (ExprLit Nothing $ ExprNum $ fromIntegral i)
	toStdLogicEleExpr _ _ _ = error "missing pattern in toStdLogicEleExpr"

instance ToStdLogicExpr Integer where
	-- From a literal into a StdLogic Expr
	toStdLogicExpr = fromIntegerToExpr

instance ToStdLogicExpr RepValue where
	toStdLogicExpr t r = toTypedExpr t (fromRepToInteger r)

instance ToStdLogicExpr Expr where
	-- Convert from a typed expression (as noted by the type) back into a std_logic*
	toStdLogicExpr B      e =      		     e
	toStdLogicExpr ClkTy  e = 		     e
	toStdLogicExpr (V _)  e = 	   	     e
	toStdLogicExpr (TupleTy _) e = 		     e
	toStdLogicExpr (MatrixTy _n _) (ExprVar _nm) = error "BBB"
{-
		ExprConcat
		[ ExprIndex nm
		           (ExprLit Nothing $ ExprNum $ fromIntegral i)
		| i <- [0..(n-1)]
		]
-}
	toStdLogicExpr (S _)  e = std_logic_vector  e
	toStdLogicExpr (U _)  e = std_logic_vector  e
	toStdLogicExpr(SampledTy {}) e =	     e
	toStdLogicExpr _other e = error $ show ("toStdLogicExpr", _other,e)

-- | Convert an integer to a netlist expression, not represented as a Netlist
-- std_logic_vector, though.
class ToIntegerExpr v where
  -- | Given  a type and a signal, generate the appropriate Netlist Expr.
  toIntegerExpr :: Type -> v -> Expr

instance (Integral i, Show i) => ToIntegerExpr (Driver i) where
        -- can assume a small (shift-by) number
  toIntegerExpr _ (Lit v)      = ExprLit Nothing $ ExprNum (fromRepToInteger v)
  toIntegerExpr GenericTy other = toTypedExpr GenericTy other -- HACK
  toIntegerExpr ty other        = to_integer (toTypedExpr ty other)

-- TOOD: remove, and replace with toStdLogicType.
-- | Turn a Kansas Lava type into its std_logic[_vector] type (in KL format)
-- There are three possible results (V n, B, MatrixTy n (V m))
-- This function does not have an inverse.
toStdLogicTy :: Type -> Type
toStdLogicTy B               = B
toStdLogicTy ClkTy           = B
toStdLogicTy (V n)           = V n
toStdLogicTy GenericTy       = GenericTy
toStdLogicTy (MatrixTy i ty) = MatrixTy i (V $ fromIntegral size)
  where size = typeWidth ty
toStdLogicTy ty              = V $ fromIntegral size
  where size = typeWidth ty


-- | Does this type have a *matrix* representation?
isMatrixStdLogicTy :: Type -> Bool
isMatrixStdLogicTy ty = case toStdLogicType ty of
                         SLVA {} -> True
                         _ -> False


isStdLogicTy :: Type -> Bool
isStdLogicTy ty = case toStdLogicType ty of
                         SL {} -> True
                         _ -> False

isStdLogicVectorTy :: Type -> Bool
isStdLogicVectorTy ty = case toStdLogicType ty of
                         SLV {} -> True
                         _ -> False

-- | Create a name for a signal.
sigName :: String -> Unique -> String
sigName v d = "sig_" ++  show d ++ "_" ++ v

-- | Given a Lava type, calculate the Netlist Range corresponding to the size.
sizedRange :: Type -> Maybe Range
sizedRange ty = case toStdLogicTy ty of
		  B -> Nothing
		  V n -> Just $ Range high low
                    where high = ExprLit Nothing (ExprNum (fromIntegral n - 1))
                          low = ExprLit Nothing (ExprNum 0)
                  MatrixTy _ _ -> error "sizedRange: does not support matrix types"
                  sty -> error $ "sizedRange: does not support type " ++ show sty

-- * VHDL macros

-- | The netlist representation of the active_high function.
active_high :: Expr -> Expr
active_high d      = ExprCond d  (ExprLit Nothing (ExprBit T)) (ExprLit Nothing (ExprBit F))

-- | The netlist representation of the VHDL std_logic_vector coercion.
std_logic_vector :: Expr -> Expr
std_logic_vector d = ExprFunCall "std_logic_vector" [d]

-- | The netlist representation of the VHDL unsigned coercion.
unsigned :: Expr -> Expr
unsigned x         = ExprFunCall "unsigned" [x]

-- | The netlist representation of the VHDL signed coercion.
signed :: Expr -> Expr
signed x           = ExprFunCall "signed" [x]

-- | The netlist representation of the VHDL to_integer coercion.
to_integer :: Expr -> Expr
to_integer e       = ExprFunCall "to_integer" [e]

-- | The netlist representation of the isHigh predicate.
isHigh :: Expr -> Expr
isHigh (ExprLit Nothing (ExprBit T)) = ExprVar "true"
isHigh d = ExprBinary Equals d (ExprLit Nothing (ExprBit T))

-- | Convert a driver to an Expr to be used as a memory address.
toMemIndex :: (Integral t, Show t) => Type -> Driver t -> Expr
toMemIndex ty _ | typeWidth ty == 0 = ExprLit Nothing (ExprNum 0)
toMemIndex _ (Lit n) = ExprLit Nothing $ ExprNum $ fromRepToInteger n
toMemIndex ty dr = to_integer $ unsigned $ toStdLogicExpr ty dr

-- Both of these are hacks for memories, that do not use arrays of Bools.
-- | Convert a 'memory' to a std_logic_vector.
memToStdLogic :: Type -> Expr -> Expr
memToStdLogic B e = ExprFunCall "lava_to_std_logic" [e]
memToStdLogic _ e = e

-- | Convert a std_logic_vector to a memory.
stdLogicToMem :: Type -> Expr -> Expr
stdLogicToMem B e = ExprConcat [ExprLit Nothing $ ExprBitVector [],e]
stdLogicToMem _ e = e

-- mkExprConcat always returns a std_logic_vector
-- If there is only one thing, and it is B, then we
-- coerce into a std_logic_vector
mkExprConcat :: [(Type,Expr)] -> Expr
mkExprConcat [(B,e)] = ExprConcat [ExprVar "\"\"",e]
mkExprConcat xs = ExprConcat $ map snd xs

---------------------------------------------------------------------------------------------------
-- Other utils

-- The Type here goes from left to right, but we use it right to left.
-- So [B,U4] => <XXXX:4 to 1><X:0>, because of the convension ordering in our generated VHDL.
-- Notice the result list is in the same order as the argument list.
-- The result is written out as a std_logic[_vector].
-- We assume that the input is *not* a constant (would cause lava-compile-time crash)

-- | Given a value and a list of types, corresponding to tuple element types,
-- generate a list of expressions corresponding to the indexing operations for
-- each tuple element.
prodSlices :: Driver Unique -> [Type] -> [Expr]
prodSlices d tys = reverse $ snd $ mapAccumL f size $ reverse tys
  where size = fromIntegral $ sum (map typeWidth tys) - 1

	nm = case d of
		Port v n -> sigName v n
		Pad v -> v
		Lit {} -> error "projecting into a literal (not implemented yet!)"
                driver -> error "projecting into " ++ show driver ++ " not implemented"

	f :: Integer -> Type -> (Integer,Expr)
        f i B = (i-1,ExprIndex nm (ExprLit Nothing (ExprNum i)))
        f i ty = let w = fromIntegral $ typeWidth ty
                     nextIdx = i - w
                 in (nextIdx, ExprSlice nm (ExprLit Nothing (ExprNum i))
                                        (ExprLit Nothing (ExprNum (nextIdx + 1))))

-- | Find some specific (named) input inside the entity.
lookupInput :: (Show b) => String -> Entity b -> Driver b
lookupInput i (Entity _ _ inps) = case find (\(v,_,_) -> v == i) inps of
                                      Just (_,_,d) -> d
                                      Nothing -> error $ "lookupInput: Can't find input" ++ show (i,inps)

-- | Find some specific (named) input's type inside the entity.
lookupInputType :: String -> Entity t -> Type
lookupInputType i (Entity _ _ inps) = case find (\(v,_,_) -> v == i) inps of
                                          Just (_,ty,_) -> ty
                                          Nothing -> error "lookupInputType: Can't find input"


-- | Add an integer generic (always named "i0" to an input list.
addNum :: Integer -> [(String,Type,Driver Unique)] -> [(String,Type,Driver Unique)]
addNum i [("i0",ty,d)] = [("i0",GenericTy,Generic i),("i1",ty,d)]
addNum _ _ = error "addNum"
-- TODO: should ty be GenericTy only here?

------------------------------------------------------------------------

-- | The 'AddNext' class is used to uniformly generate the names of 'next' signals.
class AddNext s where
  -- | Given a signal, return the name of the "next" signal.
  next :: s -> s

instance AddNext String where
   next nm = nm ++ "_next"

instance AddNext (Driver i) where
   next (Port v i) = Port (next v) i
   next other = other

------------------------------------------------------------------------------

-- | Convert a string representing a Lava operation to a VHDL-friendly name.
sanitizeName :: String -> String
sanitizeName "+"         = "add"
sanitizeName "-"         = "sub"
sanitizeName "*"         = "mul"
sanitizeName ".>."       = "gt"
sanitizeName ".<."       = "lt"
sanitizeName ".<=."      = "ge"
sanitizeName ".>=."      = "le"
-- TODO: Add check for symbols
sanitizeName other       = other


{-
-- Use the log of the resolution + 1 bit for sign
log2 1 = 0
log2 num
   | num > 1 = 1 + log2 (num `div` 2)
   | otherwise = error $ "Can't take the log of negative number " ++ show num
-}

----------------------------------------------

-- Build an assignment statement.
assignStmt :: String -> Unique -> Type -> Driver Unique -> Stmt
assignStmt nm i ty d =
   case toStdLogicType ty of
      SL  ->    Assign (ExprVar $ sigName nm i) (toStdLogicExpr ty d)
      SLV {} -> Assign (ExprVar $ sigName nm i) (toStdLogicExpr ty d)
      SLVA n m -> statements $
		[ Assign (ExprIndex (sigName nm i)
				    (ExprLit Nothing $ ExprNum $ fromIntegral j))
			$ toStdLogicEleExpr j (V m) d
		| j <- [0..(n-1)]
		]
      G {} -> error "assignStmt {G} ?"

-- | 'assignDecl' takes a name and unique, a target type, and
-- a function that takes a driver-to-expr function, and returns an expr.
assignDecl :: String -> Unique -> Type -> ((Driver Unique -> Expr) -> Expr) -> [Decl]
assignDecl nm i ty f =
   case toStdLogicType ty of
      SL  ->   [ NetAssign (sigName nm i)
      	       	 	   (f $ toStdLogicExpr ty)
               ]
      SLV {} -> [ NetAssign (sigName nm i)
      	     	  	    (f $ toStdLogicExpr ty)
                ]
      SLVA n m -> [  MemAssign (sigName "o0" i)
      	      	    	       (ExprLit Nothing $ ExprNum $ fromIntegral j)
      	     	  	       (f $ toStdLogicEleExpr j (V m))
	         | j <- [0..(n-1)]
                ]
      G {} -> error "assignDecl {G} ?"

--error "assignStmt of Matrix"
