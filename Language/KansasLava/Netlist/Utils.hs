{-# LANGUAGE TypeSynonymInstances #-}
module Language.KansasLava.Netlist.Utils where

import Language.KansasLava.Type
import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL
import Language.KansasLava.Entity

import qualified Data.Map as Map

import Data.Reify.Graph (Unique)

import Data.List(intersperse,find,mapAccumL,nub)

-- Turn a signal of std_logic[_vector], and
-- turn it into a Typed logic Expr. (signed, unsigned, or as is)
-- based on the given type.

class ToTypedExpr v where
	-- Turning a 'v' of (typically a std_logic_[vector] signal var) to a *typed* Expr,
	-- that is we use signed and unsigned for our S and U.
	toTypedExpr :: BaseTy -> v -> Expr

instance (Integral a) => ToTypedExpr (Driver a) where
	-- From a std_logic* into a typed Expr
	toTypedExpr ty (Lit n)          = toTypedExpr ty n
	toTypedExpr ty (Port (Var v) n) = toTypedExpr ty (sigName v (fromIntegral n))
	toTypedExpr ty (Pad (Var nm))   = toTypedExpr ty nm

instance ToTypedExpr String where
	-- From a std_logic* into a typed Expr
	toTypedExpr B      nm = 		       ExprVar $ nm -- ExprBinary Equals (ExprVar nm) (ExprBit 1)
	toTypedExpr ClkTy  nm = 		       ExprVar $ nm
	toTypedExpr RstTy  nm = 	 	       ExprVar $ nm
	toTypedExpr (V _)  nm = 	   	       ExprVar $ nm
	toTypedExpr (S _)  nm = signed $ 	       ExprVar $ nm
	toTypedExpr (U _)  nm = unsigned $ 	       ExprVar $ nm
	toTypedExpr (TupleTy _) nm =		       ExprVar $ nm
	toTypedExpr (MatrixTy _ _) nm =		       ExprVar $ nm
	toTypedExpr _other nm = error $ (show ("toTypedExpr",_other,nm))

instance ToTypedExpr Integer where
	-- From a literal into a typed Expr
	toTypedExpr B     i = ExprBit (fromInteger i)
	toTypedExpr (V n) i = ExprLit n i
	-- A bit of a hack, perhapse use case
	toTypedExpr ty    i = toTypedExpr (toStdLogicTy ty) i

class ToStdLogicExpr v where
	-- Turn a 'v' into a std_logic[_vector] Expr.
	toStdLogicExpr :: BaseTy -> v -> Expr

instance (Integral a) => ToStdLogicExpr (Driver a) where
	-- From a std_logic* (because you are a driver) into a std_logic.
	toStdLogicExpr ty (Lit n)          = toStdLogicExpr ty n
	toStdLogicExpr ty (Port (Var v) n) = ExprVar $ sigName v (fromIntegral n)
	toStdLogicExpr ty (Pad (Var v))	   = ExprVar $ v

instance ToStdLogicExpr Integer where
	-- From a literal into a StdLogic Expr
	toStdLogicExpr B     i = ExprBit (fromInteger i)
	toStdLogicExpr (V n) i = ExprLit n i
	-- A bit of a hack, perhapse use case
	toStdLogicExpr ty    i = toStdLogicExpr (toStdLogicTy ty) i

instance ToStdLogicExpr Expr where
	-- Convert from a typed expression (as noted by the type) back into a std_logic*
	--
	toStdLogicExpr B      e =      		     e
	toStdLogicExpr ClkTy  e = 		     e
	toStdLogicExpr RstTy  e = 	 	     e
	toStdLogicExpr (V _)  e = 	   	     e
	toStdLogicExpr (TupleTy _) e = 		     e
	toStdLogicExpr (MatrixTy _ _) e =	     e
	toStdLogicExpr (S _)  e = std_logic_vector $ e
	toStdLogicExpr (U _)  e = std_logic_vector $ e
	toStdLogicExpr _other e = error $ show ("toStdLogicExpr", _other,e)

class ToIntegerExpr v where
	toIntegerExpr :: BaseTy -> v -> Expr
	
instance (Integral i) => ToIntegerExpr (Driver i) where
	toIntegerExpr ty (Lit v) = ExprNum v
	toIntegerExpr ty other   = to_integer (toTypedExpr ty other)

-- NEVER USED 
{-
toStdLogicVectorExpr :: (Integral a) => BaseTy -> Driver a -> Expr
toStdLogicVectorExpr ty dr =
	case toStdLogicTy ty of
	   B   -> singleton $ toStdLogicExpr B dr
	   V n -> toStdLogicExpr (V n) dr
-}

-- Turn a Kansas Lava type into its std_logic[_vector] type (in KL format)
-- Note this function does not have an inverse.
toStdLogicTy :: BaseTy -> BaseTy
toStdLogicTy B     = B
toStdLogicTy ClkTy = B
toStdLogicTy RstTy = B
toStdLogicTy ty    = V (fromIntegral size)
  where size = baseTypeLength ty
-- 

-- Name a signal
-- TODO: consider Var -> Unique -> String
sigName :: String -> Unique -> String
sigName v d = "sig_" ++  show d ++ "_" ++ v

-- figure out the size of a type.
sizedRange :: BaseTy -> Maybe Range
sizedRange ty = case toStdLogicTy ty of
		  B -> Nothing
		  V n -> Just $ Range (ExprNum (fromIntegral n - 1)) (ExprNum 0)

-- like sizedRange, but allowing 2^n elements (for building memories)
memRange :: BaseTy -> Maybe Range
memRange ty = case toStdLogicTy ty of
		  B -> Nothing
		  V n -> Just $ Range (ExprNum $ 2^(fromIntegral n - 1)) (ExprNum 0)

-- VHDL "macros"
active_high d      = ExprFunCall "active_high" [d]
std_logic_vector d = ExprFunCall "std_logic_vector" [d]
to_unsigned x w    = ExprFunCall "to_unsigned" [x, w]		-- is this used now?
unsigned x         = ExprFunCall "unsigned" [x]
to_signed x w      = ExprFunCall "to_signed" [x, w]		-- is this used now?
signed x           = ExprFunCall "signed" [x]
to_integer e       = ExprFunCall "to_integer" [e]

--singleton x 	   = ExprFunCall "singleton" [x]
-- Others

isHigh d = (ExprBinary Equals d (ExprBit 1))
isLow d = (ExprBinary Equals d (ExprBit 0))
allLow ty = ExprLit (baseTypeLength ty) 0
zeros = ExprString "(others => '0')"

---------------------------------------------------------------------------------------------------
-- Other utils

-- The BaseTy here goes from left to right, but we use it right to left.
-- So [B,U4] => <XXXX:4 to 1><X:0>, because of the convension ordering in our generated VHDL.
-- Notice the result list is in the same order as the argument list.
-- The result is written out as a std_logic[_vector].
-- We assume that the input is *not* a constant (would cause lava-compile-time crash)

prodSlices :: Driver Unique -> [BaseTy] -> [Expr]
prodSlices d tys = reverse $ snd $ mapAccumL f size $ reverse tys
  where size = fromIntegral $ sum (map baseTypeLength tys) - 1

	nm = case d of
		Port (Var v) n -> sigName v n
		Pad (Var v) -> v
		Lit {} -> error "projecting into a literal (not implemented yet!)"
		
	f :: Integer -> BaseTy -> (Integer,Expr)
        f i B = (i-1,ExprIndex nm (ExprNum i))
        f i ty = let w = fromIntegral $ baseTypeLength ty
                     next = i - w
                 in (next, ExprSlice nm (ExprNum i) (ExprNum (next + 1)))

-- Find some specific (named) input inside the entity.
lookupInput :: (Show a, Show b) => String -> Entity a b -> Driver b
lookupInput i (Entity _ _ inps _) = case find (\(Var v,_,_) -> v == i) inps of
                                      Just (_,_,d) -> d
                                      Nothing -> error $ "lookupInput: Can't find input" ++ show (i,inps)

-- Find some specific (named) input's type inside the entity.
lookupInputType i (Entity _ _ inps _) = case find (\(Var v,_,_) -> v == i) inps of
                                          Just (_,ty,_) -> ty
                                          Nothing -> error "lookupInputType: Can't find input"

addNum :: Integer -> [(Var,BaseTy,Driver Unique)] -> [(Var,BaseTy,Driver Unique)]
addNum i [(Var "i0",ty,d)] = [(Var "i0",IntegerTy,Lit i),(Var "i1",ty,d)]
addNum _ _ = error "addNum"

------------------------------------------------------------------------


data NetlistOption 
		= LoadEnable 			-- add an enable signal to the entity, and each gate.
		| AsynchResets 
--		| ReifyOptions [ReifyOption]
		deriving (Eq, Show, Ord)
type NetlistOptions = [NetlistOption]

addEnabled opts = LoadEnable `elem` opts
asynchResets opts = AsynchResets `elem` opts

------------------------------------------------------------------------------

class AddNext s where
   next :: s -> s

instance AddNext String where 
   next nm = nm ++ "_next"

instance AddNext Var where 
   next (Var nm) = Var $ next nm


instance AddNext (Driver i) where 	
   next (Port v i) = Port (next v) i
   next other = other 

------------------------------------------------------------------------------
-- Turn a name into something VHDL could use

cleanupName :: String -> String
cleanupName "+" = "addition"
cleanupName "-" = "subtraction"
cleanupName "*" = "multiplication"
cleanupName other = other

------------------------------------------------------------------------------------

-- Grab all of the synchronous elements (listed in 'nms') and return a map keyed
-- on clk input, with the value including a list of associated entities.
-- TODO: What is going on here!!

--getSynchs :: [String] -> [(Unique,Entity BaseTy Unique)] -> Map.Map ( [a]
getSynchs nms ents = Map.fromListWith (++) synchs
  where
        synchs = [((getInput "clk" is,getInput "rst" is),[e])  
		 | e@(i,Entity (Name "Memory" n) _ is _) <- ents, 
		    n `elem` nms]
        getInput nm is = case find (\(Var c,_,_) -> c == nm) is of
                      Just (Var _,_,d) -> d
                      Nothing -> error $ "getSynchs: Can't find a signal " ++ show (nm,is,ents)

