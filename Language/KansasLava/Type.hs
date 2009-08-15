{-# LANGUAGE RankNTypes #-}
module Language.KansasLava.Type where

import qualified Data.Set as Set
import Data.Set (Set)


data BaseTy 
	= B		-- Bit
	| S Int		-- Signed vector
	| U Int  	-- Unsigned vector
			-- What about Float/Double, special, etc.
	deriving (Eq, Ord)
	
data Ty v = BaseTy BaseTy | TyVar v
	deriving (Eq, Ord)


instance Functor Ty where
	fmap f (BaseTy ty)	= BaseTy ty
	fmap f (TyVar v) 	= TyVar (f v)

instance Show BaseTy where
	show B 		= "B"
	show (S i) 	= show i ++ "S"
	show (U i) 	= show i ++ "U"

instance Show v => Show (Ty v) where
	show (BaseTy ty) = show ty
	show (TyVar v) 	 = show v



{-
--
data BaseTy = BaseTy (forall a . Ty a)

baseToTy :: BaseTy -> Ty v
baseToTy (BaseTy ty) = fmap undefined ty
-}

-- we depend on the total number of final sets being small for efficiency here.

findMinEquivSets :: (Show a, Ord a) => [Set a] -> [Set a]
findMinEquivSets [] = [] 
findMinEquivSets [x] = [x] 
findMinEquivSets (x:xs)  = foldr insert [x] xs

insert x [] = [x]
insert x (y:ys) | Set.null (x `Set.intersection` y) = y : insert x ys
		| otherwise                         = insert (x `Set.union` y) ys

