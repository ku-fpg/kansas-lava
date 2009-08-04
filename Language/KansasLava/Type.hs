module Language.KansasLava.Type where

import qualified Data.Set as Set
import Data.Set (Set)

data Ty v 
	= B		-- Bit
	| S Int		-- Signed vector
	| U Int  	-- Unsigned vector
			-- What about Float/Double, special, etc.

	| TyVar v
	deriving (Eq, Ord)

instance Functor Ty where
	fmap f B 		= B
	fmap f (S i) 		= S i
	fmap f (U i)		= U i
	fmap f (TyVar v) 	= TyVar (f v)

instance Show v => Show (Ty v) where
	show B 		= "B"
	show (S i) 	= show i ++ "S"
	show (U i) 	= show i ++ "U"
	show (TyVar v) 	= show v


-- we depend on the total number of final sets being small for efficiency here.

findMinEquivSets :: (Show a, Ord a) => [Set a] -> [Set a]
findMinEquivSets [] = [] 
findMinEquivSets [x] = [x] 
findMinEquivSets (x:xs)  = foldr insert [x] xs

insert x [] = [x]
insert x (y:ys) | Set.null (x `Set.intersection` y) = y : insert x ys
		| otherwise                         = insert (x `Set.union` y) ys

