module Language.KansasLava.Radix 
	( Radix		-- abstract
	, empty
	, insert
	, lookup
	) where

import Prelude hiding (lookup)

-- | Simple Radix trees, customized for Lava internals, for example is strict.
-- It is used to represent function types and ROMS and RAMs.
-- There is a requirement that all keys (list of bits) be the same length.

data Radix a
  = Res !a
  | NoRes
  | Choose !(Radix a) !(Radix a)
	deriving Show

empty :: Radix a
empty = NoRes

insert :: [Bool] -> a -> Radix a -> Radix a

insert []    y (Res _) = Res $! y
insert []    y NoRes   = Res $! y
insert []    y (Choose _ _) = error "inserting with short key"

insert (x:a) y NoRes   = insert (x:a) y expanded
insert (x:a) y (Res _) = error "inserting with too long a key"
insert (x:a) y (Choose l r)
	| x == True 	  = Choose (insert a y l) r
	| x == False	  = Choose l (insert a y r)

-- Would this be lifted?
expanded = Choose NoRes NoRes

lookup :: [Bool] -> Radix a -> Maybe a

lookup [] (Res v) = Just v
lookup [] NoRes   = Nothing
lookup [] _       = error "lookup error with short key"

lookup (x:a) (Res _) = error "lookup error with long key"
lookup (x:a) NoRes   = Nothing
lookup (True:a) (Choose l r) = lookup a l
lookup (False:a) (Choose l r) = lookup a r



