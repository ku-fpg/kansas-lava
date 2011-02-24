-- | Simple Radix trees, customized for Lava internals, for example is strict.
-- It is used to represent function types and ROMS and RAMs.
-- There is a requirement that all keys (list of bits) be the same length.
module Language.KansasLava.Radix
	( Radix		-- abstract
	, empty
	, insert
	, lookup
	) where

import Prelude hiding (lookup)


data Radix a
  = Res !a -- ^ A value stored in the tree
  | NoRes -- ^ Non-present value
  -- | A split-node, left corresponds to 'True' key bit, right corresponds to 'False' key bit.
  | Choose !(Radix a) !(Radix a)
	deriving Show

-- | The empty tree
empty :: Radix a
empty = NoRes

-- | Add a value (keyed by the list of bools) into a tree
insert :: [Bool] -> a -> Radix a -> Radix a
insert []    y (Res _) = Res $! y
insert []    y NoRes   = Res $! y
insert []    _ (Choose _ _) = error "inserting with short key"
insert xs     y NoRes   = insert xs y (Choose NoRes NoRes)
insert _  _ (Res _) = error "inserting with too long a key"
insert (True:a) y (Choose l r) = Choose (insert a y l) r
insert (False:a) y (Choose l r) = Choose l (insert a y r)


-- | Find a value in a radix tree
lookup :: [Bool] -> Radix a -> Maybe a

lookup [] (Res v) = Just v
lookup [] NoRes   = Nothing
lookup [] _       = error "lookup error with short key"

lookup (_:_) (Res _) = error "lookup error with long key"
lookup (_:_) NoRes   = Nothing
lookup (True:a) (Choose l _) = lookup a l
lookup (False:a) (Choose _ r) = lookup a r



