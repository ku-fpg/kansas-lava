module Language.KansasLava.Radix where

-- Simple Radix trees, customized for Lava internals, for example is strict.
-- It is used to represent function types.
-- There is a requirement that all keys (list of bits) be the same length.

data Radix a
  = Res !a
  | NoRes
  | Choose !(Radix a) !(Radix a)
	deriving Show

emptyRadix :: Radix a
emptyRadix = NoRes

insertRadix :: [Bool] -> a -> Radix a -> Radix a

insertRadix []    y (Res _) = Res $! y
insertRadix []    y NoRes   = Res $! y
insertRadix []    y (Choose _ _) = error "inserting with short key"

insertRadix (x:a) y NoRes   = insertRadix (x:a) y expanded
insertRadix (x:a) y (Res _) = error "inserting with too long a key"
insertRadix (x:a) y (Choose l r)
	| x == True 	  = Choose (insertRadix a y l) r
	| x == False	  = Choose l (insertRadix a y r)

-- Would this be lifted?
expanded = Choose NoRes NoRes

lookupRadix :: [Bool] -> Radix a -> Maybe a

lookupRadix [] (Res v) = Just v
lookupRadix [] NoRes   = Nothing
lookupRadix [] _       = error "lookup error with short key"

lookupRadix (x:a) (Res _) = error "lookup error with long key"
lookupRadix (x:a) NoRes   = Nothing
lookupRadix (True:a) (Choose l r) = lookupRadix a l
lookupRadix (False:a) (Choose l r) = lookupRadix a r



