{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies, ParallelListComp, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses  #-}

module Language.KansasLava.Types where


import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative
import Data.Monoid

import Data.Reify
import qualified Data.Traversable as T
import Control.Applicative
import Data.Unique as U
import Data.Dynamic

-- Key internal type

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

-- This is required for the deserialization of Trace objects.
instance Read Type where
    readsPrec _ "B" = [(B,"")]
    readsPrec _ "CLK" = [(ClkTy,"")]
    readsPrec _ "G" = [(GenericTy,"")]
    readsPrec _ str | (last str) `elem` ['U', 'S', 'V'] = [(con int,"")]
        where con = case last str of
                        'U' -> U
                        'S' -> S
                        'V' -> V
              int = read (init str) :: Int
    readsPrec _ _   = error "read BaseTy: no parse"


-------------------------------------------------------------------------
-- OVar

data OVar = OVar Int String		-- The # is used purely for sorting order.
	deriving (Eq, Ord, Read)

instance Show OVar where
	show (OVar i nm) = nm ++ "$" ++ show i

-------------------------------------------------------------------------
-- Id

data Id = Name String String			-- external thing
	| Prim String				-- built in thing
	| Tracer String [[WireVal Bool]]	-- tracer (probes, etc)
						-- This is type of identity
						-- that records its shallow value,
						-- for later inspection
	| Function [(Integer,Integer)] 	-- anonymous function
    deriving (Eq, Ord)

instance Show Id where
    show (Name "" nm)  = nm	-- do we use "" or "Lava" for the magic built-in?
    show (Name pre nm) = pre ++ "::" ++ nm
    show (Prim nm)     = nm
    show (Tracer str _) = "^" ++ str
--    show (UniqNm n)    = "#" ++ show (hashUnique n) -- might not be uniq
    show (Function _)  = "<fn>"

-------------------------------------------------------------------------
-- Entity

-- An Entity is our central "BOX" of compuation.


-- We tie the knot at the 'Entity' level, for observable sharing.
-- TODO: remove Table, remove annotations
data Entity ty a s = Entity Id [(String,ty)] [(String,ty,Driver s)] [a]
			-- specialized Entity, because tables (typically ROMs) are verbose.
		   | Table (String,ty) (String,ty,Driver s) [(Integer,String,Integer,String)]
              deriving (Show, Eq, Ord)

instance T.Traversable (Entity ty a) where
  traverse f (Entity v vs ss dyn) =
    Entity v vs <$> (T.traverse (\ (val,ty,a) -> ((,,) val ty) `fmap` T.traverse f a) ss) <*> pure dyn
  traverse f (Table (v0,t0) (v1,t1,d) tbl) =
	(\ d' -> Table (v0,t0) (v1,t1,d') tbl) <$> T.traverse f d

instance F.Foldable (Entity ty a) where
  foldMap f (Entity _ _ ss _) = mconcat [ F.foldMap f d | (_,_,d) <- ss ]

instance Functor (Entity ty a) where
    fmap f (Entity v vs ss dyn) = Entity v vs (fmap (\ (var,ty,a) -> (var,ty,fmap f a)) ss) dyn


-------------------------------------------------------------------------
-- Driver

-- A Driver is a specific driven 'wire' (signal in VHDL),
-- which types contains a value that changes over time.

data Driver s = Port String s      -- a specific port on the entity
              | Pad OVar       	  --
              | Lit Integer
	      | Error String	-- A call to err, in Datatype format for reification purposes
              deriving (Eq, Ord)

instance Show i => Show (Driver i) where
  show (Port v i) = "(" ++ show i ++ ")." ++ show v 
  show (Lit x) = show x
  show (Pad v) = show v 
  show (Error msg) = show msg

instance T.Traversable Driver where
  traverse f (Port v s)    = Port v <$> f s
  traverse _ (Pad v)       = pure $ Pad v
--  traverse _ (PathPad v)   = pure $ PathPad v
  traverse _ (Lit i)       = pure $ Lit i
  traverse _ (Error s)     = pure $ Error s

instance F.Foldable Driver where
  foldMap f (Port _ s)    = f s
  foldMap _ (Pad _)       = mempty
--  foldMap _ (PathPad _)   = mempty
  foldMap _ (Lit _)       = mempty
  foldMap _ (Error s)     = mempty

instance Functor Driver where
    fmap f (Port v s)    = Port v (f s)
    fmap _ (Pad v)       = Pad v
--    fmap _ (PathPad v)   = PathPad v
    fmap _ (Lit i)       = Lit i
    fmap _ (Error s)     = Error s

---------------------------------------------------------------------------------------------------------

-- Should be called SignalVal? Par Cable? hmm.
data WireVal a = WireUnknown | WireVal a
    deriving (Eq,Ord) -- Useful for comparing [X a] lists in Trace.hs

instance Monad WireVal where
	return = WireVal
	fail _ = WireUnknown
	WireUnknown >>= _ = WireUnknown
	WireVal a >>= f = f a

instance Functor WireVal where
	fmap f WireUnknown = WireUnknown
	fmap f (WireVal a) = WireVal (f a)

instance Applicative WireVal where
	pure = WireVal
	WireVal f <*> WireVal a = WireVal $ f a
	_ <*> _ = WireUnknown

instance Show a => Show (WireVal a) where
	show WireUnknown = "?"
	show (WireVal a) = show a

---------------------------------------------------------------------------------------------------------
-- 

data TraceStream = TraceStream Type [[WireVal Bool]] -- to recover type, eventually clock too?
                 | Empty
    deriving (Eq, Show)


---------------------------------------------------------------------------------------------------------
-- 

data Annotation = ProbeValue OVar TraceStream
                | Ann String Dynamic
		| Comment String		-- intended to arrive in the VHDL

instance Eq Annotation where {}
instance Ord Annotation where {}
instance Show Annotation where
    show (ProbeValue name _) = show name
    show _                   = error "Show: Unknown Annotation"

---------------------------------------------------------------------------------------------------------

newtype E = E (MuE E)

-- What should this be called. Pre-MuE?
type MuE u = Entity Type Annotation u

-- You want to observe
instance MuRef E where
  type DeRef E = Entity Type Annotation
  mapDeRef f (E s) = T.traverse f s

instance Show E where
    show (E s) = show s

-- Consider this carefully
instance Eq E where
   (E s1) == (E s2) = s1 == s2

---------------------------------------------------------------------------------------------------------

-- A pin to an E/Entity
newtype D a = D (Driver E)
	deriving Show

-- TODO: is this used?
unD :: D a -> Driver E
unD (D a) = a

