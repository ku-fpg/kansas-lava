{-# LANGUAGE TypeFamilies, Rank2Types, ScopedTypeVariables, GADTs #-}

-- | This module contains the key internal types for Kansas Lava,
-- and some basic utilties (like Show instances) for these types.

module Language.KansasLava.Types (
	-- * Types
	  Type(..)
	, typeWidth
	, isTypeSigned
	-- * OVar
	, OVar(..)
	-- * Id
	, Id(..)
	, Box(..)
	-- * Entity
	, Entity(..)
	, E(..)
	-- * Driver
	, Driver(..)
	, D(..)
	-- * Clock
	, Clock(..)	-- type class
	-- * WireVal
	, WireVal(..)
	-- * RepValue
	, RepValue(..)
	, appendRepValue
	, isValidRepValue
	, getValidRepValue
	, cmpRepValue
	-- * Tracing
	, TraceStream(..)
	, TraceMap
	, Trace(..)

	-- * Circuit
	, Circuit(..)
	, Signature(..)
	, circuitSignature
	-- *Witness
	, Witness(..)
--	, witness
	-- * TO REMOVE
	, MuE(..) -- TODO: remove
	, Annotation(..)
	) where

import Control.Applicative

import Data.Dynamic
import qualified Data.Foldable as F
import Data.List as L
import qualified Data.Map as M
import Data.Monoid
import Data.Reify
import qualified Data.Traversable as T

-------------------------------------------------------------------------
-- | Type captures HDL-representable types.
data Type
        -- basic representations
        = B             -- ^ Bit
        | S Int         -- ^ Signed vector, with a width.
        | U Int         -- ^ Unsigned vector, with a width.
        | V Int         -- ^ std_logic_vector, with a width.

        -- type of bit, used only for clock (TODO: do we need this?)
        | ClkTy         -- ^ Clock Signal

	| ClkDomTy	-- ^ The clock domain type, which has a clock, a clock enable,
			-- and an asyncronized reset.

        | GenericTy     -- ^ generics in VHDL, right now just Integer

        | TupleTy [Type]
                        -- ^ Tuple, represented as a larger std_logic_vector
        | MatrixTy Int Type
                        -- ^ Matrix, for example a vhdl array.

        | SampledTy Int Int
                        -- ^ Our "floating" values.
                        --   The first number is the precision/scale (+/- N)
                        --   The second number is the bits used to represent this number
        deriving (Eq, Ord)

-- | 'typeWidth' returns the width of a type when represented in VHDL.
typeWidth :: Type -> Int
typeWidth B  = 1
typeWidth ClkTy = 1
typeWidth ClkDomTy = 3
typeWidth (S x) = x
typeWidth (U x) = x
typeWidth (V x) = x
typeWidth (TupleTy tys) = sum (map typeWidth tys)
typeWidth (MatrixTy i ty) = i * typeWidth ty
typeWidth (SampledTy _ i) = i
typeWidth other = error $ show ("typeWidth",other)

-- | 'isTypeSigned' determines if a type has a signed representation. This is
-- necessary for the implementation of 'isSigned' in the 'Bits' type class.
isTypeSigned :: Type -> Bool
isTypeSigned B     = False
isTypeSigned ClkTy = False
isTypeSigned (S _) = True
isTypeSigned (U _) = False
isTypeSigned (V _) = False
isTypeSigned (SampledTy {}) = True

instance Show Type where
        show B          = "B"
        show ClkTy      = "Clk"
        show ClkDomTy   = "ClkDom"
        show (S i)      = show i ++ "S"
        show (U i)      = show i ++ "U"
        show (V i)      = show i ++ "V"
        show GenericTy  = "G"
        show (TupleTy tys) = show tys
        show (MatrixTy i ty) = show i ++ "[" ++ show ty ++ "]"
        show (SampledTy m n) = "Sampled " ++ show m ++ " " ++ show n

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
-- | 'OVar' is an order Var, with a number that is used for sorting purposes.
-- This is used for names of arguments in VHDL, and other places, so
-- that arguments can preserve order through transformation.

data OVar = OVar Int String             -- The # is used purely for sorting order.
        deriving (Eq, Ord)

instance Show OVar where
        show (OVar i nm) = nm ++ "$" ++ show i

instance Read OVar where
        readsPrec _ xs = case span (/= '$') xs of
                          (n,'$':r1) -> [ (OVar i n,r2)
                                        | (i,r2) <- reads r1
                                        ]
                          _         -> [] -- no parse

-------------------------------------------------------------------------
-- | Id is the name/tag of a block of compuation.

data Id = Name String String                    -- ^ external thing (TODO: remove)
        | Prim String                           -- ^ built in thing
	| External String			-- 
        | Function [(RepValue,RepValue)]        -- ^ anonymous function


						-- 
        | TraceVal [OVar] TraceStream           -- ^ trace (probes, etc)
                                                -- may have multiple names matching same data
                                                -- This is type of identity
                                                -- that records its shallow value,
                                                -- for later inspection
						-- (Why are the OVar's here?)

	| ClockId String			-- ^ An environment box

	| Label String				-- ^ An identity; also a name, used to tag function arguments
	| Comment' [String]			-- ^ An identity; also a multi-line comments
	| BlackBox (Box Dynamic)		-- ^ 'BlackBox' can be removed without harm
						-- The rule is you can only insert you own
						-- types in here (or use newtype).
						-- Prelude or other peoples types
						-- are not allowed (because typecase becomes ambigious)

    deriving (Eq, Ord)


instance Show Id where
    show (Name "" nm)  = nm     -- do we use "" or "Lava" for the magic built-in?
    show (Name pre nm) = pre ++ "::" ++ nm
    show (External nm) = "$" ++ nm
    show (Prim nm)     = nm
    show (Label nm)    = show nm
    show (TraceVal ovar _) = "^" ++ show ovar
    show (ClockId nm)    = "@" ++ nm	
--    show (UniqNm n)    = "#" ++ show (hashUnique n) -- might not be uniq
    show (Function _)  = "<fn>"
    show (BlackBox bx) = "<bb>"

-- | The type indirection to to allow the Eq/Ord to be provided without
-- crashing the Dynamic Eq/Ord space.
newtype Box a = Box a

-- I do not like this, but at least it is defined.
-- All black boxes are the same.
instance Eq (Box a) where { (==) _ _ = True }
instance Ord (Box a) where { compare _ _ = EQ }

-------------------------------------------------------------------------
-- | An 'Entity' Entity is our central "BOX" of compuation, round an 'Id'.

-- We tie the knot at the 'Entity' level, for observable sharing.
-- TODO, remove annotations; specialize to Type (because we always do).

data Entity ty a s = Entity Id [(String,ty)] [(String,ty,Driver s)] [a]
              deriving (Show, Eq, Ord)

instance T.Traversable (Entity ty a) where
  traverse f (Entity v vs ss dyn) =
    Entity v vs <$> (T.traverse (\ (val,ty,a) -> ((,,) val ty) `fmap` T.traverse f a) ss) <*> pure dyn

instance F.Foldable (Entity ty a) where
  foldMap f (Entity _ _ ss _) = mconcat [ F.foldMap f d | (_,_,d) <- ss ]

instance Functor (Entity ty a) where
    fmap f (Entity v vs ss dyn) = Entity v vs (fmap (\ (var,ty,a) -> (var,ty,fmap f a)) ss) dyn


-- 'E' is our knot tieing version of Entity.
newtype E = E (Entity Type Annotation E)

-- TODO: Remove after specialization
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

-------------------------------------------------------------------------
-- | A 'Driver' is a specific driven 'wire' (signal in VHDL),
-- which types contains a value that changes over time.

data Driver s = Port String s   -- ^ a specific port on the entity
              | Pad OVar        -- ^ an input pad
	      | ClkDom String	-- ^ the clock domain
              | Lit RepValue    -- ^ A representable Value (including unknowns, aka X in VHDL)
              | Generic Integer -- ^ A generic argument, always fully defined
              | Error String    -- ^ A call to err, in Datatype format for reification purposes
              deriving (Eq, Ord)

instance Show i => Show (Driver i) where
  show (Port v i) = "(" ++ show i ++ ")." ++ v
  show (Pad v) = show v
  show (ClkDom d) = "@" ++ d
  show (Lit x) = "'" ++ show x ++ "'"
  show (Generic x) = show x
  show (Error msg) = show $ "Error: " ++ msg
--  show (Env' env) = "<env>"

instance T.Traversable Driver where
  traverse f (Port v s)    = Port v <$> f s
  traverse _ (Pad v)       = pure $ Pad v
  traverse _ (ClkDom d)    = pure $ ClkDom d
--  traverse _ (PathPad v)   = pure $ PathPad v
  traverse _ (Lit i)       = pure $ Lit i
  traverse _ (Generic i)   = pure $ Generic i
  traverse _ (Error s)     = pure $ Error s
--  traverse _ (Env' env)     = pure $ Env' env

instance F.Foldable Driver where
  foldMap f (Port _ s)    = f s
  foldMap _ (Pad _)       = mempty
  foldMap _ (ClkDom _)    = mempty
--  foldMap _ (PathPad _)   = mempty
  foldMap _ (Lit _)       = mempty
  foldMap _ (Generic _)       = mempty
  foldMap _ (Error s)     = mempty

instance Functor Driver where
    fmap f (Port v s)    = Port v (f s)
    fmap _ (Pad v)       = Pad v
    fmap _ (ClkDom d)    = ClkDom d
--    fmap _ (PathPad v)   = PathPad v
    fmap _ (Lit i)       = Lit i
    fmap _ (Generic i)   = Generic i
    fmap _ (Error s)     = Error s


-- A 'D' is a "Deep", phantom-typed driver into an MuE or Entity
newtype D a = D { unD :: Driver E }
        deriving Show

---------------------------------------------------------------------------------------------------------
-- | class 'Clock' is a type that can be be used to represent a clock.

class Clock clk where
	clock :: D clk 

-- '()' is the default/standard/vanilla clock.	
instance Clock () where
	clock = D $ ClkDom "unit"

---------------------------------------------------------------------------------------------------------
-- | 'WireVal' is a value over a wire, either known or representing unknown.
-- The 'Show' class is overloaded to 'show' "?" for unknowns.


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
---- | Our bitwise representation value is a list of 'WireVal'.
-- The least significant bit is at the front of the list.

newtype RepValue = RepValue { unRepValue :: [WireVal Bool] }
        deriving (Eq, Ord)

instance Show RepValue where
        show (RepValue vals) = [ case v of
                                   WireUnknown   -> 'X'
                                   WireVal True  -> '1'
                                   WireVal False -> '0'
                               | v <- vals
                               ]

-- TODO: Read for RepValue?

-- | 'appendRepValue' joins two 'RepValue'; the least significant value first.
appendRepValue :: RepValue -> RepValue -> RepValue
appendRepValue (RepValue xs) (RepValue ys) = RepValue (xs ++ ys)


-- | 'isValidRepValue' checks to see is a 'RepValue' is completely valid.
isValidRepValue :: RepValue -> Bool
isValidRepValue (RepValue m) = and $ fmap isGood $ m
   where
        isGood :: WireVal Bool -> Bool
        isGood WireUnknown  = False
        isGood (WireVal {}) = True

-- | 'getValidRepValue' Rreturns a binary rep, or Nothing is *any* bits are 'X'.
getValidRepValue :: RepValue -> Maybe [Bool]
getValidRepValue r@(RepValue m)
        | isValidRepValue r = Just $ fmap (\ (WireVal v) -> v) m
        | otherwise         = Nothing

-- | 'cmpRepValue' compares a golden value with another value, returning the bits that are different.
-- The first value may contain 'X', in which case *any* value in that bit location will
-- match. This means that 'cmpRepValue' is not commutative.
cmpRepValue :: RepValue -> RepValue -> Bool
cmpRepValue (RepValue gs) (RepValue vs)
	| length gs == length vs
		= and $ zipWith (\ g v ->
			     case (g,v) of
				(WireUnknown,_)               -> True
				(WireVal True,WireVal True)   -> True
				(WireVal False,WireVal False) -> True
				_ -> False) gs vs
cmpRepValue _ _ = False

---------------------------------------------------------------------------------------------------------
-- 'TraceStream' is a typed stream, 

-- TODO: Consider why the Empty?
data TraceStream = TraceStream Type [RepValue] -- to recover type, eventually clock too?
                 | Empty
    deriving (Eq, Ord, Show)


type TraceMap k = M.Map k TraceStream

-- | 'Trace' is a primary bit-wise record of an interactive session with some circuit
data Trace = Trace { len :: Maybe Int
                   , inputs :: TraceMap OVar
                   , outputs :: TraceMap OVar
                   , probes :: TraceMap OVar
--                   , opts :: DebugOpts -- can see a case for this eventually
                   }

---------------------------------------------------------------------------------------------------------

-- TODO: Remove
data Annotation = Comment String                -- intended to arrive in the VHDL
                | Ann String Int
--                | Ann String Dynamic

instance Eq Annotation where {}
instance Ord Annotation where {}
instance Show Annotation where
    show (Comment str) = "Comment: " ++ str
    show _             = error "Show: Unknown Annotation"

---------------------------------------------------------------------------------------------------------
-- | 'Circuit' isd our primary way of representing a graph of entities.

data Circuit = Circuit
        { theCircuit :: [(Unique,MuE Unique)]
                -- ^ This the main graph. There is no actual node for the source or sink.
        , theSrcs    :: [(OVar,Type)]
                -- ^ this is a (convenence) list of the src values.
        , theSinks   :: [(OVar,Type,Driver Unique)]
                -- ^ these are the sinks; all values are generated from here.
        }


instance Show Circuit where
   show rCir = msg
     where
        showDriver d t = show d ++ " : " ++ show t

        bar = (replicate 78 '-') ++ "\n"

        inputs = unlines
                [ show var ++ " : " ++ show ty
                | (var,ty) <- sortBy (\ (OVar i _,_) (OVar j _,_) -> i `compare` j)
			    $ theSrcs rCir
                ]

        outputs = unlines
                [ show var   ++ " <- " ++ showDriver dr ty
                | (var,ty,dr) <- sortBy (\ (OVar i _,_,_) (OVar j _,_,_) -> i `compare` j)
			       $ theSinks rCir
                ]

        circuit = unlines
                [ case e of
                    Entity nm outs ins ann ->
                        "(" ++ show uq ++ ") " ++ show nm ++ "\n"
                            ++ unlines [ "      out    " ++ v ++ ":" ++ show ty | (v,ty) <- outs ]
                            ++ unlines [ "      in     " ++ v ++ " <- " ++ showDriver dr ty | (v,ty,dr) <- ins ]
			    ++ unlines [ "      case   " ++ show x ++ " -> " ++ show y
				       | (Function pairs) <- [nm]
				       , (x,y) <- pairs
				       ]
                            ++ unlines [ "      comment " ++ str | Comment str <- ann ]
                | (uq,e) <- theCircuit rCir
                ]

        msg = bar
                ++ "-- Inputs                                                                   --\n"
                ++ bar
                ++ inputs
                ++ bar
                ++ "-- Outputs                                                                  --\n"
                ++ bar
                ++ outputs
                ++ bar
                ++ "-- Entities                                                                 --\n"
                ++ bar
                ++ circuit
                ++ bar


-- | A 'Signature' is the structure-level type of a Circuit.
data Signature = Signature
        { sigInputs   :: [(OVar,Type)]
        , sigOutputs  :: [(OVar,Type)]
        , sigGenerics :: [(OVar,Integer)]
        }
        deriving (Show, Read, Eq)

circuitSignature :: Circuit -> Signature
circuitSignature cir = Signature
        { sigInputs   = theSrcs cir
        , sigOutputs  = [ (v,t) | (v,t,_) <- theSinks cir ]
        , sigGenerics = [] -- TODO: fix this
        }

-------------------------------------------------------------------------------------
-- | Create a type witness, to help resolve some of the type issues.
-- Really, we are using this in a system-F style. 
-- (As suggested by an anonymous TFP referee, as a better alterntive to using 'error "witness"').

-- TODO: Move into sized types.

data Witness w = Witness

-- | TODO: change to Witness a Not a type, but used as a first class type.
--witness :: a
--witness = error "witness"

