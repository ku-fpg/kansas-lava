{-# LANGUAGE TypeFamilies, Rank2Types, ScopedTypeVariables, GADTs #-}

-- | This module contains the key internal types for Kansas Lava,
-- and some basic utilities (like Show instances) for these types.

module Language.KansasLava.Types (
        -- * Types
          Type(..)
        , typeWidth
        , isTypeSigned
        , StdLogicType(..)
        , toStdLogicType
        , fromStdLogicType
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
        , Clock(..)     -- type class
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
        -- * KLEG
        , KLEG(..)
        , visitEntities
        , mapEntities
        , allocEntities
        , Signature(..)
        , circuitSignature
        -- *Witness
        , Witness(..)
        -- I and O
        , I
        , O
        -- *Dual shallow/deep
        , Dual(..)
        ) where

import Control.Applicative

import Data.Char
import Data.Dynamic
import qualified Data.Foldable as F
import Data.List as L
import Data.Maybe
import Data.Monoid hiding (Dual)
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

	-- TODO: change to StdLogicTy, because it is clock independent
        | ClkTy         -- ^ Clock Signal

        | ClkDomTy      -- ^ The clock domain type, which has a clock, a clock enable,
                        -- and an hybrid asyncronized/syncronized reset.

        | GenericTy     -- ^ generics in VHDL, argument must be integer

        | RomTy Int     -- ^ a constant array of values.

        | TupleTy [Type]
                        -- ^ Tuple, represented as a larger std_logic_vector
        | MatrixTy Int Type
                        -- ^ Matrix, for example a vhdl array.

        -- TODO: Call this FixedPointTy
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
isTypeSigned ClkDomTy = False
isTypeSigned GenericTy = False
isTypeSigned (RomTy _) = False
isTypeSigned (TupleTy _) = False
isTypeSigned (MatrixTy _ _) = False

instance Show Type where
        show B          = "B"
        show ClkTy      = "Clk"
        show ClkDomTy   = "ClkDom"
        show GenericTy  = "G"
        show (RomTy i)  = show i ++ "R"
        show (S i)      = show i ++ "S"
        show (U i)      = show i ++ "U"
        show (V i)      = show i ++ "V"
        show (TupleTy tys) = show tys
        show (MatrixTy i ty) = show i ++ "[" ++ show ty ++ "]"
        show (SampledTy m n) = "Sampled " ++ show m ++ " " ++ show n

-- This is required for the deserialization of Trace objects.
instance Read Type where
    readsPrec p (x:xs) | isSpace x = readsPrec p xs -- chew up whitespace?
    readsPrec _ xs | hasSizePrefix xs = [fromJust $ parseSize xs]
        where hasSizePrefix = isJust . parseSize
              parseSize str = let (ds,cs) = span isDigit str
                              in case cs of
                                   (c:rest) | (not $ null ds) && c `elem` ['U', 'S', 'V','R']
                                            -> Just ((con c) (read ds :: Int), rest)
                                   ('[':rest) | (not $ null ds) ->
                                        case [ (MatrixTy (read ds :: Int) ty,rest')
                                             | (ty,']':rest') <- reads rest
                                             ] of
                                          [] -> Nothing
                                          (x:_) -> Just x
                                   _ -> Nothing
              con ch = case ch of
                        'U' -> U
                        'S' -> S
                        'V' -> V
                        'R' -> RomTy
                        ty   -> error $ "Can't read type" ++ show ty
    readsPrec _ xs | "Sampled" `isPrefixOf` xs = [(SampledTy (read m :: Int) (read n :: Int),rest)]
        where ("Sampled ",r1) = span (not . isDigit) xs
              (m,' ':r2) = span isDigit r1
              (n,rest) = span isDigit r2
    readsPrec _ xs | foldr (\s b -> b || s `isPrefixOf` xs) False strs =
                        concat [ maybe [] (\rest -> [(con,rest)]) (stripPrefix str xs)
                               | (con,str) <- zip [B  , ClkTy, ClkDomTy, GenericTy] strs
                               ]
        where strs = ["B", "Clk", "ClkDom", "G"]
    readsPrec _ xs@('[':_) = [ (TupleTy tys,rest)| (tys,rest) <- readList xs ]
    readsPrec _ what = error $ "read Type - can't parse: " ++ what

-------------------------------------------------------------------------
-- | 'StdLogicType' is the type for std_logic things,
-- typically input/output arguments to VHDL entities.

data StdLogicType
        = SL            -- ^ std_logic
        | SLV Int       -- ^ std_logic_vector (n-1 downto 0)
        | SLVA Int Int  -- ^ std_logic_vector (n-1 downto 0) (m-1 downto 0)
        | G             -- ^ generic (inward) argument
       deriving (Eq, Ord)

instance Show StdLogicType where
        show (SL)       = "std_logic"
        show (SLV n)    = "std_logic_vector(" ++ show (n-1) ++ " downto 0)"
        show (SLVA n m) = "std_logic_array_" ++ show n ++ "_" ++ show m
        show (G)        = "generic"

-- These encode how we translate from std_logic to regular Lava types
toStdLogicType :: Type -> StdLogicType
toStdLogicType B               = SL
toStdLogicType ClkTy           = SL
toStdLogicType (V n)           = SLV n
toStdLogicType GenericTy       = G
toStdLogicType (MatrixTy i ty) = SLVA i (fromIntegral size)
  where size = typeWidth ty
toStdLogicType ty              = SLV $ fromIntegral size
  where size = typeWidth ty

fromStdLogicType :: StdLogicType -> Type
fromStdLogicType SL         = B
fromStdLogicType (SLV n)    = V n
fromStdLogicType (SLVA n m) = MatrixTy n (V m)
fromStdLogicType G          = GenericTy

-------------------------------------------------------------------------
-- | 'OVar' is an order Var, with a number that is used for sorting purposes.
-- This is used for names of arguments in VHDL, and other places, so
-- that arguments can preserve order through transformation.

data OVar = OVar Int String             -- The # is used purely for sorting order.
                                        -- Idea: there can be several with the same #;
                                        -- its about ordering.

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

data Id = Prim String                           -- ^ built in thing
        | External String                       -- ^ VHDL entity
        | Function [(RepValue,RepValue)]        -- ^ anonymous function


                                                --
        | TraceVal [OVar] TraceStream           -- ^ trace (probes, etc)
                                                -- may have multiple names matching same data
                                                -- This is type of identity
                                                -- that records its shallow value,
                                                -- for later inspection

        | ClockId String                        -- ^ An environment box

        | Label String                          -- ^ An identity; also a name, used to tag function arguments
        | Comment' [String]                     -- ^ An identity; also a multi-line comments
        | BlackBox (Box Dynamic)                -- ^ 'BlackBox' can be removed without harm
                                                -- The rule is you can only insert you own
                                                -- types in here (or use newtype).
                                                -- Prelude or other peoples types
                                                -- are not allowed (because typecase becomes ambigious)

    deriving (Eq, Ord)


{-
 - List of prims
        id              ::
        index           :: M<n> -> ix -> n
        proj            :: G<Int> ->
-}

instance Show Id where
    show (External nm) = "$" ++ nm
    show (Prim nm)     = nm
    show (Label nm)    = show nm ++ ":"
    show (TraceVal ovar _) = "^" ++ show ovar
    show (ClockId nm)    = "@" ++ nm
--    show (UniqNm n)    = "#" ++ show (hashUnique n) -- might not be uniq
    show (Function _)  = "<fn>"
    show (BlackBox _) = "<bb>"
    show (Comment' xs) = "{- " ++ show xs ++ " -}"

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

data Entity s = Entity Id [(String,Type)] [(String,Type,Driver s)]
              deriving (Show, Eq, Ord)

instance T.Traversable Entity where
  traverse f (Entity v vs ss) =
    Entity v vs <$> (T.traverse (\ (val,ty,a) -> ((,,) val ty) `fmap` T.traverse f a) ss)

instance F.Foldable Entity where
  foldMap f (Entity _ _ ss) = mconcat [ F.foldMap f d | (_,_,d) <- ss ]

instance Functor Entity where
    fmap f (Entity v vs ss) = Entity v vs (fmap (\ (var,ty,a) -> (var,ty,fmap f a)) ss)


-- 'E' is our knot tieing version of Entity.
newtype E = E (Entity E)

-- TODO: Remove after specialization
--type Entity u = Entity Type Annotation u

-- You want to observe
instance MuRef E where
  type DeRef E = Entity
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
              | ClkDom String   -- ^ the clock domain (the clock enable, resolved via context)
              | Lit RepValue    -- ^ A representable Value (including unknowns, aka X in VHDL)
              | Generic Integer -- ^ A generic argument, always fully defined
              | Lits [RepValue] -- ^ A list of values, typically constituting a ROM initialization.
              | Error String    -- ^ A call to err, in Datatype format for reification purposes
              deriving (Eq, Ord)

instance Show i => Show (Driver i) where
  show (Port v i) = "(" ++ show i ++ ")." ++ v
  show (Pad v) = show v
  show (ClkDom d) = "@" ++ d
  show (Lit x) = "'" ++ show x ++ "'"
  show (Lits xs) = (show $ take 16 xs) ++ "..."
  show (Generic x) = show x
  show (Error msg) = show $ "Error: " ++ msg
--  show (Env' env) = "<env>"

instance T.Traversable Driver where
  traverse f (Port v s)    = Port v <$> f s
  traverse _ (Pad v)       = pure $ Pad v
  traverse _ (ClkDom d)    = pure $ ClkDom d
--  traverse _ (PathPad v)   = pure $ PathPad v
  traverse _ (Lit i)       = pure $ Lit i
  traverse _ (Lits vals)   = pure $ Lits vals
  traverse _ (Generic i)   = pure $ Generic i
  traverse _ (Error s)     = pure $ Error s
--  traverse _ (Env' env)     = pure $ Env' env

instance F.Foldable Driver where
  foldMap f (Port _ s)    = f s
  foldMap _ (Pad _)       = mempty
  foldMap _ (ClkDom _)    = mempty
--  foldMap _ (PathPad _)   = mempty
  foldMap _ (Lit _)       = mempty
  foldMap _ (Lits _)       = mempty
  foldMap _ (Generic _)       = mempty
  foldMap _ (Error _)     = mempty

instance Functor Driver where
    fmap f (Port v s)    = Port v (f s)
    fmap _ (Pad v)       = Pad v
    fmap _ (ClkDom d)    = ClkDom d
--    fmap _ (PathPad v)   = PathPad v
    fmap _ (Lit i)       = Lit i
    fmap _ (Lits vals)   = Lits vals
    fmap _ (Generic i)   = Generic i
    fmap _ (Error s)     = Error s


-- A 'D' is a "Deep", phantom-typed driver into an Entity or Entity
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
        fmap _ WireUnknown = WireUnknown
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

instance Read RepValue where
        readsPrec _ xs = [(RepValue [ case c of
                                        'X' -> WireUnknown
                                        'U' -> WireUnknown
                                        '0' -> WireVal False
                                        '1' -> WireVal True
                                        v -> error $ "Can't read repvalue " ++ show v
                                    | c <- cs
                                    ]
                          ,rest)]
            where (cs,rest) = span (`elem` "01XU") xs

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
        | isValidRepValue r = Just $ fmap f m
        | otherwise         = Nothing
  where f (WireVal v) = v
        f WireUnknown = error "Can't get the value of an unknown wire."

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
data TraceStream = TraceStream Type [RepValue] -- to recover type, eventually clock too?
    deriving (Eq, Ord, Read)

-- ACF: This is a hack to prevent infinite printing,
--      but for now we obey the rules here, so we can derive Read above
instance Show TraceStream where
    show (TraceStream ty strm) = "TraceStream " ++ show ty ++ " " ++ show (take 1000 strm)

---------------------------------------------------------------------------------------------------------
-- | 'KLEG' (Kansas Lava Entity Graph) is our primary way of representing a graph of entities.
data KLEG = KLEG
        { theCircuit :: [(Unique,Entity Unique)]
                -- ^ This the main graph. There is no actual node for the source or sink.
        , theSrcs    :: [(OVar,Type)]
                -- ^ this is a (convenience) list of the src values.
        , theSinks   :: [(OVar,Type,Driver Unique)]
                -- ^ these are the sinks; all values are generated from here.
        }


instance Show KLEG where
   show rCir = msg
     where
        showDriver d t = show d ++ " : " ++ show t

        bar = (replicate 78 '-') ++ "\n"

        circInputs = unlines
                [ show var ++ " : " ++ show ty
                | (var,ty) <- sortBy (\ (OVar i _,_) (OVar j _,_) -> i `compare` j)
                            $ theSrcs rCir
                ]

        circOutputs = unlines
                [ show var   ++ " <- " ++ showDriver dr ty
                | (var,ty,dr) <- sortBy (\ (OVar i _,_,_) (OVar j _,_,_) -> i `compare` j)
                               $ theSinks rCir
                ]

        circuit = unlines
                [ case e of
                    Entity nm outs ins ->
                        "(" ++ show uq ++ ") " ++ show nm ++ "\n"
                            ++ unlines [ "      out    " ++ v ++ ":" ++ show ty | (v,ty) <- outs ]
                            ++ unlines [ "      in     " ++ v ++ " <- " ++ showDriver dr ty | (v,ty,dr) <- ins ]
                            ++ unlines [ "      case   " ++ show x ++ " -> " ++ show y
                                       | (Function pairs) <- [nm]
                                       , (x,y) <- pairs
                                       ]
                | (uq,e) <- theCircuit rCir
                ]

        msg = bar
                ++ "-- Inputs                                                                   --\n"
                ++ bar
                ++ circInputs
                ++ bar
                ++ "-- Outputs                                                                  --\n"
                ++ bar
                ++ circOutputs
                ++ bar
                ++ "-- Entities                                                                 --\n"
                ++ bar
                ++ circuit
                ++ bar


visitEntities :: KLEG -> (Unique -> Entity Unique -> Maybe a) -> [a]
visitEntities cir fn =
        [ a
        | (u,m) <- theCircuit cir
        , Just a <- [fn u m]
        ]

mapEntities :: KLEG -> (Unique -> Entity Unique -> Maybe (Entity Unique)) -> KLEG
mapEntities cir fn = cir { theCircuit =
                                [ (u,a)
                                | (u,m) <- theCircuit cir
                                , Just a <- [fn u m]
                                ] }


allocEntities :: KLEG -> [Unique]
allocEntities cir = [ highest + i | i <- [1..]]
   where
        highest = maximum (0 : (visitEntities cir $ \ u _ -> return u))

-- | A 'Signature' is the structure-level type of a KLEG.
data Signature = Signature
        { sigInputs   :: [(OVar,Type)]
        , sigOutputs  :: [(OVar,Type)]
        , sigGenerics :: [(OVar,Integer)]
        }
        deriving (Show, Read, Eq)

circuitSignature :: KLEG -> Signature
circuitSignature cir = Signature
        { sigInputs   = theSrcs cir
        , sigOutputs  = [ (v,t) | (v,t,_) <- theSinks cir ]
        , sigGenerics = [] -- TODO: fix this
        }

-------------------------------------------------------------------------------------
-- | Create a type witness, to help resolve some of the type issues.
-- Really, we are using this in a system-F style.
-- (As suggested by an anonymous TFP referee, as a better alternative to using 'error "witness"').

-- TODO: Move into sized types.

data Witness w = Witness

-- | TODO: change to Witness a Not a type, but used as a first class type.
--witness :: a
--witness = error "witness"

--------------------------------------------------------------------------------------
-- We sometimes talk about bytes, which are unsigned 8-bit values.

-- type Byte = U8

--------------------------------------------------------------------------------------

-- Prepare yourself for an ASCII FIFO diagram!
--
--                ============
--      input---->=   FIFO   =---->output
--  writeflag<----=          =<----readflag
--                ============
--
-- where readflag  :: CSeq c Bool = read successful
--       writeflag :: CSeq c Bool = write successful
--
-- eventually, these may become datatypes.
type I input     readflag = (input    ,readflag)
type O writeflag output   = (writeflag,output  )


----------------------------------------------------------------------------

-- | Select the shallow embedding from one circuit, and the deep embedding from another.
class Dual a where
    -- | Take the shallow value from the first argument, and the deep value from the second.
    dual :: a -> a -> a

instance (Dual a, Dual b) => Dual (a,b) where
	dual (a1,b1) (a2,b2) = (dual a1 a2,dual b1 b2)

instance (Dual a, Dual b,Dual c) => Dual (a,b,c) where
	dual (a1,b1,c1) (a2,b2,c2) = (dual a1 a2,dual b1 b2,dual c1 c2)

instance (Dual b) => Dual (a -> b) where
	dual f1 f2 x = dual (f1 x) (f2 x)
