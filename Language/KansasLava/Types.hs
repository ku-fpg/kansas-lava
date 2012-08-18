{-# LANGUAGE TypeFamilies, Rank2Types, ScopedTypeVariables, GADTs, TypeOperators, EmptyDataDecls #-}

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
        -- * Id
        , Id(..)
        , Box(..)
        -- * Entity
        , Entity(..)
        , E(..)
	, entityFind
        -- * Driver
        , Driver(..)
        , D(..)
        -- * Ways of intepreting 'Signal'
        , Clock      -- type class
        , CLK
        -- * RepValue
        , RepValue(..)
        , showRepValue
        , showPackedRepValue
        , readPackedRepValue
        , appendRepValue
        , isValidRepValue
        , getValidRepValue
	, chooseRepValue
        , cmpRepValue
	-- * BitPat
	, BitPat(..)
	, (&)
	, bits
	, bool
	, every
	, bitPatToInteger
        -- * KLEG
        , KLEG(..)
        , visitEntities
        , mapEntities
        , allocEntities
        , Signature(..)
        , circuitSignature
        -- *Witness
        , Witness(..)
        -- * Dual shallow/deep
        , Dual(..)
	-- * Our version of tuples
	, (:>)(..)
	-- * Synthesis control
	, Synthesis(..)
        ) where

import Control.Applicative

import Data.Char
import Data.Dynamic
import qualified Data.Foldable as F
import Data.List as L
import Data.Maybe
import Data.Monoid hiding (Dual)
import Data.Reify
import Data.Ratio
import qualified Data.Traversable as T
import Data.Sized.Ix
import GHC.Exts( IsString(..) )

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
        | MessageTy
                        -- ^ A message (only in shallow)
        deriving (Eq, Ord)

-- | 'typeWidth' returns the width of a type when represented in VHDL.
typeWidth :: Type -> Int
typeWidth B  = 1
typeWidth ClkTy = 1
typeWidth (S x) = x
typeWidth (U x) = x
typeWidth (V x) = x
typeWidth (TupleTy tys) = sum (map typeWidth tys)
typeWidth (MatrixTy i ty) = i * typeWidth ty
typeWidth (SampledTy _ i) = i
typeWidth (MessageTy)     = 0
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
isTypeSigned GenericTy = False
isTypeSigned (RomTy _) = False
isTypeSigned (TupleTy _) = False
isTypeSigned (MatrixTy _ _) = False
isTypeSigned (MessageTy) = False

instance Show Type where
        show B          = "B"
        show ClkTy      = "Clk"
        show GenericTy  = "G"
        show (RomTy i)  = show i ++ "R"
        show (S i)      = show i ++ "S"
        show (U i)      = show i ++ "U"
        show (V i)      = show i ++ "V"
        show (TupleTy tys) = show tys
        show (MatrixTy i ty) = show i ++ "[" ++ show ty ++ "]"
        show (SampledTy m n) = "Sampled " ++ show m ++ " " ++ show n
        show (MessageTy)     = "T"      -- T for text

-- This is required for the deserialization of Trace objects.
instance Read Type where
    readsPrec p (x:xs) | isSpace x = readsPrec p xs -- chew up whitespace?
    readsPrec _ xs | hasSizePrefix xs = [fromJust $ parseSize xs]
        where hasSizePrefix = isJust . parseSize
              parseSize str = let (ds,cs) = span isDigit str
                              in case cs of
                                   (c:rest) | not (null ds) && c `elem` ['U', 'S', 'V','R']
                                            -> Just (con c (read ds :: Int), rest)
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
        where ("Sampled ",r1) = break isDigit xs
              (m,' ':r2) = span isDigit r1
              (n,rest) = span isDigit r2
    readsPrec _ xs | foldr (\s b -> b || s `isPrefixOf` xs) False strs =
                        concat [ maybe [] (\rest -> [(con,rest)]) (stripPrefix str xs)
                               | (con,str) <- zip [B  , ClkTy, GenericTy] strs
                               ]
        where strs = ["B", "Clk", "G"]
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

-- | toStdLogic maps Lava Types to a StdLogicType
toStdLogicType :: Type -> StdLogicType
toStdLogicType B               = SL
toStdLogicType ClkTy           = SL
toStdLogicType (V n)           = SLV n
toStdLogicType GenericTy       = G
toStdLogicType (MatrixTy i ty) = SLVA i (fromIntegral size')
  where size' = typeWidth ty
toStdLogicType ty              = SLV $ fromIntegral size'
  where size' = typeWidth ty

-- | fromStdLogic maps StdLogicTypes to Lava types.
fromStdLogicType :: StdLogicType -> Type
fromStdLogicType SL         = B
fromStdLogicType (SLV n)    = V n
fromStdLogicType (SLVA n m) = MatrixTy n (V m)
fromStdLogicType G          = GenericTy

-------------------------------------------------------------------------
-- | Id is the name/tag of a block of compuation.
data Id = Prim String                           -- ^ built in thing
        | External String                       -- ^ VHDL entity
        | Function [(RepValue,RepValue)]        -- ^ anonymous function

        | ClockId String                        -- ^ An environment box

        | Comment [String]                      -- ^ An identity; also a multi-line comments
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
    show (External nm) = '$':nm
    show (Prim nm)     = nm
    show (ClockId nm)    = '@':nm
--    show (UniqNm n)    = "#" ++ show (hashUnique n) -- might not be uniq
    show (Function _)  = "<fn>"
    show (BlackBox _) = "<bb>"
    show (Comment xs) = "{- " ++ show xs ++ " -}"

-- | Box wraps a dynamic, so that we can define custom Eq/Ord instances.
newtype Box a = Box a

-- I do not like this, but at least it is defined.
-- All black boxes are the same.
instance Eq (Box a) where { (==) _ _ = True }
instance Ord (Box a) where { compare _ _ = EQ }

-------------------------------------------------------------------------


-- We tie the knot at the 'Entity' level, for observable sharing.

-- | An 'Entity' Entity is our central "BOX" of computation, round an 'Id'.
data Entity s = Entity Id [(String,Type)] [(String,Type,Driver s)]
              deriving (Show, Eq, Ord)

-- | entityFind finds an input in a list, avoiding the need to have ordering.
entityFind :: (Show a) => String -> Entity a -> (Type, Driver a)
entityFind nm e@(Entity _ _ ins) =
	case [ (t,p) | (nm',t,p) <- ins, nm == nm' ] of
	  [] -> error $ "can not find : " ++ show nm ++ " in " ++ show e
	  [x] -> x
	  _ ->  error $ "found multiple : " ++ show nm ++ " in " ++ show e


instance T.Traversable Entity where
  traverse f (Entity v vs ss) =
    Entity v vs <$> T.traverse (\ (val,ty,a) -> (,,) val ty `fmap` T.traverse f a) ss

instance F.Foldable Entity where
  foldMap f (Entity _ _ ss) = mconcat [ F.foldMap f d | (_,_,d) <- ss ]

instance Functor Entity where
    fmap f (Entity v vs ss) = Entity v vs (fmap (\ (var,ty,a) -> (var,ty,fmap f a)) ss)


-- | 'E' is the knot-tyed version of Entity.
newtype E = E (Entity E)


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
              | Pad String      -- ^ an input pad
              | ClkDom String   -- ^ the clock domain (the clock enable, resolved via context)
              | Lit RepValue    -- ^ A representable Value (including unknowns, aka X in VHDL)
              | Generic Integer -- ^ A generic argument, always fully defined
              | Lits [RepValue] -- ^ A list of values, typically constituting a ROM initialization.
              | Error String    -- ^ A call to err, in Datatype format for reification purposes
              deriving (Eq, Ord)

instance Show i => Show (Driver i) where
  show (Port v i) = "(" ++ show i ++ ")." ++ v
  show (Pad v) = show v
  show (ClkDom d) = '@':d
  show (Lit x) = "'" ++ show x ++ "'"
  show (Lits xs) = show (take 16 xs) ++ "..."
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
-- | The 'D' type adds a phantom type to a driver.
newtype D a = D { unD :: Driver E } deriving Show

---------------------------------------------------------------------------------------------------------
-- | class 'Clock' is a type that can be be used to represent a clock.
class Clock clk where {}

-- | generic/default/board/standard/vanilla clock.
data CLK
instance Clock CLK where {}

---------------------------------------------------------------------------------------------------------
-- | A RepValue is a value that can be represented using a bit encoding.  The
-- least significant bit is at the front of the list.
newtype RepValue = RepValue { unRepValue :: [Maybe Bool] }
        deriving (Eq, Ord)

instance Show RepValue where
        show val = "0b" ++ showPackedRepValue val

showPackedRepValue :: RepValue -> String
showPackedRepValue (RepValue vals) =
        [ case v of
           Nothing   -> 'X'
           Just True  -> '1'
           Just False -> '0'
        | v <- reverse vals
        ]


readPackedRepValue :: String -> Maybe RepValue
readPackedRepValue xs | L.all (`elem` "01XU") xs
        = Just
        $ RepValue
        $ map (\ c -> case c of
                        'X' -> Nothing
                        'U' -> Nothing
                        '0' -> Just False
                        '1' -> Just True)
        $ xs
readPackedRepValue _ = Nothing

instance Read RepValue where
        readsPrec _ ('0':'b':xs)
		      = [ (RepValue [ case c of
                                        'X' -> Nothing
                                        'U' -> Nothing
                                        '0' -> Just False
                                        '1' -> Just True
                                        v -> error $ "Can't read RepValue " ++ show v
                                    | c <- reverse cs
                                    ]
                          ,rest)]
            where (cs,rest) = span (`elem` "01XU") xs
	readsPrec _ other = error $ "Can't read RepValue " ++ show other

showRepValue :: Type -> RepValue -> String
showRepValue (TupleTy tys) (RepValue vals) =
        "(" ++ concat [ sep ++ showRepValue ty (RepValue (take (typeWidth ty) (drop len vals)))
                      | (ty,len,sep) <- zip3 tys lens' ("" : repeat ",")
                      ] ++ ")"
  where
          lens = map typeWidth tys
          lens' = 0 : zipWith (+) lens' lens
showRepValue (MatrixTy i ty) (RepValue vals) =
        "[" ++ concat [ sep ++ showRepValue ty (RepValue (take (typeWidth ty) (drop len vals)))
                      | (len,sep) <- take i $ zip lens' ("" : repeat ",")
                      ] ++ "]"
  where
          lens = map typeWidth (replicate i ty)
          lens' = 0 : zipWith (+) lens' lens
showRepValue ty repValue | isValidRepValue repValue = case ty of
        B -> case vals of
                [True] -> "high"
                [False] -> "low"
                _ -> sizeError
        S n | n == length vals -> show signed_number
        S _ | otherwise -> sizeError
        U n | n == length vals -> show number
        U _ | otherwise -> sizeError
        V n | n == length vals -> show repValue
        V _ | otherwise -> sizeError

        -- We should reconsider the other cases
        _   -> show repValue
  where
          sizeError = error $ "size error with " ++ show repValue ++ " (ty = " ++ show ty ++ ")"
          Just vals = getValidRepValue repValue
          number :: Integer
          number   = sum [ n
                         | (n,True) <- zip (iterate (*2) 1) vals
                         ]

          signed_number :: Integer
          signed_number = sum
                         [ n
                         | (n,True) <- zip (iterate (*2) 1) (init vals)
                         ] * if last vals then (-1) else 1


-- Show the structure if there are *any* value bits.
showRepValue _ty repValue@(RepValue xs)
        | any isJust xs = show repValue
-- Otherwise, just show ?
showRepValue _ty _repValue = "?"

-- | 'appendRepValue' joins two 'RepValue'; the least significant value first.
-- TODO: reverse this!
appendRepValue :: RepValue -> RepValue -> RepValue
appendRepValue (RepValue xs) (RepValue ys) = RepValue (xs ++ ys)


-- | 'isValidRepValue' checks to see is a 'RepValue' is completely valid.
isValidRepValue :: RepValue -> Bool
isValidRepValue (RepValue m) = and $ fmap isGood m
   where
        isGood :: Maybe Bool -> Bool
        isGood Nothing  = False
        isGood (Just {}) = True

-- | 'getValidRepValue' Returns a binary rep, or Nothing is *any* bits are 'X'.
getValidRepValue :: RepValue -> Maybe [Bool]
getValidRepValue r@(RepValue m)
        | isValidRepValue r = Just $ fmap f m
        | otherwise         = Nothing
  where f (Just v) = v
        f Nothing = error "Can't get the value of an unknown wire."


-- | 'chooseRepValue' turns a RepValue with (optional) unknow values,
-- and chooses a representation for the RepValue.
chooseRepValue :: RepValue -> RepValue
chooseRepValue (RepValue xs) = RepValue $ map f xs
  where
	f Nothing = Just False
	f other	  = other

-- | 'cmpRepValue' compares a golden value with another value, returning the bits that are different.
-- The first value may contain 'X', in which case *any* value in that bit location will
-- match. This means that 'cmpRepValue' is not commutative.
cmpRepValue :: RepValue -> RepValue -> Bool
cmpRepValue (RepValue gs) (RepValue vs)
        | length gs == length vs
                = and $ zipWith (\ g v ->
                             case (g,v) of
                                (Nothing,_)             -> True
                                (Just True,Just True)   -> True
                                (Just False,Just False) -> True
                                _ -> False) gs vs
cmpRepValue _ _ = False

---------------------------------------------------------------------------------------------------------
-- BitPat is a small DSL for writing bit-patterns.
-- It is bit-endian, unlike other parts of KL.
-- It is also a sized version of RepValue.

data BitPat w = BitPat { bitPatToRepValue :: RepValue }
    deriving (Eq, Ord, Show)

-- | '&' is a sized append for BitPat.
infixl 6 &
(&) :: (Size w1, Size w2, Size w, w ~ ADD w1 w2, w1 ~ SUB w w2, w2 ~ SUB w w1)
    => BitPat w1 -> BitPat w2 -> BitPat w
(BitPat a) & (BitPat b) = BitPat (appendRepValue b a)

instance (Size w) => Num (BitPat w) where
    (+) = error "(+) undefined for BitPat"
    (*) = error "(*) undefined for BitPat"
    abs = error "abs undefined for BitPat"
    signum = error "signum undefined for BitPat"
    fromInteger n
	| n >= 2^(size (error "witness" :: w))
	= error $ "fromInteger: out of range, value = " ++  show n
	| otherwise
	= BitPat $ RepValue
		           $ take (size (error "witness" :: w))
                           $ map (Just . odd)
			   $ iterate (`div` (2::Integer))
			   $ n

instance (Size w) => Real (BitPat w) where
	toRational n = toInteger n % 1

instance (Size w) => Enum (BitPat w) where
	toEnum = fromInteger . fromIntegral
	fromEnum p = case bitPatToInteger p of
			Nothing -> error $ "fromEnum failure: " ++ show p
			Just i -> fromIntegral i
instance (Size w) => Integral (BitPat w) where
	quotRem = error "quotRem undefined for BitPat"
	toInteger p = case bitPatToInteger p of
			Nothing -> error $ "toInteger failure: " ++ show p
			Just i -> i

bitPatToInteger :: BitPat w -> Maybe Integer
bitPatToInteger (BitPat rv) = case getValidRepValue rv of
	Nothing -> Nothing
	Just xs -> return $
		sum [ n
                    | (n,b) <- zip (iterate (* 2) 1)
                       		    xs
                    , b
                    ]

instance IsString (BitPat w) where
  fromString = bits

bits :: String -> BitPat w
bits = BitPat . RepValue . map f . reverse
    where
	f '0' = return False
	f '1' = return True
	f 'X' = Nothing
	f '_' = Nothing
	f '-' = Nothing
	f o   = error $ "bit pattern, expecting one of 01X_-, found " ++ show o

bool :: BitPat X1 -> Bool
bool (BitPat (RepValue [Just b])) = b
bool other = error $ "bool: expecting bool isomophism, found: " ++ show other

every :: forall w . (Size w) => [BitPat w]
every = [ BitPat $ RepValue (fmap Just count) | count <- counts n ]
   where
    n = size (error "witness" :: w)
    counts :: Int -> [[Bool]]
    counts 0 = [[]]
    counts num = [ x : xs |  xs <- counts (num-1), x <- [False,True] ]

---------------------------------------------------------------------------------------------------------
-- | 'KLEG' (Kansas Lava Entity Graph) is our primary way of representing a graph of entities.
data KLEG = KLEG
        { theCircuit :: [(Unique,Entity Unique)]
                -- ^ This the main graph. There is no actual node for the source or sink.
        , theSrcs    :: [(String,Type)]
                -- ^ this is a (convenience) list of the src values.
        , theSinks   :: [(String,Type,Driver Unique)]
                -- ^ these are the sinks; all values are generated from here.
        }


instance Show KLEG where
   show rCir = msg
     where
        showDriver d t = show d ++ " : " ++ show t

        bar = replicate 78 '-' ++ "\n"

        circInputs = unlines
                [ show var ++ " : " ++ show ty
                | (var,ty) <- sort $ theSrcs rCir
                ]

        circOutputs = unlines
                [ show var   ++ " <- " ++ showDriver dr ty
                | (var,ty,dr) <- sort $ theSinks rCir
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

-- | Map a function across all of the entities in a KLEG, accumulating the results in a list.
visitEntities :: KLEG -> (Unique -> Entity Unique -> Maybe a) -> [a]
visitEntities cir fn =
        [ a
        | (u,m) <- theCircuit cir
        , Just a <- [fn u m]
        ]

-- | Map a function across a KLEG, modifying each Entity for which the function
-- returns a Just. Any entities that the function returns Nothing for will be
-- removed from the resulting KLEG.
mapEntities :: KLEG -> (Unique -> Entity Unique -> Maybe (Entity Unique)) -> KLEG
mapEntities cir fn = cir { theCircuit =
                                [ (u,a)
                                | (u,m) <- theCircuit cir
                                , Just a <- [fn u m]
                                ] }

-- | Generate a list of Unique ids that are guaranteed not to conflict with any
-- ids already in the KLEG.
allocEntities :: KLEG -> [Unique]
allocEntities cir = [ highest + i | i <- [1..]]
   where
        highest = maximum (0 : visitEntities cir (\ u _ -> return u))

-- | A 'Signature' is the structure-level type of a KLEG.
data Signature = Signature
        { sigInputs   :: [(String,Type)]
        , sigOutputs  :: [(String,Type)]
        , sigGenerics :: [(String,Integer)]
        }
        deriving (Show, Read, Eq)

-- | Calculate a signature from a KLEG.
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
data Witness w = Witness

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


----------------------------------------------------------------------------
-- Our version of tuples, with a right leaning (aka lists).
infixr 5 :>
-- | Alternative definition for (,). Constructor is right-associative.
data a :> b = a :> b deriving (Eq, Ord, Show, Read)


----------------------------------------------------------------------------
-- | How to balance our circuits. Typically use 'Sweet'(spot), but
-- 'Small' has permission to take longer, and 'Fast' has permission
-- use extra gates.

data Synthesis = Small | Sweet | Fast

