{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances
 #-}
module Language.KansasLava.Fabric
        ( Fabric(..)
        , Pad(..)
        , runFabric
        , inStdLogic
        , inStdLogicVector
        , inGeneric
        , outStdLogic
        , outStdLogicVector
        , padStdLogicType
--        , driving
--        , fabric_example
        , backedges
        , genFabric, MakeFabric
        ) where


import Language.KansasLava.Types
import Language.KansasLava.Seq
import Data.Sized.Unsigned
import Data.Sized.Ix
import Control.Monad.Fix
import Control.Monad
import Control.Monad.State

-- For testing
import Language.KansasLava.Utils


-- The '_' will disappear soon from these names.

data Pad = StdLogic_ (Seq Bool)
         | forall x . (Size x) => StdLogicVector_ (Seq (Unsigned x))
         | Generic_ Integer

padStdLogicType :: Pad -> StdLogicType
padStdLogicType (StdLogic_ _)       = SL
padStdLogicType (StdLogicVector_ s) = SLV $ size (untype s)
    where untype :: Seq (Unsigned a) -> a
          untype = error "untype"
padStdLogicType (Generic_ _)        = G

instance Show Pad where
        show (StdLogic_ sq)       = "StdLogic_ " ++ show sq
        show (StdLogicVector_ sq) = "StdLogicVector_ " ++ show sq
        show (Generic_ i)         = "Generic_ " ++ show i

         -- TODO: the 2D Array

-- The 'Fabric' structure, which is also a monad.
data Fabric a = Fabric { unFabric :: [(String,Pad)] -> (a,[(String,Pad)],[(String,Pad)]) }

instance Functor Fabric where
        fmap f fab = fab >>= \ a -> return (f a)

instance Monad Fabric where
        return a = Fabric $ \ _ -> (a,[],[])
        (Fabric f) >>= k = Fabric $ \ ins -> let
                          (a,in_names,outs) = f ins
                          (r,in_names',outs') = unFabric (k a) ins
                       in (r,in_names ++ in_names',outs ++ outs')

instance MonadFix Fabric where
        mfix f = Fabric $ \ env -> let (a,in_names,outs) = unFabric (f a) env
                                   in (a,in_names,outs)



input :: String -> Pad -> Fabric Pad
input nm deepPad = Fabric $ \ ins ->
        let p = case lookup nm ins of
                   Just v -> v
                   _ -> error $ "input internal error finding : " ++ show nm
        in (p,[(nm,deepPad)],[])


output :: String -> Pad -> Fabric ()
output nm pad = Fabric $ \ _ins -> ((),[],[(nm,pad)])

--inStdLogic :: FabricPorts m => String -> m (Seq Bool)
inStdLogic nm = do
        pad <- input nm (StdLogic_ $ deepSeq $ D $ Pad (OVar 0 nm))
        case pad of
          StdLogic_ sq -> return sq
          _            -> fail "internal type error in inStdLogic"


inGeneric :: String -> Fabric Integer
inGeneric nm = do
        pad <- input nm (Generic_ $ error "Fix Generic")
        case pad of
          Generic_ g -> return g
          _          -> fail "internal type error in inGeneric"

inStdLogicVector :: forall x . (Size x) => String -> Fabric (Seq (Unsigned x))
inStdLogicVector nm = do
        pad <- input nm (StdLogicVector_ $ (deepSeq $ D $ Pad (OVar 0 nm) :: Seq (Unsigned x)))
        case pad of
          StdLogicVector_ sq -> return $ (unsigned) sq
          _                  -> fail "internal type error in inStdLogic"



outStdLogic :: String -> Seq Bool -> Fabric ()
outStdLogic nm seq_bool = output nm (StdLogic_ seq_bool)


outStdLogicVector
  :: Size x => String -> Seq (Unsigned x) -> Fabric ()
outStdLogicVector nm sq = output nm (StdLogicVector_ sq)

-------------------------------------------------------------------------------

-- | 'driving' chains two fabrics together, leting
-- the first one drive the second one. Note this
-- is not the same as '(>>)', which makes no
-- connections.

-- NOTES: I'm unsure about the shadowing of names here.
-- It will work, as long as inputs and output never
-- interset.
{-
infixr 5 `driving`

driving :: Fabric a -> Fabric b -> Fabric b
driving (Fabric f) (Fabric g) = Fabric $ \ ins ->
    let (_,f_in_names,f_outs) = f ins
        (b,g_in_names,g_outs) = g (f_outs ++ ins)
    in ( b
       , f_in_names ++ [ (nm,ty)
                       | (nm,ty) <- g_in_names
                       , nm `notElem` map fst f_outs ]
       , [ (nm,ty)
         | (nm,ty) <- f_outs
         , nm `notElem` map fst g_in_names
         ] ++ g_outs
       )

-}

-- 'runFabric'  runs a Fabric () with arguments, and gives a (structured) reply.
runFabric :: Fabric () -> [(String,Pad)] -> [(String,Pad)]
runFabric (Fabric f) args = result
        where ((),_arg_types,result) = f args

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-
fabric_example :: Fabric ()
fabric_example = do
        i0 <- inStdLogic "i0"
        i1 <- inStdLogic "i1"
        let (c,s) = halfAdder i0 i1
        outStdLogic "carry" c
        outStdLogic "sum" s
  where
          halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool,Seq Bool)
          halfAdder a b = (carry,sum_)
                where carry = and2 a b
                      sum_  = xor2 a b
-}
-------------------------------------------------------------------------------

backedges :: (MonadFix m) => (b -> m (a,b)) -> m a
backedges f = liftM fst $ mfix $ \ ~(_,b) -> f b


-- | Given a circuit (where the inputs/outputs support MakeFabric),
-- automatically generate a Fabric.
genFabric :: MakeFabric a => a -> Fabric ()
genFabric c = evalStateT (mkFabric c) (0,0)


-- | The FabricGen just wraps Fabric with some state for tracking the number of
-- inputs/outputs.
type FabricGen = StateT (Int,Int) Fabric


-- | Generate the next output name in the sequence.
newOutputName :: FabricGen String
newOutputName = do
  (i,o) <- get
  put (i,o+1)
  return $  "out" ++ show o

-- | Generate the next input name in the sequence.
newInputName :: FabricGen String
newInputName = do
  (i,o) <- get
  put (i,o+1)
  return $  "in" ++ show o

-- | Automatically generate the input/output declarations for a Lava function.
class MakeFabric a where
  -- | Construct the Fabric
  mkFabric :: a -> FabricGen ()

instance MakeFabric (Seq Bool) where
   mkFabric b = do
     nm <- newOutputName
     lift $ outStdLogic nm b

instance MakeFabric a =>  MakeFabric (Seq Bool -> a) where
   mkFabric f = do
     nm <- newInputName
     i <- lift $ inStdLogic nm
     mkFabric (f i)

instance Size x =>  MakeFabric (Seq (Unsigned x)) where
  mkFabric v = do
    nm <- newOutputName
    lift $ outStdLogicVector nm v

instance (Size x, MakeFabric a) =>  MakeFabric (Seq (Unsigned x) -> a) where
   mkFabric f = do
     nm <- newInputName
     i <- lift $ inStdLogicVector nm
     mkFabric (f i)

instance (MakeFabric a, MakeFabric b) => MakeFabric (a,b) where
  mkFabric (a,b) = do
    mkFabric a
    mkFabric b

