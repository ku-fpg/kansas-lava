{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables
 #-}
module Language.KansasLava.Fabric 
        ( Fabric(..)
        , Pad(..)
        , inStdLogic
        , inGeneric
        , outStdLogic
        , fabric_example
        , backedges
        ) where


import Language.KansasLava.Types
import Language.KansasLava.Seq
import Data.Sized.Unsigned
import Data.Sized.Ix
import Control.Monad.Fix
import Control.Monad

-- For testing
import Language.KansasLava.Utils

-- The '_' will disappear soon from these names.

data Pad = StdLogic_ (Seq Bool)
         | forall x . (Size x) => StdLogicVector_ (Seq (Unsigned x))
         | Generic_ Integer
         -- TODO: the 2D Array

-- The 'Fabric' structure, which is also a monad.
data Fabric a = Fabric { unFabric :: [(String,Pad)] -> (a,[(String,StdLogicType)],[(String,Pad)]) }

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
        


input :: String -> StdLogicType -> Fabric Pad
input nm ty = Fabric $ \ ins ->
        let p = case lookup nm ins of
                   Just v -> v
                   _ -> error $ "input internal error finding : " ++ show nm
        in (p,[(nm,ty)],[])

inStdLogic :: String -> Fabric (Seq Bool)
inStdLogic nm = do
        pad <- input nm SL
        case pad of
          StdLogic_ sq -> return sq
          _            -> fail "internal type error in inStdLogic"
          
inGeneric :: String -> Fabric Integer
inGeneric nm = do
        pad <- input nm SL
        case pad of
          Generic_ g -> return g
          _          -> fail "internal type error in inGeneric"


output :: String -> Pad -> Fabric ()
output nm pad = Fabric $ \ _ins -> ((),[],[(nm,pad)])

outStdLogic :: String -> Seq Bool -> Fabric ()
outStdLogic nm seq_bool = output nm (StdLogic_ seq_bool)

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

-------------------------------------------------------------------------------

backedges :: (MonadFix m) => (b -> m (a,b)) -> m a
backedges f = liftM fst $ mfix $ \ ~(_,b) -> f b
