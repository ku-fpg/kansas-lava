{-# LANGUAGE ExistentialQuantification, TypeFamilies, ParallelListComp, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs
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
        ) where


import Control.Monad.Fix
import Control.Monad
import Data.Sized.Ix
--import Data.Sized.Matrix

import Language.KansasLava.Types
import Language.KansasLava.Seq
import Language.KansasLava.Utils
import Language.KansasLava.Shallow



-- The '_' will disappear soon from these names.

data Pad = StdLogic (Seq Bool)
         | forall a x . (Size (W a), Show a, Rep a) 
                => StdLogicVector (Seq a)
--         | TypedPad (...)
         | GenericPad Integer

padStdLogicType :: Pad -> StdLogicType
padStdLogicType (StdLogic _)       = SL
padStdLogicType (StdLogicVector s) = SLV $ size (untype s)
    where untype :: (Size (W a)) => Seq a -> W a
          untype = error "untype"
padStdLogicType (GenericPad _)        = G

instance Show Pad where
        show (StdLogic sq)       = "StdLogic " ++ show sq
        show (StdLogicVector sq) = "StdLogicVector " ++ show sq
        show (GenericPad i)      = "Generic " ++ show i

         -- TODO: the 2D Array

{- | The 'Fabric' structure, which is also a monad.

> fabric_example :: Fabric ()
> fabric_example = do
>        i0 <- inStdLogic "i0"
>        i1 <- inStdLogic "i1"
>        let (c,s) = halfAdder i0 i1
>        outStdLogic "carry" c
>        outStdLogic "sum" s
>  where
>          halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool,Seq Bool)
>          halfAdder a b = (carry,sum_)
>                where carry = and2 a b
>                      sum_  = xor2 a b

-}

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

inStdLogic :: String -> Fabric (Seq Bool)
inStdLogic nm = do
        pad <- input nm (StdLogic $ deepSeq $ D $ Pad (OVar 0 nm))
        case pad of
          StdLogic sq -> return sq
          _            -> fail "internal type error in inStdLogic"


inGeneric :: String -> Fabric Integer
inGeneric nm = do
        pad <- input nm (GenericPad $ error "Fix Generic")
        case pad of
          GenericPad g -> return g
          _            -> fail "internal type error in inGeneric"

inStdLogicVector :: forall a . (Rep a, Show a, Size (W a)) => String -> Fabric (Seq a)
inStdLogicVector nm = do
        pad <- input nm (StdLogicVector $ (deepSeq $ D $ Pad (OVar 0 nm) :: Seq a))
        case pad of
                                -- This unsigned is hack, but the sizes should always match.
          StdLogicVector sq -> return $ (unsafeCoerce sq)
          _                  -> fail "internal type error in inStdLogic"

outStdLogic :: String -> Seq Bool -> Fabric ()
outStdLogic nm seq_bool = output nm (StdLogic seq_bool)

outStdLogicVector
  :: forall a .
     (Rep a, Show a, Size (W a)) => String -> Seq a -> Fabric ()
outStdLogicVector nm sq = output nm (StdLogicVector sq)

-------------------------------------------------------------------------------


-- 'runFabric'  runs a Fabric () with arguments, and gives a (structured) reply.
runFabric :: Fabric () -> [(String,Pad)] -> [(String,Pad)]
runFabric (Fabric f) args = result
        where ((),_arg_types,result) = f args

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
