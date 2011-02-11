{-# LANGUAGE ScopedTypeVariables #-}
module Language.KansasLava.Testing.Observe where

import Language.KansasLava.Types
import Language.KansasLava.Shallow
import Language.KansasLava.Seq
import Language.KansasLava.Stream
import Debug.Trace

observeAloud :: forall a. (Rep a) => String -> Seq a -> Seq a
observeAloud = observeRep

observeRep :: forall a. (Rep a) => String -> Seq a -> Seq a
observeRep msg = dual shallow id
  where shallow
	  = shallowSeq
	  . foldr (\ (i,x) xs -> trace (msg ++ "(" ++ show i ++ ")" ++ showRep (Witness :: Witness a) x) $ x :~ xs) (error "never done")
	  . zip [(0::Int)..]
	  . fromSeqX
