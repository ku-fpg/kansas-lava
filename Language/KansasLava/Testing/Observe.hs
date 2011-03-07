{-# LANGUAGE ScopedTypeVariables #-}
-- | Trace the execution of the shallow embedding.
module Language.KansasLava.Testing.Observe
  (observeRep) where

import Language.KansasLava.Types
import Language.KansasLava.Shallow
import Language.KansasLava.Seq
import Language.KansasLava.Comb

--import Data.Stream hiding (zip)
import Language.KansasLava.Stream
import Debug.Trace

-- | Trace the values of the shallow stream.
observeRep :: forall a. (Rep a) => String -> Seq a -> Seq a
observeRep msg = dual shallow id
  where shallow
	  = shallowSeq
	  . foldr (\ (i,x) xs -> trace (msg ++ "(" ++ show i ++ ")" ++ showRep (Witness :: Witness a) x) $ Cons x xs) (error "never done")
	  . zip [(0::Int)..]
	  . fromSeqX


-- | Select the shallow embedding from one circuit, and the deep embedding from another.
class Dual a where
  -- | Take the shallow value from the first argument, and the deep value from the second.
  dual :: a -> a -> a

instance Dual (CSeq c a) where
  dual ~(Seq a _) ~(Seq _ eb) = Seq a eb

instance Dual (Comb a) where
  -- dual ~(Comb a _) ~(Comb _ eb) = Comb a eb
  dual c d = Comb (combValue c) (combDriver d)

instance (Dual a, Dual b) => Dual (a,b) where
	dual ~(a1,b1) ~(a2,b2) = (dual a1 a2,dual b1 b2)

instance (Dual a, Dual b,Dual c) => Dual (a,b,c) where
	dual ~(a1,b1,c1) ~(a2,b2,c2) = (dual a1 a2,dual b1 b2,dual c1 c2)

instance (Dual b) => Dual (a -> b) where
	dual f1 f2 x = dual (f1 x) (f2 x)
