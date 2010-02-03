{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies #-}

module Language.KansasLava.Blocks where
	
import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Seq
import Language.KansasLava.E
import Language.KansasLava.Wire
import Language.KansasLava.K

import Control.Applicative

and2 :: (SIGNAL sig) => sig Bool -> sig Bool -> sig Bool
and2 = liftS2 $ \ (K a ae) (K b be) -> K (liftA2 (&&) a b) 
	      		$ 
	

-- o0 :: 
