{-# LANGUAGE TypeFamilies #-}
-- | This module provides some basic support for co-opting an identity entity block as a box
-- containing some 'Dynamic' data. Any optimization pass
-- will assume these blocks are identities, and perhaps remove them.

module Language.KansasLava.Dynamic where

import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Types

import Data.Dynamic

-- | We use identity "black boxes" as arbitary tags in the syntax, for extentablity.

addDynamic :: (sig ~ CSeq i, Rep a) => Dynamic -> sig a -> sig a
addDynamic dyn = idD (BlackBox (Box dyn))

-- | Get any chain of (deep) black boxes on this signal.
getDynamics :: (sig ~ CSeq i, Rep a) => sig a -> [Dynamic]
getDynamics sig = find (unD $ seqDriver sig)
  where
	find :: Driver E -> [Dynamic]
	find (Port _ (E (Entity (BlackBox (Box bb)) _ ins))) =
			bb : case ins of
				[(_,_,i)] -> find i
                                _ -> error "getDynamics: no inputs"
	find _ = []
