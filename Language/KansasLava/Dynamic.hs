-- | This module provides some basic support for co-opting an identity entity block as a box
-- containing some 'Dynamic' data. Any optimization pass
-- will assume these blocks are identities, and perhaps remove them.

module Language.KansasLava.Dynamic where

import Language.KansasLava.Comb
import Language.KansasLava.Entity
import Language.KansasLava.Shallow
import Language.KansasLava.Signal
import Language.KansasLava.Types

import Data.Dynamic

-- | We use identity "black boxes" as arbitary tags in the syntax, for extentablity.

addDynamic :: (Signal sig, Rep a) => Dynamic -> sig a -> sig a
addDynamic bb = liftS1 $ \ (Comb a ae) -> Comb a (entity1 (BlackBox $ Box bb) $ ae)

-- | Get any chain of (deep) black boxes on this signal.
getDynamics :: (Signal sig) => sig a -> [Dynamic]
getDynamics sig = find (unD $ deepS sig)
  where
	find :: Driver E -> [Dynamic]
	find (Port _ (E (Entity (BlackBox (Box bb)) _ ins))) =
			bb : case ins of
				[(_,_,i)] -> find i
                                _ -> error "getDynamics: no inputs"
	find _ = []
