module Language.KansasLava.Dynamic where
	
import Language.KansasLava.Types
import Language.KansasLava.Signal
import Language.KansasLava.Comb
import Language.KansasLava.Shallow
import Language.KansasLava.Deep
import Data.Dynamic

-- We use identity "black boxes" as arbitary tags in the syntax, 
-- for extentablity.

addDynamic :: (Signal sig, Rep a) => Dynamic -> sig a -> sig a
addDynamic bb = liftS1 $ \ (Comb a ae) -> Comb a (entity1 (BlackBox $ Box bb) $ ae)

-- Get any chain of black boxes on this 
getDynamics :: (Signal sig) => sig b -> [Dynamic]
getDynamics sig = find (unD $ deepS sig)
  where
	find :: Driver E -> [Dynamic]
	find (Port _ (E (Entity (BlackBox (Box bb)) _ ins _))) = 
			bb : case ins of
				[(_,_,i)] -> find i
	find _ = []
