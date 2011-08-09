module Language.KansasLava.Patch where
	
	
data Patch lhs_in lhs_out top_in bot_out rhs_out rhs_in
	= Patch { unPatch :: (lhs_in,top_in,rhs_in)
			  -> (lhs_out,bot_out,rhs_out)
		}
		
		
infixl >==>

(>==>) :: Patch li1 lo1 t1 b1 o i -> Patch o i t2 b2 ro2 ri2 -> Patch li1 lo1 (t1,t2) (b1,b2) ro2 ri2
(Patch p1) >==> (Patch p2) = Patch $ \ ~(lhs_in,~(top_in1,top_in2),rhs_in) ->
	let
		(lhs_out1,bot_out1,rhs_out1) = p1 (lhs_in,top_in1,lhs_out2)
		(lhs_out2,bot_out2,rhs_out2) = p2 (rhs_out1,top_in2,rhs_in)
	in 
		(lhs_out1,(bot_out1,bot_out2),rhs_out2)
		
mapTop :: (b -> a) -> Patch lhs_in lhs_out a bot_out rhs_out rhs_in
		  -> Patch lhs_in lhs_out b bot_out rhs_out rhs_in
mapTop f (Patch p) = Patch $ \ ~(lhs_in,top_in,rhs_in) -> p (lhs_in,f top_in,rhs_in)


mapBot :: (a -> b) -> Patch lhs_in lhs_out top_in a rhs_out rhs_in
 		   -> Patch lhs_in lhs_out top_in b rhs_out rhs_in
mapBot f (Patch p) = Patch $ \ inp -> let (lhs_out,bot_out,rhs_out) = p inp
				      in (lhs_out,f bot_out,rhs_out)


class Unit u where
    unit :: u

instance Unit () where unit = ()
instance (Unit a,Unit b) => Unit (a,b) where unit = (unit,unit)
