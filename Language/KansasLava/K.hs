module Language.KansasLava.K where
	
import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Seq

-- an obserable, konstant(sic.) value.
data K a = K a (Driver E)
	deriving Show

newtype E = E (Entity BaseTy E)
	deriving Show

and2K :: K Bool -> K Bool -> K Bool
and2K (K a1 e1) 
      (K a2 e2) = K (a1 && a2)
		  $ Port (Var "o0")
 		  $ E
		  $ Entity (Name "K" "and") 
			    [(Var "o0",B)] 
			    [(Var "o0",B,e1),(Var "o1",B,e2)]
			    []

true :: K Bool
true = K True (Port (Var "o0") $ E $ Entity (Name "K" "True") [(Var "o0",B)] [] [])

false :: K Bool
false = K False (Port (Var "o0") $ E $ Entity (Name "K" "False") [(Var "o0",B)] [] [])

data Signal a = Signal (Seq a) (Driver E)

liftK2 :: (K a -> K b) -> Signal a -> Signal b
liftK2 = undefined

