{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp  #-}

module Language.KansasLava.Test where
	
import Language.KansasLava.Entity
import Language.KansasLava.Type
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Utils
import Language.KansasLava.Signal

import qualified Data.List as List
import Data.Sized.Matrix as M
import Data.Sized.Unsigned as U

data Example a = Example a [ExampleArg]

data ExampleArg = forall a . (Show a, RepWire a) => ExampleArg (Seq a)

example :: a -> Example a
example a = Example a []

class TestArg a where
	testArg :: a -> [ExampleArg]

instance (Show a, RepWire a) => TestArg (Seq a) where
	testArg a = [ExampleArg a]

instance (Show a, RepWire a) => TestArg (Comb a) where
	testArg a = [ExampleArg (liftS0 a)]

infixl 2 .*.

(.*.) :: TestArg a => Example (a -> b) -> a -> Example b
(.*.) (Example f args) arg = Example (f arg) (args ++ testArg arg)

--test = example xor2 .*. low .*. high

class Testable a where
	truthTable :: a -> TT

data TTL = CombValue String TT
	 | SeqValue  String TTL
	 | ResV      String
	 | TupleTT   [TTL]		-- what about multiple results?
	deriving Show

newtype TT = TT { unTT :: [TTL] }


instance (RepWire a) => Testable (Seq a) where
	truthTable sq = TT [ ResV v | v <- showStreamList sq ]

instance (RepWire a) => Testable (Comb a) where
	truthTable c =  TT [ ResV $ showRepWire (undefined :: a) (combValue c)  ]

instance (Testable a, Testable b) => Testable (a,b) where
	truthTable (a,b) = TT [ TupleTT [x,y] | (x,y) <- zip (unTT $ truthTable a) (unTT $ truthTable b)]
instance (Testable a, Testable b, Testable c) => Testable (a,b,c) where
	truthTable (a,b,c) = TT [ TupleTT [x,y,z] | (x,y,z) <- zip3 (unTT $ truthTable a)
								  (unTT $ truthTable b) 
								  (unTT $ truthTable c)]

instance (Testable b) => Testable (Example b) where
	truthTable (Example fn (ExampleArg arg:args)) 
		= TT [ SeqValue s t | (s,t) <- zip ss tt ]
	   where
		ss = showStreamList arg
		tt = unTT $ truthTable (Example fn args)
	truthTable (Example fn [])
		= truthTable fn

instance (Enum (WIDTH w), Size (WIDTH w), RepWire w, Testable b) => Testable (Comb w -> b) where
	truthTable fn = TT
			[ CombValue (showRepWire (undefined :: w) a)
				    (truthTable (fn (shallowComb a)))
		 	| a <- args
		        ]
            where
		reps :: [Matrix (WIDTH w) Bool]
		reps = allWireReps
		args0 :: [Maybe w]
		args0 = [ toWireRep rep | rep <- allWireReps ]
		args = map optX (args0 ++ [Nothing])


-----------------------------------------------------------------------------------------------------
--- ASCII pretty printers
-- Other printers should live in a seperate package, I expect.
			
asciiTTL :: TTL -> [[String]]
asciiTTL (CombValue nm rest) = [ nm : rs | rss <- map asciiTTL (unTT rest), rs <- rss ]
asciiTTL (SeqValue nm lns)   = [ nm : rs | rs <- asciiTTL lns ]
asciiTTL (ResV str)          = [[ str ]]
asciiTTL (TupleTT ttls)	     = List.transpose [ tl | [tl] <- map asciiTTL ttls ]	
	-- TupleTT is a hack, and  we NEED to restrict the [xx] using types.

instance Show TT where
	show tt = showSomeTT 20 tt

showAllTT :: TT -> String
showAllTT (TT ttls) = unlines (formatTT maxBound ttls)

showSomeTT :: Int -> TT -> String
showSomeTT n (TT ttls) = unlines (take n $ ans) ++ if null (drop n ans) then "" else "...\n"
	where ans =  formatTT n ttls


-- | formatTT build an ASCII truth table from a list of TTL (truth table lines).
-- The argument is lookahead for layout.
formatTT :: Int -> [TTL] -> [String]
formatTT n ttls = 
	[ concat [ "  " ++ ljustify n r | (r,n) <- zip rs mx ] | rs <- rss ] 
    where
	ljustify n str = str ++ take (n - Prelude.length str) (repeat ' ')
	rss = concatMap asciiTTL ttls
	rss' = List.transpose $ take n rss
	mx = map maximum $ map (map Prelude.length) rss'

--- Examples of use

tt1 = truthTable (xor2 :: Comb Bool -> Comb Bool -> Comb Bool)
tt2 = truthTable (example (xor2 :: Seq Bool -> Seq Bool -> Seq Bool) 
			.*. in1
			.*. in2)
	where
		in1 = toSeq [True,False,True,False]	-- or high, or ...
		in2 = encSeq enc "H_HL"
		enc 'H' = return True
		enc 'L' = return False
		enc _   = fail "other"
tt3 = truthTable ((*) :: Comb U2 -> Comb U2 -> Comb U2)

tt4 = truthTable (halfAdder :: Comb Bool -> Comb Bool -> (Comb Bool,Comb Bool))
  where halfAdder a b = (xor2 a b, and2 a b)

tt5 = truthTable (example (delay :: Seq SysEnv -> Comb ALPHA -> Seq ALPHA -> Seq ALPHA) 
			.*. env
			.*. def
			.*. inp)
	where
		env = takeThenSeq 7 sysEnv env
		def = pureS $ ALPHA "~def~"
		inp = toSeq $ cycle $ map ALPHA ["A","B","C","D"]

------------------------------------------------------------------------
-- Current limitations
-- Needed curried values (No (Comb X,Comp Y) -> ...)	 [FIXABLE]
-- Only handles 2 tuples 				[FIXABLE]
--- ???



