module Language.KansasLava.Simulate where


import Language.KansasLava.Seq
import Language.KansasLava.Entity
import Language.KansasLava.Stream as S
import Language.KansasLava.Wire

-- These are the functions for simulating circuits.


-- look at a seq signal.
peek :: (Rep a) => Seq a -> [Maybe a]
peek ~(Seq xs _) = map unX $ S.toList xs

-- A seq signal, that changes over time
with :: (Rep a) => String -> [a] -> Seq a
with sigName xs = poke sigName (map Just xs ++ repeat Nothing)

-- A seq signal with possible unknown values, that changes over time.
poke :: (Rep a) => String -> [Maybe a] -> Seq a
poke sigName xs = shallowSeq (S.fromList $ map optX xs) 

