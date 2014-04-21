{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeOperators, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, DataKinds #-}
-- | This module provides abstractions for working with RAMs and ROMs.
module Language.KansasLava.Protocols.Memory where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Stream as Stream
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Internal

import Data.Sized.Fin
import Data.Sized.Matrix as M
import Control.Applicative hiding (empty)
import Data.Maybe  as Maybe
import Control.Monad

import GHC.TypeLits

import Prelude hiding (tail)

import Data.Word

------------------------------------------------------------------------------------
-- We store our memory as a Radix tree.
-- A Radix Tree / Memory can be updated
-- in a way that does not leave thunks.
newtype Memory a d = Memory (Radix (X d))

instance (Rep d, Rep a) => Show (Memory a d) where
        show tree =
                "Memory {\n" ++
                unlines [ showRep a ++ " -> " ++ showRep d
                        | (a,d) <- assocsM tree
                        ] ++
                "}"

emptyM :: Memory a d
emptyM = Memory empty

insertM :: (Rep a, Rep d) => X a -> X d -> Memory a d -> Memory a d
insertM addr dat (Memory m) =
        case getValidRepValue (toRep addr) of
          Nothing -> emptyM
          Just bs -> Memory $ insert bs dat m

lookupM :: (Rep a, Rep d) => X a -> Memory a d -> X d
lookupM addr (Memory m) =
        case getValidRepValue (toRep addr) of
          Nothing -> unknownX
          Just bs -> case find bs m of
                        Nothing -> unknownX
                        Just v -> v

assocsM :: (Rep a, Rep d) => Memory a d -> [(X a,X d)]
assocsM (Memory m) =
        [ (fromRep (RepValue (map Just bs)),a)
        | (bs,a) <- assocs m
        ]

------------------------------------------------------------------------------------

instance (Rep a, Rep d) => Rep (Memory a d) where
    type W (Memory a d) =  W a * W d
    data X (Memory a d) = XMemory !(Memory a d)

    optX (Just f) = XMemory $ f
    optX Nothing  = XMemory $ emptyM

    unX (XMemory f) = return f

    repType Witness = MatrixTy (2^repWidth (Witness :: Witness a)) (repType (Witness ::Witness d))

    toRep (XMemory m) = RepValue
                      $ concatMap (unRepValue . toRep)
                      $ map (flip lookupM m)
                      $ map fromRep
                      $ reps
       where
            reps = allReps (Witness :: Witness a)

    fromRep (RepValue xs) = XMemory $ foldr (uncurry insertM)
                                            emptyM
                                            [ (x,v)
                                            | (x,v) <- zip (fmap fromRep reps)
                                                           (fmap (fromRep . RepValue) $ unconcat xs)
                                            ]

	    where unconcat [] = []
		  unconcat ys = take w_a ys : unconcat (drop w_a ys)

                  w_a = typeWidth (repType (Witness :: Witness a))
                  reps = allReps (Witness :: Witness a)

    showRep (XMemory m) = show m

------------------------------------------------------------------------------------

-- | A Pipe combines an address, data, and an Enabled control line.
type Pipe a d = Enabled (a,d)

-- | A Memory takes in a sequence of addresses, and returns a sequence of data at that address.
--type Memory clk a d = Signal clk a -> Signal clk d

-- Does not work for two clocks, *YET*
-- call writeMemory
-- | Write the input pipe to memory, return a circuit that does reads.
writeMemory :: forall a d clk1 sig . (Clock clk1, sig ~ Signal clk1, Rep a, Rep d)
	=> sig (Pipe a d)
	-> sig (Memory a d)
writeMemory pipe = res
  where
	(wEn,pipe') = unpack pipe
	(addr,dat) = unpack pipe'

    	res :: Signal clk1 (Memory a d)
    	res = Signal (fmap XMemory mem) (D $ Port "o0" $ E entity)

	mem :: Stream (Memory a d)
	mem = Stream.fromList $ emptyM :
		[ case unX up of
		    Nothing           -> emptyM	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) -> insertM (pureX a) (pureX d) m
                | up  <- Stream.toList $ shallowXS pipe
		| m   <- Stream.toList mem
		]

    	entity :: Entity E
    	entity =
		Entity (Prim "write")
			[ ("o0",typeOfS res)]
			[ ("clk",ClkTy, Pad "clk")
   		        , ("rst",B,     Pad "rst")
			, ("wEn",typeOfS wEn,unD $ deepS wEn)
			, ("wAddr",typeOfS addr,unD $ deepS addr)
			, ("wData",typeOfS dat,unD $ deepS dat)
                        , ("element_count"
                          , GenericTy
                          --, --Generic (2 ^ fromIntegral ((repWidth (Witness :: Witness a)) :: Int))
                          , Generic (2 ^ ((repWidth (Witness :: Witness a)) :: Int))
                          )
			]

-- | Read a series of addresses. Respects the latency of Xilinx BRAMs.
syncRead :: forall a d sig clk . (Clock clk, sig ~ Signal clk, Rep a, Rep d)
	=> sig (Memory a d) -> sig a -> sig d
syncRead mem addr = delay (asyncRead mem addr)

-- | Read a memory asyncrounously. There is no clock here,
-- becase this is an intra-clock operation on the sig (a -> d).
-- Compare with '<*>' from applicative functors.

asyncRead :: forall a d sig clk . (sig ~ Signal clk, Rep a, Rep d)
	=> sig (Memory a d) -> sig a -> sig d
asyncRead a d = mustAssignSLV $ primXS2 fn "asyncRead" a d
   where fn (XMemory m) a0 = lookupM a0 m

-- | memoryToMatrix should be used with caution/simulation  only,
-- because this actually clones the memory to allow this to work,
-- generating lots of LUTs and BRAMS.
memoryToMatrix ::  (SingI a, Rep d, Clock clk, sig ~ Signal clk)
	=> sig (Memory (Fin a) d) -> sig (Vector a d)
memoryToMatrix mem = pack (forAll $ \ x -> asyncRead mem (pureS x))

-- | Apply a function to the Enabled input signal producing a Pipe.
enabledToPipe :: (Rep x, Rep y, Rep z, sig ~ Signal clk) => (forall j . Signal j x -> Signal j (y,z)) -> sig (Enabled x) -> sig (Pipe y z)
enabledToPipe f se = pack (en, f x)
   where (en,x) = unpack se


--------------------------------------------------

-- The order here has the function *last*, because it allows
-- for a idiomatic way of writing things
--
--  res = rom inp $ \ a -> ....
--
-- | Generate a read-only memory.
rom :: (Rep a, Rep b, Clock clk) => Signal clk a -> (a -> Maybe b) -> Signal clk b
rom inp fn = funMapXXX fn inp

-----------------------------------------------------------------------------------------------
-- Map Ops
{-
rom2 :: forall a d clk sig . (SingI a, Rep d, sig ~ Signal clk) => ((Fin a) -> X d) -> sig ((Fin a -> d))
rom2 fn  = mustAssignSLV
      $ Signal (pure (XFunction fn))
               (D $ rom)

	where tB = repType (Witness :: Witness d)
              tMB = MatrixTy (Prelude.length all_a_bitRep) tB


              undefB :: RepValue
              undefB = unknownRepValue (Witness :: Witness d)

	      all_a_bitRep :: [RepValue]
	      all_a_bitRep = allReps (Witness :: Witness (Fin a))

              rom = Port "o0" $ E $ Entity (Prim "rom")
                                           [("o0",tMB)]
                                           [("defs",RomTy (Prelude.length all_a_bitRep),Lits lits)]

              -- assumes in order table generation
	      lits :: [RepValue]
	      lits = [ case unX (fromRep w_a) of
				 Nothing -> undefB
				 Just a' -> toRep (fn a')
		    | w_a <- all_a_bitRep
		    ]
-}




-- Assumping that the domain is finite (beacause of Rep), and *small* (say, < ~256 values).

-- | Given a function over a finite domain, generate a ROM representing the
-- function. To make this feasible to implement, we assume that the domain is
-- small (< 2^8 values).
funMapXXX :: forall sig a b i . (sig ~ Signal i, Rep a, Rep b) => (a -> Maybe b) -> sig a -> sig b
funMapXXX fn (Signal a ae) = mustAssignSLV $ Signal (fmap fn' a)
                            (D $ Port ("o0")
			       $ E
			       $ Entity (Prim "asyncRead")
					         [("o0",tB)]
						 [ ("i0",tMB,rom)
						 , ("i1",tA,unD ae)
						 ])

	where tA = repType (Witness :: Witness a)
	      tB = repType (Witness :: Witness b)
              tMB = MatrixTy (Prelude.length all_a_bitRep) tB

              undefB = unknownRepValue (Witness :: Witness b)

              fn' a' = case unX a' of
			 Nothing -> optX Nothing
			 Just v -> optX (fn v)

	      all_a_bitRep :: [RepValue]
	      all_a_bitRep = allReps (Witness :: Witness a)

              rom = Port "o0" $ E $ Entity (Prim "rom") [("o0",tMB)] [("defs",RomTy (Prelude.length all_a_bitRep),Lits lits)]

              -- assumes in order table generation
	      lits :: [RepValue]
	      lits = [ case unX (fromRep w_a) of
				 Nothing -> undefB
				 Just a' -> case fn a' of
			                    Nothing -> undefB
			                    Just b -> toRep (pureX b)
		    | w_a <- all_a_bitRep
		    ]
