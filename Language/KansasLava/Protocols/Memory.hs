{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances #-}
-- | This module provides abstractions for working with RAMs and ROMs.
module Language.KansasLava.Protocols.Memory where

import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Stream as Stream
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Internal

import Data.Sized.Matrix as M
import Control.Applicative hiding (empty)
import Data.Maybe  as Maybe
import Control.Monad

import Prelude hiding (tail)

-- | A Pipe combines an address, data, and an Enabled control line.
type Pipe a d = Enabled (a,d)

-- | A Memory takes in a sequence of addresses, and returns a sequence of data at that address.
type Memory clk a d = Signal clk a -> Signal clk d

-- Does not work for two clocks, *YET*
-- call writeMemory
-- | Write the input pipe to memory, return a circuit that does reads.
writeMemory :: forall a d clk1 sig . (Clock clk1, sig ~ Signal clk1, Size a, Rep a, Rep d)
	=> sig (Pipe a d)
	-> sig (a -> d)
writeMemory pipe = res
  where
	-- Adding a 1 cycle delay, to keep the Xilinx tools happy and working.
	-- TODO: figure this out, and fix it properly
	(wEn,pipe') = unpack  {- register (pureS Nothing) $ -} pipe
	(addr,dat) = unpack pipe'

    	res :: Signal clk1 (a -> d)
    	res = Signal shallowRes (D $ Port "o0" $ E entity)

	shallowRes :: Stream (X (a -> d))
        shallowRes = pure (\ m -> XFunction $ \ ix ->
                        case getValidRepValue (toRep (optX (Just ix))) of
                               Nothing -> optX Nothing
                               Just a' -> case find a' m of
                                            Nothing -> optX Nothing
                                            Just v -> optX (Just v)
                          )
			<*> mem -- (emptyMEM :~ mem)
--			    <*> ({- optX Nothing :~ -} shallowS addr2)

	-- This could have more fidelity, and allow you
	-- to say only a single location is undefined
	updates :: Stream (Maybe (Maybe (a,d)))
	updates = id
--	        $ observeStream "updates"
	        $ stepifyStream (\ a -> case a of
					Nothing -> ()
					Just b -> case b of
						   Nothing -> ()
						   Just (c,d) -> eval c `seq` eval d `seq` ()
			        )
		$ pure (\ e a b ->
			   do en'   <- unX e
			      if not en'
				     then return Nothing
				     else do
			      		addr' <- unX a
			      		dat'  <- unX b
			      		return $ Just (addr',dat')
		       ) <*> shallowS wEn
			 <*> shallowS addr
			 <*> shallowS dat

	mem :: Stream (Radix d)
	mem = id
--	    $ observeStream "mem"
	    $ stepifyStream (\ a -> a `seq` ())
	    $ Cons empty $ Just $ Stream.fromList
		[ case u of
		    Nothing           -> empty	-- unknown again
		    Just Nothing      -> m
		    Just (Just (a,d)) ->
			case getValidRepValue (toRep (optX (Just a))) of
			  Just bs -> ((insert $! bs) $! d) $! m
                          Nothing -> error "mem: can't get a valid rep value"
		| u <- Stream.toList updates
		| m <- Stream.toList mem
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
                          , Generic (fromIntegral (M.size (error "witness" :: a)))
                          )
			]
{-
readMemory :: forall a d sig clk . (Clock clk, sig ~ Signal clk, Size a, Rep a, Rep d)
	=> sig (a -> d) -> sig a -> sig d
readMemory mem addr = unpack mem addr
-}

-- | Read a series of addresses. Respects the latency of Xilinx BRAMs.
syncRead :: forall a d sig clk . (Clock clk, sig ~ Signal clk, Size a, Rep a, Rep d)
	=> sig (a -> d) -> sig a -> sig d
syncRead mem addr = delay (asyncRead mem addr)

-- | Read a memory asyncrounously. There is no clock here,
-- becase this is an intra-clock operation on the sig (a -> d).
-- Compare with '<*>' from applicative functors.

asyncRead :: forall a d sig clk . (sig ~ Signal clk, Size a, Rep a, Rep d)
	=> sig (a -> d) -> sig a -> sig d
asyncRead a d = mustAssignSLV $ primXS2 fn "asyncRead" a d
   where fn (XFunction f) a0 = 
           -- We need to case of XFunction, rather than use unX,
           -- because the function may not be total.
           case unX a0 of
                Just a' -> f a'
                Nothing -> optX Nothing


-- | memoryToMatrix should be used with caution/simulation  only,
-- because this actually clones the memory to allow this to work,
-- generating lots of LUTs and BRAMS.
memoryToMatrix ::  (Integral a, Size a, Rep a, Rep d, Clock clk, sig ~ Signal clk)
	=> sig (a -> d) -> sig (Matrix a d)
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
rom inp fn = delay $ funMap fn inp

---------------------------------
