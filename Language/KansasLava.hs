{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, GADTs, FlexibleContexts #-}

module Language.KansasLava (
    module Language.KansasLava.Dot,
    module Language.KansasLava.Comb,
    module Language.KansasLava.Circuit,
    module Language.KansasLava.Entity,
    module Language.KansasLava.Entity.Utils,
    module Language.KansasLava.Opt,
    module Language.KansasLava.Protocols,
    module Language.KansasLava.Reify,
    module Language.KansasLava.Seq,
    module Language.KansasLava.Signal,
    module Language.KansasLava.Stream,
    module Language.KansasLava.Simulate,
    module Language.KansasLava.Test,
    module Language.KansasLava.Type,
    module Language.KansasLava.Utils,
    module Language.KansasLava.VHDL,
    module Language.KansasLava.Wire,
--    module Language.KansasLava.Applicative,
--    module Language.KansasLava.Memory
	-- hacks
	examine, dumpBitTrace
	
     ) where

import Language.KansasLava.Dot
import Language.KansasLava.Comb
import Language.KansasLava.Circuit
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Opt
import Language.KansasLava.Protocols
import Language.KansasLava.Reify
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Simulate
import Language.KansasLava.Stream hiding (head,tail,zipWith)
import Language.KansasLava.Test
import Language.KansasLava.Type
import Language.KansasLava.Utils
import Language.KansasLava.VHDL
import Language.KansasLava.Wire



-- Location of Temp Hacks

import System.IO.Unsafe
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Sized.Arith
import Data.Sized.Ix

{-# NOINLINE eChan #-}
eChan :: Chan (String -> Int -> Int -> IO ())
eChan = unsafePerformIO $ newChan
{-
	examine :: (RepWire a) => String -> (Seq a -> b) -> (Seq a -> b)

instance (RepWire a) => Examine (Seq a) where
	examine = examine1

instance (t ~ (ADD (WIDTH a) (WIDTH b)), Size t, Enum t, RepWire a, RepWire b) => Examine (Seq b -> Seq c) where
	examine = examine2

examine2 :: forall a b c t . (t ~ (ADD (WIDTH a) (WIDTH b)), Size t, Enum t, RepWire a, RepWire b, RepWire c) 
	=> String -> (Seq a -> Seq b -> Seq c) -> (Seq a -> Seq b -> Seq c)
examine2 nm fn a1 a2 = examine1 nm fn' (pack (a1,a2))
   where fn' :: Seq (a,b) -> Seq c
	 fn' v = fn a b
		where (a,b) = unpack v

class Examine a where
	examine = examine0
-}

-- TODO: Combine with the Ports stuff
class Examine a where
	examine' :: String -> [IsRepWire] -> a -> a
	
instance (RepWire a) => Examine (Seq a) where
	examine' = examine0

instance (RepWire a, Examine b) => Examine (Seq a -> b) where
	examine' nm args f a = examine' nm (args ++ [IsRepWire a]) (f a)
	
instance (RepWire a, Examine b) => Examine (Comb a -> b) where
	examine' nm args f a = examine' nm (args ++ [IsRepWire $ liftS0 a]) (f a)	
		
-- If you are reifying things, then this should not be used. It does no harm,
-- but will generate failing 'examine' traces.

examine :: Examine a => String -> a -> a
examine nm a = examine' nm [] a

{-# NOINLINE examine0 #-}
examine0 :: forall a b . (RepWire b) => String -> [IsRepWire] -> Seq b -> Seq b
examine0 nm args res = unsafePerformIO $ do
	writeChan eChan $ \ path n count -> do
			writeFile (path ++ nm ++ "_" ++ show n ++ ".bits")
				$ unlines
				$ take count
				$ showBitfile
				$  (args ++ [IsRepWire res])
			writeFile (path ++ nm ++ "_" ++ show n ++ ".info")
				$ unlines
				$ take count
				$ showBitfileInfo
				$ (args ++ [IsRepWire res])
	return $ res

dumpBitTrace :: String -> Int -> IO ()
dumpBitTrace path depth = do
	let loop n = do
		ok <- isEmptyChan eChan
		if ok then return () else do
			f <- readChan eChan
			f path n depth
			loop (succ n)
	loop 0
