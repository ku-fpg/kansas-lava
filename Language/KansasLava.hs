{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}

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

{-# NOINLINE eChan #-}
eChan :: Chan (String -> Int -> Int -> IO ())
eChan = unsafePerformIO $ newChan

{-# NOINLINE examine #-}
examine :: forall a b . (RepWire a, RepWire b) => String -> (Seq a -> Seq b) -> (Seq a -> Seq b)
examine nm fn arg = unsafePerformIO $ do
	let res = fn arg
	writeChan eChan (\ path n depth -> writeBitfilePair (path  ++ nm ++ "_" ++ show n) depth (pack (arg,res) :: Seq (a,b)))
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

-- pretty pointless without examine. Could be inlined.
writeBitfilePair :: forall a b . (RepWire a, RepWire b) => String -> Int -> Seq (a,b) -> IO ()
writeBitfilePair nm count fn = do
	let (inp,outp) = unpack fn
	writeBitfile (nm ++ ".inp.bits") count inp
	writeBitfile (nm ++ ".out.bits") count outp
