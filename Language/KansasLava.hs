{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, GADTs, FlexibleContexts #-}

module Language.KansasLava (
    module Language.KansasLava.Comb,
    module Language.KansasLava.Circuit,
    module Language.KansasLava.Deep,
    module Language.KansasLava.Shallow,
    module Language.KansasLava.Circuit.Optimization,
    module Language.KansasLava.Protocols,
    module Language.KansasLava.Protocols.FSL,
    module Language.KansasLava.Reify,
    module Language.KansasLava.RTL,
    module Language.KansasLava.Seq,
    module Language.KansasLava.Signal,
    module Language.KansasLava.Stream,
    module Language.KansasLava.Simulate,
    module Language.KansasLava.Types,
    module Language.KansasLava.Utils,
    module Language.KansasLava.VHDL,
    module Language.KansasLava.StdLogicVector,
    module Language.KansasLava.Shallow.FIFO,

    module Language.KansasLava.Dynamic,
    module Language.KansasLava.Testing.Output.Dot,
    module Language.KansasLava.HandShake
--    module Language.KansasLava.Applicative,
--    module Language.KansasLava.Memory
    -- hacks
--    examine, dumpBitTrace, Examine(..),

     ) where

import Language.KansasLava.Comb
import Language.KansasLava.Circuit
import Language.KansasLava.Deep
import Language.KansasLava.Circuit.Optimization
import Language.KansasLava.Protocols
import Language.KansasLava.Protocols.FSL
import Language.KansasLava.Reify
import Language.KansasLava.RTL
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Simulate
import Language.KansasLava.Stream hiding (head,tail,zipWith)
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.VHDL
import Language.KansasLava.Shallow
import Language.KansasLava.Testing.Output.Dot

import Language.KansasLava.StdLogicVector
import Language.KansasLava.Shallow.FIFO
import Language.KansasLava.HandShake


import Language.KansasLava.Dynamic

-- TODO Add StdLogicVector

-- Location of Temp Hacks

import System.Cmd
import System.Directory
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

{-
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
            let nm_n = nm ++ "_" ++ show n

            writeFile (path ++ "Makefile")
                $ unlines
                $ ["run : setup " ++ nm_n ++ ".info " ++ nm ++ ".do"
                  ,"\t@echo \"Simulating...\""
                  ,"\t@vsim -c -do " ++ nm ++ ".do"
                  ,"\t@echo \"10 lines from the info file...\""
                  ,"\t@tail " ++ nm_n ++ ".info"
                  ,"\t@echo \"The same 10 lines from the input file...\""
                  ,"\t@tail " ++ nm ++ ".input"
                  ,"\t@echo \"Ditto for the output file...\""
                  ,"\t@tail " ++ nm ++ ".output"
                  ,"\t@./test.sh"
                  ,""
                  ,"setup : " ++ nm_n ++ ".bits"
                  ,"\t@/bin/cp " ++ nm_n ++ ".bits " ++ nm ++ ".input"
                  ]
            writeFile (path ++ "test.sh")
                $ unlines
                $ ["#!/bin/bash"
                  ,"THEDIFF=`diff *.input *.output`"
                  ,""
                  ,"if [[ -z \"$THEDIFF\" ]]; then"
                  ,"    echo \"Input/Output Files Are The Same\""
                  ,"else"
                  ,"    echo \"$THEDIFF\""
                  ,"fi"
                  ]
            system $ "chmod +x " ++ path ++ "test.sh"
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
    createDirectoryIfMissing True path
    let loop n = do
        ok <- isEmptyChan eChan
        if ok then return () else do
            f <- readChan eChan
            f path n depth
            loop (succ n)
    loop 0
-}
