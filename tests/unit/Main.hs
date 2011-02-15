{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Main where

import Language.KansasLava
import Language.KansasLava.Stream as S
import Language.KansasLava.Testing.Thunk

import Data.Bits
import Data.Default
import Data.List ( sortBy, sort )
import Data.Ord ( comparing )
import Data.Maybe as Maybe
import Data.Sized.Arith
import Data.Sized.Ix
import qualified Data.Sized.Matrix as M
import Data.Sized.Sampled
import Data.Sized.Signed
import Data.Sized.Unsigned
import Debug.Trace

import Control.Applicative
import Control.Concurrent.MVar
import System.Cmd
import System.FilePath
import Trace.Hpc.Reflect
import Trace.Hpc.Tix

import Types
import Report hiding (main)
import Utils

import qualified Matrix
import qualified Memory
import qualified FIFO
import qualified Coerce
import qualified Others


main = do
        let opt = def { verboseOpt = 4  -- 4 == show cases that failed
                      , genSim = True
                      , simMods = [("default_opts", (optimizeCircuit def))]
--                      , testOnly = return ["memory/async/rom/"]
                      , testNever = ["max","min","abs","signum"] -- for now
                      , testData = 1000
                      }

        putStrLn "Running with the following options:"
        putStrLn $ show opt

        prepareSimDirectory opt

        let test :: TestSeq
            test = TestSeq (testSeq opt)
                           (take (testData opt) . genToRandom)

        -- The different tests to run (from different modules)
        sequence_ [ t test
                  | t <- [ Matrix.tests
                         , Memory.tests
                         , FIFO.tests 
                         , Coerce.tests
                         , Others.tests
                         ]
                  ]

        -- If we didn't generate simulations, make a report for the shallow results.
        if genSim opt
            then if runSim opt
                    then do system $ simCmd opt
                            generateReport $ simPath opt
                    else do putStrLn $ unlines [""
                                               ,"Run simulations by using the " ++ simPath opt </> "runsims script"
                                               ,"or the individual Makefiles in each simulation subdirectory."
                                               ,"Then generate the report using main in Report.hs"
                                               ,""]
            else generateReport $ simPath opt

        -- Coverage Count

        Tix tix <- examineTix
        let counts = concat [ xs | TixModule _ _ _ xs <- tix ]
        let len = length counts
        let txs = length $ filter (> 0) counts
        putStrLn $ "Raw coverage: " ++ show (floor (100 * fromIntegral txs / fromIntegral len)) ++ "%"

