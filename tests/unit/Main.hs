{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Main where

import Language.KansasLava

import Data.Default

import System.Cmd
import Trace.Hpc.Reflect
import Trace.Hpc.Tix

import Types
import Report
import Utils

import qualified Matrix
import qualified Memory
import qualified FIFO
import qualified Coerce
import qualified Others

main :: IO ()
main = do
        let opt = def { verboseOpt = 4  -- 4 == show cases that failed
                      , genSim = True
                      , simMods = [("default_opts", (optimizeCircuit def))]
--                      , testOnly = return ["negate"]
                      , testNever = ["max","min","abs","signum"] -- for now
                      , testData = 1000
                      }

        putStrLn "Running with the following options:"
        putStrLn $ show opt

        prepareSimDirectory opt

        let test :: TestSeq
            test = TestSeq (testFabrics opt)
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
                    then do _ <- system $ simCmd opt
                            generateReport $ simPath opt
                    else do putStrLn $ unlines [""
                                               ,"Run simulations and generate reports using the Makefile commands"
                                               ,"or the individual Makefiles in each simulation subdirectory."
                                               ,""]
            else generateReport $ simPath opt

        -- Coverage Count

        Tix tix <- examineTix
        let counts = concat [ xs | TixModule _ _ _ xs <- tix ]
        let len = length counts
        let txs = length $ filter (> 0) counts
        putStrLn $ "Raw coverage: " ++ show (floor (100 * fromIntegral txs / fromIntegral len :: Double) :: Int) ++ "%"

