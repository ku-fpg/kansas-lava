{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Main where

import Language.KansasLava

import Data.Default


import qualified Matrix
import qualified Memory
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
        testDriver opt 
                [ Matrix.tests
                , Memory.tests
                , Coerce.tests 
                , Others.tests
                ]


