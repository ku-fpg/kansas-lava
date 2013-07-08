{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, GADTs, FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
-- | A top-level module that re-exports the relevent parts of the library's internal modules.
module Language.KansasLava (
    -- * Basic types in Kansas Lava
    module Language.KansasLava.Types,

    -- * Generating KLEG netlists
    SuperFabric,
    Fabric,
    reifyFabric,
    inStdLogic, inStdLogicVector, -- inGeneric,
    outStdLogic, outStdLogicVector,
    theClk, theRst, theClkEn,
    hWriterFabric, IN(..),
    hReaderFabric, OUT(..),
--    runFabricWithDriver,

    -- * The CSeq and Seq types
    Signal, Seq,
    toS, toS', undefinedS, fromS, takeS,
    pureS, witnessS,
    commentS,
    readIOS,
    writeIOS,
    pack, unpack,
    packMatrix, unpackMatrix,
    register, registers, delay, delays,

    -- * Optimizing KLEG
    OptimizationOpts(..),
    optimizeCircuit,

    -- * Outputing VHDL
    writeVhdlCircuit,
    writeVhdlPrelude,

    -- * RTL sub-DSL
    module Language.KansasLava.Spark,

    -- * Probes
    module Language.KansasLava.Probes,

    -- * Protocols
    module Language.KansasLava.Protocols,

    -- * Rep
    module Language.KansasLava.Rep,

    -- * Kansas Lava User-level Utils
    module Language.KansasLava.Utils,

    -- * Version Change Dump
--    VCD,
--    writeVCDFile,
--    readVCDFile

     ) where

import Language.KansasLava.Fabric
import Language.KansasLava.Optimization
import Language.KansasLava.Probes
import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.VHDL
import Language.KansasLava.Spark




