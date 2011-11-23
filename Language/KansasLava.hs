{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, GADTs, FlexibleContexts #-}
-- | A top-level module that re-exports the relevent parts of the library's internal modules.
module Language.KansasLava (
    -- * Basic types in Kansas Lava
    module Language.KansasLava.Types,
    
    -- * Generating KLEG netlists
    Fabric,
    reifyFabric,
    inStdLogic, inStdLogicVector,inGeneric,
    outStdLogic, outStdLogicVector,
    theClk, theRst, theClkEn,

    -- * The CSeq and Seq types
    Signal, Seq, 
    toS, toS', undefinedS, fromS, takeS,
    pureS, witnessS,
    commentS,
    pack, unpack,
    packMatrix, unpackMatrix,
    register, registers, delay, delays,
    
    -- * Rendering KLEG as a Graph
    writeDotCircuit,

    -- * Optimizing KLEG
    OptimizationOpts(..),
    optimizeCircuit,
    
    -- * Outputing VHDL
    writeVhdlCircuit,
    writeVhdlPrelude,

--    -- * RTL sub-DSL
--    module Language.KansasLava.RTL,

    -- * Probes
    module Language.KansasLava.Probes,

    -- * Protocols
    module Language.KansasLava.Protocols,

    -- * Rep
    module Language.KansasLava.Rep,

    -- * Kansas Lava User-level Utils
    module Language.KansasLava.Utils,

    -- * Version Change Dump
    VCD,
    writeVCDFile,
    readVCDFile,
    
    -- * Wakarusa Compiler
    module Language.KansasLava.Wakarusa
     ) where

import Language.KansasLava.DOT
import Language.KansasLava.Fabric
import Language.KansasLava.Optimization
import Language.KansasLava.Probes
import Language.KansasLava.Protocols
import Language.KansasLava.Rep
--import Language.KansasLava.RTL
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.VCD
import Language.KansasLava.VHDL
import Language.KansasLava.Wakarusa


