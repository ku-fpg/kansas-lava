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
    toSeq, toSeq', undefinedSeq, fromSeq,
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

    -- * RTL sub-DSL
    module Language.KansasLava.RTL,


    module Language.KansasLava.Probes,
    module Language.KansasLava.Protocols,
    module Language.KansasLava.Rep,
    module Language.KansasLava.Test,
    module Language.KansasLava.Utils

     ) where

import Language.KansasLava.DOT
import Language.KansasLava.Fabric
import Language.KansasLava.Optimization
import Language.KansasLava.Probes
import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.RTL
import Language.KansasLava.Seq
import Language.KansasLava.Types
import Language.KansasLava.Test
import Language.KansasLava.Utils
import Language.KansasLava.VHDL


