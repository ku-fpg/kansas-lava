{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, GADTs, FlexibleContexts #-}
-- | A top-level module that re-exports the relevent parts of the library's internal modules.
module Language.KansasLava (
    -- * Basic types in Kansas Lava, from "Language.KansasLava.Types"
    module Language.KansasLava.Types,
    
    -- * Generating KLEG netlists, from "Language.KansasLava.Fabric"
    Fabric,
    reifyFabric,
    inStdLogic, inStdLogicVector,inGeneric,
    outStdLogic, outStdLogicVector,
    theClk, theRst, theClkEn,

    -- * The Comb type from "Language.KansasLava.Comb"
    Comb, 
    toComb, undefinedComb, fromComb,

    -- * The CSeq and Seq types from "Language.KansasLava.Seq"
    CSeq, Seq, 
    toSeq, toSeq', undefinedSeq, fromSeq,
    
    -- * Rendering KLEG as a Graph, from "Language.KansasLava.DOT"
    writeDotCircuit,

    -- * Optimizing KLEG, from "Language.KansasLava.Optimization"
    OptimizationOpts(..),
    optimizeCircuit,
    
    -- * Outputing VHDL, from "Language.KansasLava.VHDL"
    writeVhdlCircuit,

    
    module Language.KansasLava.Probes,
    module Language.KansasLava.Protocols,
    module Language.KansasLava.Rep,
    module Language.KansasLava.RTL,

    module Language.KansasLava.Signal,
    module Language.KansasLava.Test,
--    module Language.KansasLava.Types,
    module Language.KansasLava.Utils,


    -- until we track down the space leak
    module Language.KansasLava.Stream
     ) where

import Language.KansasLava.Comb
import Language.KansasLava.DOT
import Language.KansasLava.Fabric
import Language.KansasLava.Optimization
import Language.KansasLava.Probes
import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.RTL
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Stream
import Language.KansasLava.Types
import Language.KansasLava.Test
import Language.KansasLava.Utils
import Language.KansasLava.VHDL


