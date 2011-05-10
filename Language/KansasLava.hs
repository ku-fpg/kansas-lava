{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, GADTs, FlexibleContexts #-}
-- | A top-level module that re-exports the library's internal modules.
module Language.KansasLava (
    module Language.KansasLava.Comb,
    module Language.KansasLava.Dynamic,
    module Language.KansasLava.Entity,
    module Language.KansasLava.Fabric,
    module Language.KansasLava.Optimization,
    module Language.KansasLava.Probes,
    module Language.KansasLava.Protocols,
    module Language.KansasLava.Rep,
    module Language.KansasLava.RTL,
    module Language.KansasLava.Seq,
    module Language.KansasLava.Signal,
    module Language.KansasLava.DOT,
    module Language.KansasLava.Test,
    module Language.KansasLava.Types,
    module Language.KansasLava.Utils,
    module Language.KansasLava.VHDL,

    -- until we track down the space leak
    module Language.KansasLava.Stream
     ) where

import Language.KansasLava.Comb
import Language.KansasLava.Dynamic
import Language.KansasLava.Entity
import Language.KansasLava.Fabric
import Language.KansasLava.Optimization
import Language.KansasLava.Probes
import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.RTL
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Stream
import Language.KansasLava.DOT
import Language.KansasLava.Types
import Language.KansasLava.Test
import Language.KansasLava.Utils
import Language.KansasLava.VHDL


