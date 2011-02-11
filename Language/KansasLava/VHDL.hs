{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.VHDL(writeVhdlCircuit, preprocessVhdlCircuit) where


import Language.KansasLava.Types

import Language.KansasLava.Netlist
import Data.Reify
import Language.KansasLava.Netlist.Utils
import Language.Netlist.GenVHDL
import Data.List

-- | The 'vhdlCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
{-
vhdlCircuit' :: (Ports o)
---               [CircuitOptions] -- ^ Options for controlling the observable-sharing reification.
            -> String         -- ^ The name of the generated entity.
	    -> [String]	      -- ^ The extra module arguments needed
            -> o              -- ^ The Lava circuit.
            -> IO String
vhdlCircuit' nlOpts name mods circuit = do
  mod <- netlistCircuit [] nlOpts name circuit
  return $ genVHDL mod mods
-}

writeVhdlCircuit :: [String] -> String -> FilePath -> Circuit -> IO ()
writeVhdlCircuit mods nm file cir = do
        let cir' = preprocessVhdlCircuit cir
	nlMod <- netlistCircuit nm cir'
	writeFile file (genVHDL nlMod mods)


-- This gets a circuit ready for VHDL generation.
-- Specifically, it normalizes all the arguments
-- because arguments that are of type MatrixTy are now supported.

preprocessVhdlCircuit :: Circuit -> Circuit
preprocessVhdlCircuit cir =
--        trace (show ("preprocessVhdlCircuit",res))
        res
    where
        Circuit nodes srcs sinks = cir
        res = Circuit nodes' srcs2 sinks2

        vars = allocEntities cir
        (sinkVars,srcVars)  = (take (length sinks) vars,drop (length sinks) vars)

        nodes'  = map fixUp nodes ++ nodesIn ++ nodesOut

        -- figure out the list of srcs
        srcs1   =  [ (OVar i $ nm ++ extra1, ty2)
                   | (OVar i nm, ty) <- sort srcs
                         , (extra1,ty2)
                                <- case toStdLogicTy ty of
                                     B    -> [("",ty)]
                                     V _  -> [("",ty)]
                                     MatrixTy n (V _)
                                          -> let (MatrixTy _ inner) = ty
                                             in reverse [("_x" ++ show j,inner) | j <- [0..(n-1)]]
                                     other -> error $ show ("srcs",other)
                         ]


        -- and give them better sorting numbers
        srcs2   = [ (OVar i nm,ty) | (i,(OVar _ nm,ty)) <- zip [0..] srcs1 ]

        extras0 :: [(OVar,Entity Unique)]
        extras0  = [ (OVar i nm, Entity (Prim "concat")
                              [("o0",ty)]
                              [ ( "i" ++ show j
                                , case ty of
                                   MatrixTy _ inner -> inner
                                , case [ OVar i' nm'
                                         | (OVar i' nm',_) <- srcs2
                                         , nm' == (nm ++ "_x" ++ show j)
                                         ] of
                                      [] -> error ("could not find " ++ show nm)
                                      [x] -> Pad x
                                      _ -> error ("too many of " ++ show nm)
                                )
                              | j <- [0..(n-1)]]
                     )
                  | (OVar i nm, ty) <- sort srcs
                  , (n,_) <- case toStdLogicTy ty of
                              B -> []
                              V {} -> []
                              MatrixTy n (V m) -> [(n,m)]
                  ]

        extras1 :: [(Unique, (OVar, Entity Unique))]
        extras1 = zip srcVars extras0

        nodesIn :: [(Unique, Entity Unique)]
        nodesIn = [ (u,e) | (u,(_,e)) <- extras1 ]

        --------------------------------------------------------------------------------------------

        sinks1  = [ (OVar i $ nm ++ extra1, ty2, dr2)
                  | (u,(OVar i nm, ty, dr)) <- zip (sinkVars) (sort sinks)
                         , (extra1,ty2,dr2)
                                <- case toStdLogicTy ty of
                                     B    -> [("",ty,dr)]
                                     V _  -> [("",ty,dr)]
                                     MatrixTy n (V _)
                                          -> let (MatrixTy _ inner) = ty
                                             in reverse [ ("_x" ++ show j,inner,Port ("o" ++ show j) u) | j <- [0..(n-1)]]
                                     other -> error $ show ("sinks",other)
                  ]

        sinks2 = [ (OVar i nm,ty,dr) | (i,(OVar _ nm,ty,dr)) <- zip [0..] sinks1 ]

        nodesOut :: [(Unique, Entity Unique)]
        nodesOut = [  (u,Entity (Prim "unconcat")
                                [("o" ++ show j,innerTy) | j <- [0..(n-1)]]
                                [("i0",ty,dr)])
                   | (u,(OVar _ _, ty, dr)) <- zip (sinkVars) (sort sinks)
                   , (innerTy,n )
                        <- case toStdLogicTy ty of
                             B    -> []
                             V _  -> []
                             MatrixTy n (V _)
                                  -> let (MatrixTy _ inner) = ty
                                     in [ (inner,n) ]
                             other -> error $ show ("nodesOut",other)
                   ]

        --------------------------------------------------------------------------------------------

        fixUp :: (Unique,Entity Unique) -> (Unique, Entity Unique)
        fixUp (i,Entity e ins outs) = (i,
                Entity e ins
                         [ (o,t,case d of
                                 Pad o2@(OVar _ nm)
                                     -> case [ u | (u,(o3,_)) <- extras1, o2 == o3 ] of
                                             [u] -> Port "o0" u
                                             []  -> case [ j | (OVar j nm',_) <- srcs2, nm == nm' ] of
                                                      [k] -> Pad (OVar k nm)
                                 other -> other
                                 ) | (o,t,d) <- outs ])



