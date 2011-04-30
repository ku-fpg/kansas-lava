-- | This module converts a Lava circuit to a synthesizable Netlist @Module@.
{-# LANGUAGE ParallelListComp #-}
module Language.KansasLava.Netlist
  (netlistCircuit, preprocessNetlistCircuit) where

import Language.KansasLava.Types
import Language.KansasLava.Netlist.Utils(toStdLogicExpr,toStdLogicTy, isMatrixStdLogicTy, sizedRange)
import Language.KansasLava.Netlist.Decl
import Language.KansasLava.Netlist.Inst

import Language.Netlist.AST

import Data.List(sort)
import Data.Reify(Unique)
import qualified Data.Map as M


-- | The 'netlistCircuit' function converts a Lava circuit into a Netlist AST
--   The circuit type must implement the 'Ports' class.  If the circuit type is
--   a function, the function arguments will be exposed as input ports, and the
--   result will be exposed as an output port (or ports, if it is a compound
--   type).
netlistCircuit :: String         -- ^ The name of the generated entity.
               -> KLEG 	 -- ^ The Lava circuit.
               -> IO Module
netlistCircuit name circ = do
  let (KLEG nodes srcs sinks) = preprocessNetlistCircuit circ

  let inports = checkPortType srcs
  let outports = checkPortType (map outputNameAndType sinks)

  -- Finals are the assignments from the output signals for entities to the output ports
  let finals = [ NetAssign n (toStdLogicExpr ty x) | (OVar _ n,ty,x) <- sort sinks
                                                   , case toStdLogicTy ty of
                                                        MatrixTy {} -> error "can not have a matrix as an out argument"
                                                        _ -> True
               ]

  return $ Module name inports outports []
	   (concatMap genDecl nodes ++
	    concatMap (uncurry (genInst' (M.fromList nodes))) nodes ++
	    finals)


  where checkPortType ports =  [ (nm,sizedRange ty) | (OVar _ nm, ty) <- sort ports
                               , not (isMatrixStdLogicTy ty) || error "can not have a matrix as a port"
                               ]
        outputNameAndType (n,ty,_) = (n,ty)



-- | This gets a circuit ready for Netlist generation.
-- Specifically, it normalizes all the arguments
-- because arguments that are of type MatrixTy are now supported.
preprocessNetlistCircuit :: KLEG -> KLEG
preprocessNetlistCircuit cir = res
    where
        KLEG nodes srcs sinks = cir
        res = KLEG nodes' srcs' sinks'

        vars = allocEntities cir

        (sinkVars,srcVars) = splitAt (length sinks) vars

        nodes'  = map fixUp nodes ++ nodesIn ++ nodesOut

        -- figure out the list of srcs
        srcs'   =  [ (OVar k $ nm ++ extra1, ty2)
                   | (OVar _ nm, ty) <- sort srcs
                         , (extra1,ty2)
                                <- case toStdLogicTy ty of
                                     B    -> [("",ty)]
                                     V _  -> [("",ty)]
                                     MatrixTy n (V _)
                                          -> let (MatrixTy _ inner) = ty
                                             in reverse [("_x" ++ show j,inner) | j <- [0..(n-1)]]
                                     other -> error $ show ("srcs",other)
                   | k <- [0..] -- This gives them better sorting numbers
                   ]


        extras0 :: [(OVar,Entity Unique)]
        extras0  = [ (OVar i nm, Entity (Prim "concat")
                              [("o0",ty)]
                              [ ( 'i':show j
                                , case ty of
                                   MatrixTy _ inner -> inner
                                   _ -> error $ "preprocessVhdlCircuit: not a matrix type " ++ show ty
                                , case [ OVar i' nm'
                                         | (OVar i' nm',_) <- srcs'
                                         , nm' == (nm ++ "_x" ++ show j)
                                         ] of
                                      [] -> error ("could not find " ++ show nm)
                                      [x] -> Pad x
                                      _ -> error ("too many of " ++ show nm)
                                )
                              | j <- [0..(getMatrixNumColumns ty - 1)]]
                     )
                  | (OVar i nm, ty) <- sort srcs
                  , isMatrixStdLogicTy ty
                  ]

        getMatrixNumColumns (MatrixTy c _) = c
        getMatrixNumColumns _ = error "Can't get number of columns for non-matrix type"

        extras1 :: [(Unique, (OVar, Entity Unique))]
        extras1 = zip srcVars extras0

        nodesIn :: [(Unique, Entity Unique)]
        nodesIn = [ (u,e) | (u,(_,e)) <- extras1 ]

        --------------------------------------------------------------------------------------------

        sinks'  = [ (OVar k $ nm ++ extra1, ty2, dr2)
                  | (u,(OVar _ nm, ty, dr)) <- zip sinkVars (sort sinks)
                         , (extra1,ty2,dr2)
                                <- case toStdLogicTy ty of
                                     B    -> [("",ty,dr)]
                                     V _  -> [("",ty,dr)]
                                     MatrixTy n (V _)
                                          -> let (MatrixTy _ inner) = ty
                                             in reverse [ ("_x" ++ show j,inner,Port ('o':show j) u) | j <- [0..(n-1)]]
                                     other -> error $ show ("sinks",other)
                  | k <- [0..] -- This gives them better sorting numbers
                  ]


        nodesOut :: [(Unique, Entity Unique)]
        nodesOut = [  (u,Entity (Prim "unconcat")
                                [('o':show j,innerTy) | j <- [0..(n-1)]]
                                [("i0",ty,dr)])
                   | (u,(OVar _ _, ty, dr)) <- zip sinkVars (sort sinks)
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
                                             []  -> case [ j | (OVar j nm',_) <- srcs', nm == nm' ] of
                                                      [k] -> Pad (OVar k nm)
                                                      _ -> error "fixUp find"
                                             _ -> error "fixUp"
                                 other -> other
                                 ) | (o,t,d) <- outs ])



