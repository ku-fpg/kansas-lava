module Language.KansasLava.Netlist.Sync where

import Language.KansasLava.Type
import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL
import Language.KansasLava.Entity

import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils

import qualified Data.Map as Map


-- TODO: change into uncurried.

genSync :: NetlistOptions -> [(Unique,Entity BaseTy Unique)] -> [Decl]
genSync nlOpts nodes  = (concatMap (uncurry $ regProc nlOpts ) $ Map.toList regs) ++
                          (concatMap (uncurry bramProc ) $ Map.toList brams)
  where -- Handling registers
        regs = getSynchs ["register", "delay"] nodes
        brams = getSynchs ["BRAM"] nodes

-- genSync nlOpts _ _ = []

regProc _ (clk,rst) [] = []
regProc nlOpts (clk,rst) es
  | asynchResets nlOpts =
    [ProcessDecl
     [(Event (toStdLogicExpr RstTy rst) PosEdge,
               (statements [Assign (outName e i) (defaultDriver e) |  (i,e) <- es])),
      (Event (toStdLogicExpr ClkTy clk) PosEdge,
              regNext)]]
  | otherwise =
    [ProcessDecl
     [(Event (toStdLogicExpr ClkTy clk) PosEdge,
        (case rst of
          Lit 0 -> regNext
          Lit 1 -> error "opps, bad delay code (reset *always* set)"
          _ -> If (isHigh (toTypedExpr B rst))
                           (statements [Assign (outName e i) (defaultDriver e) |  (i,e) <- es])
                           (Just regNext)))
     ]
    ]

  where outName e i = toStdLogicExpr (lookupInputType "o0" e) (Port (Var "o0") i)
        nextName e i = toStdLogicExpr  (lookupInputType "o0" e) $ next (Port (Var "o0") i)
        defaultDriver e = toStdLogicExpr (defaultDriverType e) $ lookupInput "def" e
        defaultDriverType e = lookupInputType "def" e
        driver e = toStdLogicExpr (lookupInputType "o0" e) $ next $ lookupInput "i0" e
        regAssigns = statements [Assign (outName e i) (nextName e i)  | (i,e) <- es]
        regNext
          | addEnabled nlOpts = If (isHigh (ExprVar "enable")) regAssigns Nothing
          | otherwise = regAssigns


bramProc (clk,rst) [] = []
bramProc (clk,rst) es =
  [ProcessDecl
   [(Event (toStdLogicExpr ClkTy clk) PosEdge,
           (statements $ concat
             [ [ If (isHigh (wEn e))
                   (Assign (writeIndexed i e) (wData e))
                   Nothing
		-- TODO: will need extra delay
	       , Assign (outName e i) (readIndexed i e)
	       ]
	     | (i,e) <- es 
	     ]
	   ))]
  ]
    where outName e i = toStdLogicExpr (lookupInputType "wData" e) (Port (Var "o0") i)
          ramName i = "sig_" ++ show i ++ "_o0_ram"
          wEn e = toStdLogicExpr (lookupInputType "wEn" e) $ lookupInput "wEn" e
          wAddr e = lookupInput "wAddr" e
          rAddr e = lookupInput "rAddr" e
          wData e = toStdLogicExpr (lookupInputType "wData" e) $ lookupInput "wData" e
          -- FIXME: This is a hack, to get around not having dynamically
          -- indexed exprs.
          writeIndexed i e = ExprIndex 
                         (ramName i) 
			 (toIntegerExpr (lookupInputType "wAddr" e) (wAddr e))
          readIndexed i e = ExprIndex 
			(ramName i) 
			(toIntegerExpr (lookupInputType "wAddr" e) (rAddr e))
