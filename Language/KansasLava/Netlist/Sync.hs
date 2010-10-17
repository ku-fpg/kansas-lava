module Language.KansasLava.Netlist.Sync where

import Language.KansasLava.Types
import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils

import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils

import qualified Data.Map as Map


-- TODO: change into uncurried.

genSync :: [(Unique,MuE Unique)] -> [Decl]
genSync  nodes  = (concatMap (uncurry $ regProc ) $ Map.toList regs) ++
                          (concatMap (uncurry bramProc ) $ Map.toList brams)
  where -- Handling registers
        regs = getSynchs ["register", "delay"] nodes
        brams = getSynchs ["BRAM"] nodes

-- genSync nlOpts _ _ = []

regProc :: (Driver Unique, Driver Unique,  Driver Unique)
        -> [(Unique, MuE Unique)]
	-> [Decl]
regProc (clk,rst,clk_en) [] = []
regProc (clk,rst,clk_en) es
{-
  | asynchResets nlOpts =
    [ProcessDecl
     [(Event (toStdLogicExpr RstTy rst) PosEdge,
               (statements [Assign (outName e i) (defaultDriver e) |  (i,e) <- es])),
      (Event (toStdLogicExpr ClkTy clk) PosEdge,
              regNext)]]
-}
  | otherwise =
    [ProcessDecl
     [(Event (toStdLogicExpr ClkTy rst) AsyncHigh,
       statements [Assign (outName e i) (defaultDriver e) |  (i,e) <- es]
      )
     ,(Event (toStdLogicExpr ClkTy clk) PosEdge,
        regNext
      )
     ]
    ]

  where outName e i = toStdLogicExpr (lookupInputType "o0" e) (Port ("o0") i)
        nextName e i = toStdLogicExpr  (lookupInputType "o0" e) $ next (Port ("o0") i)
        defaultDriver e = toStdLogicExpr (defaultDriverType e) $ lookupInput "def" e
        defaultDriverType e = lookupInputType "def" e
        driver e = toStdLogicExpr (lookupInputType "o0" e) $ next $ lookupInput "i0" e
        regAssigns = statements [Assign (outName e i) (nextName e i)  | (i,e) <- es]
        regNext = case clk_en of
		    Lit (RepValue [WireVal True]) -> regAssigns
		    Lit _ -> error "opps, en_clk is never enabled (boring?)"
		    _     -> If (isHigh (toTypedExpr B clk_en)) regAssigns Nothing


bramProc (clk,_,clk_en) [] = []
bramProc (clk,_,clk_en) es =
  [ProcessDecl
   [(Event (toStdLogicExpr ClkTy clk) PosEdge,
           (statements $ concat
             [ [ If (isHigh (clk_en e))
	 	    (Seq [ If (isHigh (wEn e))
                              (Assign (writeIndexed i e) (wData e))
			      Nothing
			 , Assign (outName e i) (readIndexed i e)
			 ])
                    Nothing
               ]
	     | (i,e) <- es
	     ]
	   ))]
  ]
    where outName e i = toStdLogicExpr (lookupInputType "wData" e) (Port ("o0") i)
          ramName i = "sig_" ++ show i ++ "_o0_ram"
          wEn e = toStdLogicExpr (lookupInputType "wEn" e) $ lookupInput "wEn" e
	  clk_en e = toStdLogicExpr (lookupInputType "en" e) $ lookupInput "en" e
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
