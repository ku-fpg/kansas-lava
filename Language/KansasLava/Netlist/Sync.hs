-- | The Sync module generates synchronous 'Netlist' processes for Lava
-- entities.
module Language.KansasLava.Netlist.Sync(genSync) where

import Language.KansasLava.Types
import Language.Netlist.AST
import Language.Netlist.Util

import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils



-- TODO: change into uncurried.
-- | Generate Netlist processes for registers and BRAMs.
genSync :: [(Unique,Entity Unique)] -> [Decl]
genSync  nodes  = (concatMap (uncurry $ regProc ) $ regs) ++
                          (concatMap (uncurry bramProc ) $  brams)
  where -- Handling registers
        regs = getSynchs  ["register"] nodes
        brams = getSynchs ["BRAM"] nodes

-- genSync nlOpts _ _ = []
-- | Construct a register process.
regProc :: (Driver Unique, Driver Unique,  Driver Unique)
        -> [(Unique, Entity Unique)]
	-> [Decl]
regProc (_,_,_) [] = []
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
       (Event (toStdLogicExpr ClkTy clk) PosEdge)
       (Just ( Event (toStdLogicExpr ClkTy rst) PosEdge
             , statements [Assign (outName e i) (defaultDriver e) |  (i,e) <- es]
             )
       )
       regNext
    ]

  where outName e i = toStdLogicExpr (lookupInputType "o0" e) (Port ("o0") i)
        nextName e i = toStdLogicExpr  (lookupInputType "o0" e) $ next (Port ("o0") i)
        defaultDriver e = toStdLogicExpr (defaultDriverType e) $ lookupInput "def" e
        defaultDriverType e = lookupInputType "def" e
        -- driver e = toStdLogicExpr (lookupInputType "o0" e) $ next $ lookupInput "i0" e
        regAssigns = statements [Assign (outName e i) (nextName e i)  | (i,e) <- es]
        regNext = case clk_en of
		    Lit (RepValue [WireVal True]) -> regAssigns
		    Lit _ -> error "opps, en_clk is never enabled (boring?)"
		    _     -> If (isHigh (toTypedExpr B clk_en)) regAssigns Nothing

-- | Construct a BRAM process.
bramProc :: (Driver Unique, Driver Unique,  Driver Unique)
         -> [(Unique, Entity Unique)]
	 -> [Decl]
bramProc (_,_,_) [] = []
bramProc (clk,_,clk_en) es =
  [ProcessDecl
    (Event (toStdLogicExpr ClkTy clk) PosEdge)
    Nothing
    (statements $ concat
             [ [ If (isHigh (clk_en'))
	 	    (Seq [ If (isHigh (wEn e))
                              (Assign (writeIndexed i e) (wData e))
			      Nothing
			 , Assign (outName e i) (readIndexed i e)
			 ])
                    Nothing
               ]
	     | (i,e) <- es
	     ]
	   )
  ]
    where outName e i = toStdLogicExpr (lookupInputType "wData" e) (Port ("o0") i)
          ramName i = "sig_" ++ show i ++ "_o0_ram"
          wEn e = toStdLogicExpr (lookupInputType "wEn" e) $ lookupInput "wEn" e
	  clk_en' = toStdLogicExpr B clk_en
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



------------------------------------------------------------------------------------

-- | Grab all of the synchronous elements (listed in 'nms') and return a map
-- keyed on clk input, with the value including a list of associated entities.
-- only works for a single clock domain, for now.  TODO: What is going on here!!
getSynchs :: [String]
	  -> [(Unique,Entity Unique)]
	  -> [((Driver Unique, Driver Unique, Driver Unique),[(Unique, Entity Unique)])]
getSynchs nms ents =
	[ ((clk_dr,rst_dr,en_dr),
	    [ e | e@(_,Entity (Prim n) _ _) <- ents,  n `elem` nms ]
           )
	| (_,Entity (Prim "Env") _ [("clk_en",B,en_dr),("clk",ClkTy,clk_dr),("rst",B,rst_dr)]) <- ents
	]
{-
  where
        synchs = [((getInput "clk" is,getInput "rst" is,getInput "en" is),[e])
		 | e@(i,Entity (Name "Memory" n) _ is _) <- ents,
		    n `elem` nms]
        getInput nm is = case find (\(c,_,_) -> c == nm) is of
                      Just (_,_,d) -> d
                      Nothing -> error $ "getSynchs: Can't find a signal " ++ show (nm,is,ents)
-}
---------------------------------------------------------------------------------
