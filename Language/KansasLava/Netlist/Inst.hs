{-# LANGUAGE PatternGuards #-}

module Language.KansasLava.Netlist.Inst where

import Language.KansasLava.Types
import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils

import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils

import Debug.Trace

genInst :: Unique -> MuE Unique -> [Decl]

-- The replacements or aliases

genInst i (Entity name outputs inputs (Comment msg:rest)) =
	[ CommentDecl msg
	] ++ genInst i (Entity name outputs inputs rest)

-- TODO: check for Probes, etc.

genInst i (Entity (Name "Lava" "pair") outputs inputs other)
	= genInst i (Entity (Name "Lava" "concat") outputs inputs other)
genInst i (Entity (Name "Lava" "triple") outputs inputs other)
	= genInst i (Entity (Name "Lava" "concat") outputs inputs other)


genInst i (Entity (Name "Lava" "fst") outputs inputs other)
	= genInst i (Entity (Name "Lava" "index") outputs (addNum 0 inputs) other)
genInst i (Entity (Name "Lava" "snd") outputs inputs other)
	= genInst i (Entity (Name "Lava" "index") outputs (addNum 1 inputs) other)
genInst i (Entity (Name "Lava" "fst3") outputs inputs other)
	= genInst i (Entity (Name "Lava" "index") outputs (addNum 0 inputs) other)
genInst i (Entity (Name "Lava" "snd3") outputs inputs other)
	= genInst i (Entity (Name "Lava" "index") outputs (addNum 1 inputs) other)
genInst i (Entity (Name "Lava" "thd3") outputs inputs other)
	= genInst i (Entity (Name "Lava" "index") outputs (addNum 2 inputs) other)

-- TMP aliases

--genInst i (Entity n@(Name "Sampled" op) outputs inputs dyn)
--      | op `elem` [".<.", ".>.", ".<=.", ".>=.", ".==."]
--      = genInst i (Entity (Name "Signed" op) outputs inputs dyn)

-- identity

genInst  i (Entity (Name "Lava" "id") [(vO,_)] [(vI,ty,d)] _) = 
	 	[ NetAssign (sigName vO i) $ toStdLogicExpr ty d ]

-- Concat and index (join, project)

genInst  i (Entity (Name "Lava" "concat") [("o0",_)] inps _) =
                  [NetAssign (sigName "o0" i) val]
  where val = ExprConcat
                -- Note the the layout is reversed, because the 0 bit is on the right hand size
                [ toStdLogicExpr ty s | (_,ty, s) <- reverse inps]

genInst i (Entity (Name "Lava" "index")
		  [("o0",outTy)]
		  [("i0",_, (Lit idx)),
		   ("i1",ty,input)] _
	   ) =
    [ NetAssign (sigName "o0" i) (prodSlices input tys !! (fromIntegral idx))]
  where tys = case ty of
		MatrixTy sz eleTy -> take sz $ repeat eleTy
		TupleTy tys -> tys

-- For Probes, consider adding a comment
genInst i (Entity (Name "probe" _) ins outs _) = 
	genInst i (Entity (Name "Lava" "id") ins outs [])

genInst i e@(Entity (Name "Memory" "register") [("o0",_)] inputs _) =
          [NetAssign input (toStdLogicExpr ty d) ]
  where output = sigName "o0" i
        input =  next output
	(ty,d) = head [ (ty,d) | ("i0",ty,d) <- inputs ]

-- Muxes
genInst i (Entity (Name _ "mux2") [("o0",_)] [("i0",cTy,Lit 1),("i1",tTy,t),("i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i) (toStdLogicExpr tTy t)]
genInst i (Entity (Name _ "mux2") [("o0",_)] [("i0",cTy,Lit _),("i1",tTy,t),("i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i) (toStdLogicExpr fTy f)]
genInst i (Entity (Name _ "mux2") [("o0",_)] [("i0",cTy,c),("i1",tTy,t),("i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i)
                     (ExprCond cond
                      (toStdLogicExpr tTy t)
                      (toStdLogicExpr fTy f))]
  where cond = ExprBinary Equals (toTypedExpr cTy c) (ExprBit 1)


-- This is only defined over constants that are powers of two.
genInst i (Entity (Name "Sampled" "/") [("o0",oTy)] [ ("i0",iTy,v), ("i1",iTy',Lit n)] _)
--	= trace (show n) 
	| n == 64	-- HACKHACKHACKHACK, 64 : V8 ==> 4 :: Int, in Sampled world
	= [ InstDecl "Sampled_shiftR" ("inst" ++ show i)
  		[ ("shift_by",ExprNum $ fromIntegral $ 2) ]
                [ ("i0",toStdLogicExpr iTy v) ]
		[ ("o0",ExprVar $ sigName "o0" i) ]
          ]

-- The following do not need any code in the inst segement

genInst i (Entity nm outputs inputs _)
	| nm `elem` [ Name "Memory" "BRAM"
		    ]
	= []

-- Logic assignments

genInst i (Entity n@(Name _ "fromStdLogicVector") [("o0",t_out)] [("i0",t_in,w)] _) =
	case (t_in,t_out) of
	   (V n,U m) | n == m -> 
		[ NetAssign  (sigName "o0" i) (toStdLogicExpr t_in w) 
		]
	   (V n,V m) | n == m -> 
		[ NetAssign  (sigName "o0" i) (toStdLogicExpr t_in w) 
		]
	   _ -> error $ "fatal : converting from " ++ show t_in ++ " to " ++ show t_out ++ " using fromStdLogicVector failed"
genInst i (Entity n@(Name "Lava" "toStdLogicVector") [("o0",t_out)] [("i0",t_in,w)] _) =
	case (t_in,t_out) of
	   (U n,V m) | n == m -> 
		[ NetAssign  (sigName "o0" i) $ (toStdLogicExpr t_in w) 
		]
	   (V n,V m) | n == m -> 
		[ NetAssign  (sigName "o0" i) $ (toStdLogicExpr t_in w) 
		]
	   _ -> error $ "fatal : converting from " ++ show t_in ++ " to " ++ show t_out ++ " using toStdLogicVector failed"

genInst i (Entity n@(Name "Lava" "spliceStdLogicVector") [("o0",V outs)] [("i0",_,Lit x),("i1",V ins,w)] _)
	| outs < (ins - i) = error "NEED TO PAD spliceStdLogicVector (TODO)"
	| otherwise = 
	[ NetAssign  (sigName "o0" i) $ ExprSlice nm (ExprNum (fromIntegral x + fromIntegral outs - 1)) (ExprNum (fromIntegral x))
	]
  where
     nm = case toTypedExpr (V ins) w of
  	    ExprVar n -> n
	    other -> error $ " problem with spliceStdLogicVector " ++ show w

-- The specials (from a table)


genInst i (Entity n@(Name _ _) [("o0",oTy)] ins _)
        | Just (NetlistOp arity f) <- lookup n specials, arity == length ins =
          [NetAssign  (sigName "o0" i)
                  (f oTy [(inTy, driver)  | (_,inTy,driver) <- ins])]

-- And the defaults

-- Right now, we *assume* that every external entity
-- has in and outs of type std_logic[_vector].
--
genInst i (Entity n@(Name mod_nm nm) outputs inputs _) =
	trace (show ("mkInst",n,[ t | (_,t) <- outputs ],[ t | (_,t,_) <- inputs ])) $
          [ InstDecl (mod_nm ++ "_" ++ cleanupName nm) ("inst" ++ show i)
  		[ ("width_size",ExprNum $ fromIntegral $ head [ typeWidth ty | (_,ty) <- outputs ])
			| mod_nms <- ["Sampled"]	-- hack
			, mod_nm == mod_nms
		]
                [ (n,toStdLogicExpr nTy x)  | (n,nTy,x) <- inputs ]
		[ (n,ExprVar $ sigName n i) | (n,nTy)   <- outputs ]
          ]

-- Idea: table that says you take the Width of i/o Var X, and call it y, for the generics.

-- TODO: Table should have a default, for space reasons

genInst i tab@(Table (vout,tyout) (vin,tyin,d) mp) =
	[ NetAssign (sigName vout i)
		(ExprCase (toStdLogicExpr tyin d)
			[ ([toStdLogicExpr tyin ix],toStdLogicExpr tyout val)
			| (ix,_,val,_) <- mp
			]
			(Just $ toStdLogicExpr tyout (0 :: Integer))
		)
	]



--------------------------------------------------------------

data NetlistOperation = NetlistOp Int (Type -> [(Type,Driver Unique)] -> Expr)

mkSpecialUnary
	:: (Type -> Expr -> Expr)
	-> (Type -> Driver Unique -> Expr)
	-> [(String, UnaryOp)]
	-> [(Id, NetlistOperation)]
mkSpecialUnary coerceR coerceF ops =
       [( Name "Lava" lavaName
	, NetlistOp 1 $ \ fTy [(ity,i)] ->
		coerceR fTy (ExprUnary netListOp
					(coerceF ity i))

	)
         | (lavaName,netListOp) <- ops
         ]

mkSpecialBinary
	:: (Type -> Expr -> Expr)
	-> (Type -> Driver Unique -> Expr)
--	-> [String]
	-> [(String, BinaryOp)]
	-> [(Id, NetlistOperation)]
mkSpecialBinary coerceR coerceF ops =
       [( Name "Lava" lavaName
	, NetlistOp 2 $ \ fTy [(lty,l),(rty,r)] ->
		coerceR fTy (ExprBinary netListOp
					(coerceF lty l)
					(coerceF rty r))

	)
         | (lavaName,netListOp) <- ops
         ]

mkSpecialShifts ops =
    [(Name "Lava" lavaName
      , NetlistOp 2 ( \ fTy [(lty,l),(rty,r)] ->
                          toStdLogicExpr fTy $ ExprFunCall funName [toTypedExpr lty l, toIntegerExpr rty r])
     )
    | (lavaName, funName) <- ops
    ]

-- testBit returns the bit-value at a specific (constant) bit position
-- of a bit-vector.
-- This generates:    invar(indexVal);
mkSpecialTestBit =
    [(Name "Lava" lavaName
      , NetlistOp 2 ( \ fTy [(lty,l),(rty,r)] ->
                          let (ExprVar varname) =  toStdLogicExpr lty l
                          in (ExprIndex varname (toIntegerExpr rty r)))
     )
    | lavaName <- ["testBit"]
    ]


specials :: [(Id, NetlistOperation)]
specials =
      mkSpecialBinary (\ _t -> active_high) toTypedExpr
        [ (".<.",LessThan)
	, (".>.",GreaterThan)
	, (".<=.",LessEqual)
	, (".>=.",GreaterEqual)
        , (".==.",Equals)
	, ("./=.",NotEquals)
	]
   ++ mkSpecialBinary toStdLogicExpr toTypedExpr
        [("+",Plus)
	, ("-",Minus)
	, ("*", Times)
	, ("/", Divide)
	]
   ++ mkSpecialBinary (\ _ e -> e) toStdLogicExpr
        [ (".|.",Or), (".&.",And), (".^.",Xor)
	, ("or2",Or), ("and2",And), ("xor2",Xor)
	]
   ++ mkSpecialUnary  toStdLogicExpr toTypedExpr
	[("negate",Neg)]
   ++ mkSpecialUnary  (\ _ e -> e) toStdLogicExpr
	[("not",LNeg)]
   ++   mkSpecialTestBit
   ++   mkSpecialShifts
        [ ("shiftL", "shift_left")
        , ("shiftR", "shift_right")
        , ("rotateL", "rotate_left")
        , ("rotateR", "rotate_right")
        ]
