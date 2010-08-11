{-# LANGUAGE PatternGuards #-}

module Language.KansasLava.Netlist.Inst where

import Language.KansasLava.Type
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

genInst i (Entity n@(Name "X32" "+") outputs inputs dyn) =
	genInst i (Entity (Name "Unsigned" "+") outputs inputs dyn)
genInst i (Entity n@(Name "X32" "-") outputs inputs dyn) =
	genInst i (Entity (Name "Unsigned" "-") outputs inputs dyn)
genInst i (Entity n@(Name "Sampled" op) outputs inputs dyn)
      | op `elem` [".<.", ".>.", ".<=.", ".>=.", ".==."]
      = genInst i (Entity (Name "Signed" op) outputs inputs dyn)


-- identity

genInst  i (Entity (Name "Lava" "id") [(Var vO,_)] [(Var vI,ty,d)] _) = 
	 	[ NetAssign (sigName vO i) $ toStdLogicExpr ty d ]

-- Concat and index (join, project)

genInst  i (Entity (Name "Lava" "concat") [(Var "o0",_)] inps _) =
                  [NetAssign (sigName "o0" i) val]
  where val = ExprConcat
                -- Note the the layout is reversed, because the 0 bit is on the right hand size
                [ toStdLogicExpr ty s | (Var _,ty, s) <- reverse inps]

genInst i (Entity (Name "Lava" "index")
		  [(Var "o0",outTy)]
		  [(Var "i0",_, (Lit idx)),
		   (Var "i1",ty,input)] _
	   ) =
    [ NetAssign (sigName "o0" i) (prodSlices input tys !! (fromIntegral idx))]
  where tys = case ty of
		MatrixTy sz eleTy -> take sz $ repeat eleTy
		TupleTy tys -> tys

-- For Probes, consider adding a comment
genInst i (Entity (Name "probe" _) ins outs _) = 
	genInst i (Entity (Name "Lava" "id") ins outs [])

genInst i e@(Entity (Name "Memory" "register") [(Var "o0",_)] inputs _) =
          [NetAssign input (toStdLogicExpr ty d) ]
  where output = sigName "o0" i
        input =  next output
	(ty,d) = head [ (ty,d) | (Var "i0",ty,d) <- inputs ]

-- Muxes
genInst i (Entity (Name _ "mux2") [(Var "o0",_)] [(Var "i0",cTy,Lit 1),(Var "i1",tTy,t),(Var "i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i) (toStdLogicExpr tTy t)]
genInst i (Entity (Name _ "mux2") [(Var "o0",_)] [(Var "i0",cTy,Lit _),(Var "i1",tTy,t),(Var "i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i) (toStdLogicExpr fTy f)]
genInst i (Entity (Name _ "mux2") [(Var "o0",_)] [(Var "i0",cTy,c),(Var "i1",tTy,t),(Var "i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i)
                     (ExprCond cond
                      (toStdLogicExpr tTy t)
                      (toStdLogicExpr fTy f))]
  where cond = ExprBinary Equals (toTypedExpr cTy c) (ExprBit 1)

-- The following do not need any code in the inst segement

genInst i (Entity nm outputs inputs _)
	| nm `elem` [ Name "Memory" "BRAM"
		    ]
	= []

-- Logic assignments

genInst i (Entity n@(Name _ "fromStdLogicVector") [(Var "o0",t_out)] [(Var "i0",t_in,w)] _) =
	case (t_in,t_out) of
	   (V n,U m) | n == m -> 
		[ NetAssign  (sigName "o0" i) (toStdLogicExpr t_in w) 
		]
	   _ -> error $ "fatal : converting from " ++ show t_in ++ " to " ++ show t_out ++ " using fromStdLogicVector failed"
genInst i (Entity n@(Name "StdLogicVector" "toStdLogicVector") [(Var "o0",t_out)] [(Var "i0",t_in,w)] _) =
	case (t_in,t_out) of
	   (U n,V m) | n == m -> 
		[ NetAssign  (sigName "o0" i) $ (toStdLogicExpr t_in w) 
		]
	   _ -> error $ "fatal : converting from " ++ show t_in ++ " to " ++ show t_out ++ " using toStdLogicVector failed"

genInst i (Entity n@(Name "StdLogicVector" "spliceStdLogicVector") [(Var "o0",V outs)] [(Var "i0",_,Lit x),(Var "i1",V ins,w)] _)
	| outs < (ins - i) = error "NEED TO PAD spliceStdLogicVector (TODO)"
	| otherwise = 
	[ NetAssign  (sigName "o0" i) $ ExprSlice nm (ExprNum (fromIntegral x + fromIntegral outs - 1)) (ExprNum (fromIntegral x))
	]
  where
     nm = case toTypedExpr (V ins) w of
  	    ExprVar n -> n
	    other -> error $ " problem with spliceStdLogicVector " ++ show w
-- HACK

genInst i (Entity n@(Name modNm bitOp) outs ins misc)
	| bitOp `elem` [".==.","./=."] && modNm /= "Vector"
	= genInst i (Entity (Name "Vector" bitOp) outs ins misc)

-- The specials (from a table)


genInst i (Entity n@(Name _ _) [(Var "o0",oTy)] ins _)
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
  		[ ("width_size",ExprNum $ fromIntegral $ head [ baseTypeLength ty | (_,ty) <- outputs ])
			| mod_nms <- ["Sampled"]	-- hack
			, mod_nm == mod_nms
		]
                [ (n,toStdLogicExpr nTy x)            | (Var n,nTy,x) <- inputs ]
		[ (n,ExprVar $ sigName n i) | (Var n,nTy)   <- outputs ]
          ]

-- Idea: table that says you take the Width of i/o Var X, and call it y, for the generics.

-- TODO: Table should have a default, for space reasons

genInst i tab@(Table (Var vout,tyout) (vin,tyin,d) mp) =
	[ NetAssign (sigName vout i)
		(ExprCase (toStdLogicExpr tyin d)
			[ ([toStdLogicExpr tyin ix],toStdLogicExpr tyout val)
			| (ix,_,val,_) <- mp
			]
			(Just $ toStdLogicExpr tyout (0 :: Integer))
		)
	]



--------------------------------------------------------------

data NetlistOperation = NetlistOp Int (BaseTy -> [(BaseTy,Driver Unique)] -> Expr)

mkSpecialUnary
	:: (BaseTy -> Expr -> Expr)
	-> (BaseTy -> Driver Unique -> Expr)
	-> [String]
	-> [(String, UnaryOp)]
	-> [(Name, NetlistOperation)]
mkSpecialUnary coerceR coerceF mtys ops =
       [( Name moduleName lavaName
	, NetlistOp 1 $ \ fTy [(ity,i)] ->
		coerceR fTy (ExprUnary netListOp
					(coerceF ity i))

	)
         | moduleName <- mtys
         , (lavaName,netListOp) <- ops
         ]

mkSpecialBinary
	:: (BaseTy -> Expr -> Expr)
	-> (BaseTy -> Driver Unique -> Expr)
	-> [String]
	-> [(String, BinaryOp)]
	-> [(Name, NetlistOperation)]
mkSpecialBinary coerceR coerceF mtys ops =
       [( Name moduleName lavaName
	, NetlistOp 2 $ \ fTy [(lty,l),(rty,r)] ->
		coerceR fTy (ExprBinary netListOp
					(coerceF lty l)
					(coerceF rty r))

	)
         | moduleName <- mtys
         , (lavaName,netListOp) <- ops
         ]

-- testBit returns the bit-value at a specific (constant) bit position
-- of a bit-vector.
-- This generates:    invar(indexVal);
mkSpecialTestBit mtys  =
    [(Name moduleName lavaName
      , NetlistOp 2 ( \ fTy [(lty,l),(rty,r)] ->
                          let (ExprVar varname) =  toStdLogicExpr lty l
                          in (ExprIndex varname (toIntegerExpr rty r)))
     )
     | moduleName <- mtys
    , lavaName <- ["testBit"]
    ]

mkSpecialShifts mtys ops =
    [(Name moduleName lavaName
      , NetlistOp 2 ( \ fTy [(lty,l),(rty,r)] ->
                          let (ExprVar varname) =  toStdLogicExpr lty l
                          in toStdLogicExpr fTy $ ExprFunCall funName [toTypedExpr lty l, toIntegerExpr rty r])
     )
     | moduleName <- mtys
    , (lavaName, funName) <- ops
    ]

specials :: [(Name, NetlistOperation)]
specials =
      mkSpecialBinary (\ _t -> active_high) toTypedExpr
        ["Unsigned", "Signed", "Bool"]
        [ (".<.",LessThan)
	, (".>.",GreaterThan)
--	, (".==.",Equals)
	, (".<=.",LessEqual)
	, (".>=.",GreaterEqual)
--	, ("./=.",NotEquals) 
	]
   ++ mkSpecialBinary (\ _t -> active_high) toTypedExpr
        ["Vector"]
        [(".==.",Equals), ("./=.",NotEquals)]
   ++ mkSpecialBinary toStdLogicExpr toTypedExpr
        ["Unsigned", "Signed"]
        [("+",Plus)
	, ("-",Minus)
	, ("*", Times)
	, ("/", Divide)]
   ++ mkSpecialBinary (\ _ e -> e) toStdLogicExpr
        ["Unsigned", "Signed"]
        [(".|.",Or), (".&.",And), (".^.",Xor)]
   ++ mkSpecialBinary (\ _ e -> e) toStdLogicExpr
        ["Bool"]
        [("or2",Or), ("and2",And), ("xor2",Xor)]
   ++ mkSpecialUnary  toStdLogicExpr toTypedExpr
        ["Signed"]
	[("negate",Neg)]
   ++ mkSpecialUnary  (\ _ e -> e) toStdLogicExpr
        ["Bool"]
	[("not",LNeg)]
   ++   mkSpecialTestBit
        ["Unsigned", "Signed"]
   ++   mkSpecialShifts
        ["Unsigned", "Signed"]
        [ ("shiftL", "shift_left")
        , ("shiftR", "shift_right")
        , ("rotateL", "rotate_left")
        , ("rotateR", "rotate_right")
        ]
