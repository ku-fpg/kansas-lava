{-# LANGUAGE PatternGuards #-}

module Language.KansasLava.Netlist.Inst where

import Language.KansasLava.Type
import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL
import Language.KansasLava.Entity

import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils

import Debug.Trace

genInst :: Unique -> Entity BaseTy Unique -> [Decl]

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

-- For Probes
genInst i (Entity (Name "probe" _)
         [(Var "o0",oty)]
         [(Var "i0", _, input)] _) = [NetAssign (sigName "o0" i) (toTypedExpr oty input)]

genInst i e@(Entity (Name "Memory" "register") [(Var "o0",_)] inputs _) =
          [NetAssign input (toStdLogicExpr ty d) ]
  where output = sigName "o0" i
        input =  next output
	(ty,d) = head [ (ty,d) | (Var "i0",ty,d) <- inputs ]

-- Muxes
genInst i (Entity (Name _ "mux2") [(Var "o0",_)] [(Var i0,cTy,c),(Var i1 ,tTy,t),(Var i2,fTy,f)] _)
	= [NetAssign (sigName "o0" i)
                     (ExprCond cond
                      (toStdLogicExpr fTy t)
                      (toStdLogicExpr tTy f))]
  where cond = ExprBinary Equals (toTypedExpr cTy c) (ExprBit 1)

-- The following do not need any code in the inst segement

genInst i (Entity nm outputs inputs _)
	| nm `elem` [ Name "Memory" "BRAM"
		    ]
	= []


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

specials :: [(Name, NetlistOperation)]
specials =
      mkSpecialBinary (\ _t -> active_high) toTypedExpr
        ["Unsigned", "Signed", "Bool"]
        [ (".<.",LessThan)
	, (".>.",GreaterThan)
	, (".==.",Equals)
	, (".<=.",LessEqual)
	, (".>=.",GreaterEqual)
	, ("./=.",NotEquals) ]
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

