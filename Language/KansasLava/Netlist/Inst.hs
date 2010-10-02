{-# LANGUAGE PatternGuards #-}

module Language.KansasLava.Netlist.Inst where

import Language.KansasLava.Types
import Language.Netlist.AST hiding (U)
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL
import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Wire

import Data.List
import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils

import Debug.Trace

genInst :: Unique -> MuE Unique -> [Decl]

-- The replacements or aliases

genInst i (Entity name outputs inputs (Comment msg:rest)) =
	[ CommentDecl msg
	] ++ genInst i (Entity name outputs inputs rest)

-- Probes are turned into id nodes, add comments to indicate
-- which probes are on which signals in the vhdl.
genInst i (Entity (TraceVal nms _) ins outs _) =
	genInst i (Entity (Name "Lava" "id") ins outs [Comment (intercalate ", " $ map show nms)])


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

genInst  i (Entity (Label label) [(vO,_)] [(vI,ty,d)] _) =
	 	[ CommentDecl label
	        , NetAssign (sigName vO i) $ toStdLogicExpr ty d 
	        ]

-- Concat and index (join, project)

genInst  i (Entity (Name "Lava" "concat") [("o0",_)] inps _) =
                  [NetAssign (sigName "o0" i) val]
  where val = ExprConcat
                -- Note the the layout is reversed, because the 0 bit is on the right hand size
                [ toStdLogicExpr ty s | (_,ty, s) <- reverse inps]

genInst i (Entity (Name "Lava" "index")
		  [("o0",outTy)]
		  [("i0", GenericTy, (Generic idx)),
		   ("i1",ty,input)] _
	   ) =
    [ NetAssign (sigName "o0" i) (prodSlices input tys !! (fromIntegral idx))]
  where tys = case ty of
		MatrixTy sz eleTy -> take sz $ repeat eleTy
		TupleTy tys -> tys
genInst i (Entity (Name "Lava" "index")
		  [("o0",outTy)]
		  [("i0", ixTy, ix),
		   ("i1",eleTy,input)] _) =
	[ NetAssign (sigName "o0" i)
		(ExprCase (toStdLogicExpr ixTy ix)
			[ ([toStdLogicExpr ixTy (i :: Integer)],toStdLogicExpr outTy val)
			| (i,val) <- zip [0..] $ prodSlices input tys
			]
			(Just $ toStdLogicExpr outTy (0 :: Integer))
		)
	]
  where tys = case eleTy of
		MatrixTy sz eleTy -> take sz $ repeat eleTy
		TupleTy tys -> tys

genInst i e@(Entity (Name "Memory" "register") [("o0",_)] inputs _) =
          [NetAssign input (toStdLogicExpr ty d) ]
  where output = sigName "o0" i
        input =  next output
	(ty,d) = head [ (ty,d) | ("i0",ty,d) <- inputs ]

-- Muxes
genInst i (Entity (Name _ "mux2") [("o0",_)] [("i0",cTy,Lit (RepValue [WireVal True])),("i1",tTy,t),("i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i) (toStdLogicExpr tTy t)]
genInst i (Entity (Name _ "mux2") [("o0",_)] [("i0",cTy,Lit (RepValue [WireVal False])),("i1",tTy,t),("i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i) (toStdLogicExpr fTy f)]
genInst i (Entity (Name _ "mux2") [("o0",_)] [("i0",cTy,c),("i1",tTy,t),("i2",fTy,f)] _)
	= [NetAssign (sigName "o0" i)
                     (ExprCond cond
                      (toStdLogicExpr tTy t)
                      (toStdLogicExpr fTy f))]
  where cond = ExprBinary Equals (toTypedExpr cTy c) (ExprLit Nothing (ExprBit T))

-- Sampled

-- TODO: check all arguments types are the same
genInst i (Entity (Name "Lava" op) [("o0",ty@(SampledTy m n))] ins _)
	| op `elem` (map fst mappings)
	= genInst i (Entity (Name "Sampled" nm) [("o0",ty)]
				        (ins ++ [ ("max_value", GenericTy, Generic $ fromIntegral m)
					        , ("width_size",GenericTy, Generic $ fromIntegral n)
					        ]) [])

  where
	Just nm = lookup op mappings

	mappings =
		[ ("+","addition")
		, ("-","subtraction")
		, ("negate","negate")
		]
-- For compares, we need to use one of the arguments.
genInst i (Entity (Name "Lava" op) [("o0",B)] ins@(("i0",SampledTy m n,_):_) _)
	| op `elem` (map fst mappings)
	= genInst i (Entity (Name "Sampled" nm) [("o0",B)]
				        (ins ++ [ ("max_value", GenericTy, Generic $ fromIntegral m)
					        , ("width_size",GenericTy, Generic $ fromIntegral n)
					        ]) [])

  where
	Just nm = lookup op mappings

	mappings =
		[ (".>.","greaterThan")
		]


-- This is only defined over constants that are powers of two.
genInst i (Entity (Name "Lava" "/") [("o0",oTy@(SampledTy m n))] [ ("i0",iTy,v), ("i1",iTy',Lit lit)] _)
--	= trace (show n)
	|  fromRepToInteger lit == 16 * 4
		-- BAD use of fromRepToInteger, because of the mapping to *ANY* value if undefined.
    		-- HACKHACKHACKHACK, 64 : V8 ==> 4 :: Int, in Sampled world
	= [ InstDecl "Sampled_fixedDivPowOfTwo" ("inst" ++ show i)
  		[ ("shift_by",ExprLit Nothing (ExprNum $ fromIntegral $ 2)) ] -- because / 4 is same as >> 2
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
	   (SampledTy _ n,V m) | n == m ->
		[ NetAssign  (sigName "o0" i) $ (toStdLogicExpr t_in w)
		]
	   (B,V 1) ->
		[ NetAssign  (sigName "o0" i ++ "(0)") $ (toStdLogicExpr t_in w) -- complete hack
		]
	   _ -> error $ "fatal : converting from " ++ show t_in ++ " to " ++ show t_out ++ " using toStdLogicVector failed"

genInst i (Entity n@(Name "Lava" "spliceStdLogicVector") [("o0",V outs)] [("i0",_,Generic x),("i1",V ins,w)] _)
	| outs < (ins - i) = error "NEED TO PAD spliceStdLogicVector (TODO)"
	| otherwise =
	[ NetAssign  (sigName "o0" i) $ ExprSlice nm sliceHigh sliceLow
	]
  where
     nm = case toTypedExpr (V ins) w of
  	    ExprVar n -> n
	    other -> error $ " problem with spliceStdLogicVector " ++ show w
     sliceHigh = ExprLit Nothing (ExprNum (fromIntegral x + fromIntegral outs - 1))
     sliceLow = ExprLit Nothing (ExprNum (fromIntegral x))

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
		[ (n,case x of
			Generic v -> ExprLit Nothing (ExprNum v)
			_ -> error $ "genInst, Generic, " ++ show (n,nTy,x)
	          )
		| (n,nTy,x) <- inputs, isGenericTy nTy
		]
                [ (n,toStdLogicExpr nTy x)  | (n,nTy,x) <- inputs, not (isGenericTy nTy) ]
		[ (n,ExprVar $ sigName n i) | (n,nTy)   <- outputs ]
          ]
   where isGenericTy GenericTy = True
         isGenericTy _         = False

-- Idea: table that says you take the Width of i/o Var X, and call it y, for the generics.

genInst i tab@(Entity (Function mp) [(vout,tyout)] [(vin,tyin,d)] _) =
	[ NetAssign (sigName vout i)
		(ExprCase (toStdLogicExpr tyin d)
			[ ([toStdLogicExpr tyin ix],toStdLogicExpr tyout val)
			| (ix,val) <- mp
			]
			(Just $ toStdLogicExpr tyout (0 :: Integer))	-- replace with unknowns
		)
	]

genInst i other = error $ show ("genInst",i,other)


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
