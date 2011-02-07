{-# LANGUAGE PatternGuards #-}

module Language.KansasLava.Netlist.Inst where

import Language.KansasLava.Types
import Language.Netlist.AST hiding (U)
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL
-- import Language.KansasLava.Entity
import Language.KansasLava.Deep
import Language.KansasLava.Shallow
import qualified Data.Map as M

import Data.List
import Data.Maybe as Maybe
import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils as Utils

import Debug.Trace


genInst' env i e =
	[ CommentDecl $ show (i,e)
	] ++ genInst env i e

genInst :: M.Map Unique (Entity Unique) -> Unique -> Entity Unique -> [Decl]


-- (Commented out) debugging hook
-- genInst env i en | trace (show ("genInst",en)) False = undefined

-- Some entities never appear in output (because they are virtual)
--genInst env i (Entity nm ins outs) | nm `elem` isVirtualEntity = []

-- You never actually write something that is zero width.
genInst env i (Entity nm [(_,ty)] outs) | toStdLogicTy ty == V 0 = []

{-
-- We expand out all the ClkDom's, projecting into the components,
-- for VHDL generation purposes.
genInst env i e@(Entity (Prim nm) outs ins) | length ins2 > 0 = 
	genInst env i (Entity (Prim nm) outs (ins' ++ ins2))
   where
	ins' = [ p | p@(nm,ty,dr) <- ins, ty /= ClkDomTy ]

	ins2 = concat 
		[ case M.lookup p_id env of
	   	    Just (Entity (Prim "Env") _ ins_e) -> 
				[ (env_nm ++ "_" ++ nm,ty,dr) 
				| (nm,ty,dr) <- ins_e 
				]
	   	    _ -> error $ "can not find clock domain for " ++ show (p_id,e)
		| (env_nm,ClkDomTy, Port "env" p_id) <- ins 
		]
-}


-- For now, translate primitives to Prim
genInst env i (Entity (Name "Lava" nm) ins outs) =
	genInst env i (Entity (Prim nm) ins outs)
genInst env i (Entity (Name "" nm) ins outs) =
	genInst env i (Entity (Prim nm) ins outs)
	
-- Probes are turned into id nodes, add comments to indicate
-- which probes are on which signals in the vhdl.
genInst env i (Entity (TraceVal nms _) ins outs) =
	genInst env i (Entity (Prim "id") ins outs) -- TODO: add [Comment (intercalate ", " $ map show nms)])

-- Blackbox nodes should have been removed by reification, but alas, no.
genInst env i (Entity (BlackBox box) ins outs) =
  genInst env i (Entity (Prim "id") ins outs)

genInst env i (Entity (Comment' comments) [] []) =
        [ CommentDecl (unlines comments)
        ] 
genInst env i (Entity (Comment' comments) ins@[_] outs@[_]) =
        [ CommentDecl (unlines comments)
        ] ++ 
	genInst env i (Entity (Prim "id") ins outs) 
		
genInst env i (Entity (Prim "pair") outputs inputs)
	= genInst env i (Entity (Prim "concat") outputs inputs)
genInst env i (Entity (Prim "triple") outputs inputs)
	= genInst env i (Entity (Prim "concat") outputs inputs)


genInst env i (Entity (Prim "fst") outputs inputs)
	= genInst env i (Entity (Prim "index") outputs (addNum 0 inputs))
genInst env i (Entity (Prim "snd") outputs inputs)
	= genInst env i (Entity (Prim "index") outputs (addNum 1 inputs))
genInst env i (Entity (Prim "fst3") outputs inputs)
	= genInst env i (Entity (Prim "index") outputs (addNum 0 inputs))
genInst env i (Entity (Prim "snd3") outputs inputs)
	= genInst env i (Entity (Prim "index") outputs (addNum 1 inputs))
genInst env i (Entity (Prim "thd3") outputs inputs)
	= genInst env i (Entity (Prim "index") outputs (addNum 2 inputs))

-- TMP aliases

--genInst env i (Entity n@(Name "Sampled" op) outputs inputs dyn)
--      | op `elem` [".<.", ".>.", ".<=.", ".>=.", ".==."]
--      = genInst env i (Entity (Name "Signed" op) outputs inputs dyn)

-- identity

genInst env i (Entity (Prim "id") [(vO,_)] [(vI,ty,d)] ) =
	 	[ NetAssign (sigName vO i) $ toStdLogicExpr ty d ]

genInst env i (Entity (Label label) [(vO,_)] [(vI,ty,d)] ) =
	 	[ CommentDecl label
	        , NetAssign (sigName vO i) $ toStdLogicExpr ty d 
	        ]

-- Concat and index (join, project)

genInst env i (Entity (Prim "concat") [("o0",ty)] ins)
        | case toStdLogicTy ty of
            MatrixTy {} -> True
            _ -> False
        =
        [ MemAssign 
                (sigName ("o0") i) 
                (ExprLit Nothing $ ExprNum $ j)
                (toStdLogicExpr tyIn dr)
    | (j,(_,tyIn,dr)) <- zip [0..] ins
    ]

-- hack to handle bit to vector with singleton bools.
genInst env i (Entity (Prim "concat") outs ins@[(n,B,_)]) =
        genInst env i (Entity (Prim "concat") 
                              (outs)
                              (ins ++ [("_",V 0,Lit (RepValue []))]))
        
genInst env i (Entity (Prim "concat") [("o0",_)] inps) =
                  [NetAssign (sigName "o0" i) val]
  where val = ExprConcat
                -- Note the the layout is reversed, because the 0 bit is on the right hand size
                [ toStdLogicExpr ty s | (_,ty, s) <- reverse inps ]

genInst env i (Entity (Prim "index")
		  [("o0",outTy)]
		  [("i0", GenericTy, (Generic idx)),
		   ("i1",ty@MatrixTy {},dr)
		  ]) = 
    [ NetAssign (sigName "o0" i) 
                (ExprIndex varname
                  (ExprLit Nothing
                   $ ExprNum
                   $ idx))]
   where                  
           -- we assume the expression is a var name (no constants here, initiaized at startup instead).
           (ExprVar varname) =  toStdLogicExpr ty dr
           
genInst env i (Entity (Prim "unconcat")  outs [("i0", ty@(MatrixTy n inTy), dr)]) 
   | length outs == n =
    [ NetAssign (sigName ("o" ++ show j) i) 
                (ExprIndex varname
                  (ExprLit Nothing
                   $ ExprNum
                   $ j))
    | (j,out) <- zip [0..] outs
    ]
   where                  
           -- we assume the expression is a var name (no constants here, initiaized at startup instead).
           (ExprVar varname) = toStdLogicExpr ty dr           

genInst env i (Entity (Prim "index")
		  [("o0",outTy)]
		  [("i0", GenericTy, Generic ix),
		   ("i1",eleTy,input)]) =
	[ NetAssign (sigName "o0" i)
                (case eleTy of
                    -- Not sure about way this works over two different types.
		    TupleTy tys -> prodSlices input tys !! (fromIntegral ix)
		    other -> error $ show ("genInst/index",other)
		)
	]

genInst env i (Entity (Prim "index")
		  [("o0",outTy)]
		  [("i0", ixTy, ix),
		   ("i1",eleTy,input)]) =
	[ NetAssign (sigName "o0" i)
		(ExprCase (toStdLogicExpr ixTy ix)
			[ ([toStdLogicExpr ixTy (i :: Integer)],toStdLogicExpr outTy val)
			| (i,val) <- zip [0..] $ prodSlices input tys
			]
			(Just $ toStdLogicExpr outTy (0 :: Integer))
		)
	]
  where tys = case eleTy of
                -- Not sure about way this works over two different types.
		MatrixTy sz eleTy -> take sz $ repeat eleTy
		TupleTy tys -> tys
		other -> error $ show ("genInst/index",other)


{-
genInst env i e@(Entity nm outs	ins) | newName nm /= Nothing = 
	genInst env i (Entity nm' outs (ins' ++ ins2))
   where
	expandEnv = [Prim "register",Prim "BRAM"]
	newName (Prim "register") = return $ Name "Memory" "register"
	newName (Prim "BRAM")     = return $ Name "Memory" "BRAM"
	newName _		  = Nothing

	Just nm' = newName nm
	
	ins' = [ p | p@(nm,ty,dr) <- ins, ty /= ClkDomTy ]
	p_id = shrink 
	       [ p_id
 	       | (_, ClkDomTy, Port "env" p_id) <- ins
	       ]
	shrink [p] = p
	shrink [p1,p2] | p1 == p2 = p1	-- two clocks, the same actual clock
	shrink p_ids = error $ "Clock domain problem " ++ show (i,e,p_ids)

	ins2 = case M.lookup p_id env of
	   	   Just (Entity (Prim "Env") _ ins_e) -> [ (nm,ty,dr) | (nm,ty,dr) <- ins_e ]
	   	   _ -> error $ "can not find clock domain for " ++ show (p_id,e)
-}	       

genInst env i e@(Entity (Name "Memory" "register") [("o0",_)] inputs) =
          [NetAssign input (toStdLogicExpr ty d) ]
  where output = sigName "o0" i
        input =  next output
	(ty,d) = head [ (ty,d) | ("i0",ty,d) <- inputs ]


-- Muxes
genInst env i (Entity (Prim "mux2") [("o0",_)] [("i0",cTy,Lit (RepValue [WireVal True])),("i1",tTy,t),("i2",fTy,f)])
	= [NetAssign (sigName "o0" i) (toStdLogicExpr tTy t)]
genInst env i (Entity (Prim "mux2") [("o0",_)] [("i0",cTy,Lit (RepValue [WireVal False])),("i1",tTy,t),("i2",fTy,f)])
	= [NetAssign (sigName "o0" i) (toStdLogicExpr fTy f)]
genInst env i (Entity (Prim "mux2") [("o0",_)] [("i0",cTy,c),("i1",tTy,t),("i2",fTy,f)])
	= [NetAssign (sigName "o0" i)
                     (ExprCond cond
                      (toStdLogicExpr tTy t)
                      (toStdLogicExpr fTy f))]
  where cond = ExprBinary Equals (toTypedExpr cTy c) (ExprLit Nothing (ExprBit T))

--------------------------------------------------------------------------------------------
-- Sampled
--------------------------------------------------------------------------------------------

-- TODO: check all arguments types are the same
genInst env i (Entity (Prim op) [("o0",ty@(SampledTy m n))] ins)
	| op `elem` ["+","-","*","negate"]
	= genInst env i (Entity (External $ "lava_sampled_" ++ sanitizeName op) [("o0",ty)]
				        (ins ++ [ ("frac_width", 
				                        GenericTy, 
				                        Generic $ fromIntegral $ n - (1 + log2 m))
					        , ("width",GenericTy, Generic $ fromIntegral n)
					        ]))
                where
                        -- Use the log of the resolution + 1 bit for sign
                        log2 1 = 0
                        log2 n | n > 1 = 1 + log2 (n `div` 2)

-- For compares, we need to use one of the arguments.
-- With fixed width, we can just consider the bits to be "signed".
genInst env i (Entity (Prim op) [("o0",B)] [("i0",SampledTy m n,d0),("i1",SampledTy m' n',d1)])
	| op `elem` [".>.",".<.",".>=.",".<=."] && m == m' && n == n
        = genInst env i $ Entity (Prim op) [("o0",B)] [("i0",S n,d0),("i1",S n',d1)]

-- This is only defined over constants that are powers of two.
genInst env i (Entity (Prim "/") [("o0",oTy@(SampledTy m n))] [ ("i0",iTy,v), ("i1",iTy',Lit lit)])
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

genInst env i (Entity nm outputs inputs)
	| nm `elem` [ Name "Memory" "BRAM"
		    ]
	= []

-- Logic assignments

genInst env i (Entity n@(Prim "fromStdLogicVector") [("o0",t_out)] [("i0",t_in,w)]) =
	case (t_in,t_out) of
	   (V n,U m) | n == m ->
		[ NetAssign  (sigName "o0" i) (toStdLogicExpr t_in w)
		]
	   (V n,V m) | n == m ->
		[ NetAssign  (sigName "o0" i) (toStdLogicExpr t_in w)
		]
	   (V n,MatrixTy m B) | n == m ->
		[ NetAssign  (sigName "o0" i) (toStdLogicExpr t_in w)
		]
	   (V n,SampledTy _ m) | n == m ->
		[ NetAssign  (sigName "o0" i) (toStdLogicExpr t_in w)
		]
	   _ -> error $ "fatal : converting from " ++ show t_in ++ " to " ++ show t_out ++ " using fromStdLogicVector failed"
genInst env i (Entity n@(Prim "toStdLogicVector") [("o0",t_out)] [("i0",t_in,w)]) =
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
	   (MatrixTy n B,V m) | n == m ->
		[ NetAssign  (sigName "o0" i) $ (toStdLogicExpr t_in w)
		]
	   (B,V 1) ->
		[ NetAssign  (sigName "o0" i ++ "(0)") $ (toStdLogicExpr t_in w) -- complete hack
		]
	   _ -> error $ "fatal : converting from " ++ show t_in ++ " to " ++ show t_out ++ " using toStdLogicVector failed"


-- <= x(7 downto 2)

genInst env i (Entity n@(Prim "spliceStdLogicVector") [("o0",V outs)] [("i0",_,Generic x),("i1",V ins,w)])
{-
	| outs < (ins - fromIntegral x) 
	= 
	-- TODO: Still needs more work here to cover all cases
	[ NetAssign  (sigName "o0" i) 
		$ ExprConcat
			[ ExprSlice nm (ExprLit Nothing (ExprNum $ high)) (ExprLit Nothing (ExprNum low))
			, ExprLit Nothing (ExprNum 1234567)
			]
	]
-}

	| null zs =
	[ NetAssign  (sigName "o0" i) $ slice
	]
	| otherwise =
	[ NetAssign  (sigName "o0" i) $	ExprConcat 
		[ ExprLit (Just $ length zs) $ ExprBitVector [ F | _ <- zs ]
		, slice
		]
	]
		
  where
     xs = take outs [x..]
     ys = take (ins - fromIntegral x) xs 
     zs = drop (ins - fromIntegral x) xs

     slice = ExprSlice nm (ExprLit Nothing (ExprNum $ last ys)) (ExprLit Nothing (ExprNum $ head ys))


     nm = case toTypedExpr (V ins) w of
  	    ExprVar n -> n
	    other -> error $ " problem with spliceStdLogicVector " ++ show w

     high = fromIntegral x + fromIntegral outs - 1
     low = fromIntegral x


--------------------------------------------------------------------------------
-- Basic Coerce, with truncation and zero padding
--------------------------------------------------------------------------------

genInst env i (Entity (Prim "coerce") [("o0",tO)] [("i0",tI,w)])
        | typeWidth tI >= typeWidth tO =
	[ NetAssign  (sigName "o0" i) $ 
                ExprSlice nm (ExprLit Nothing (ExprNum (fromIntegral (typeWidth tO - 1)))) (ExprLit Nothing (ExprNum 0))
	]                
        | otherwise =
	[ NetAssign  (sigName "o0" i) $	ExprConcat 
		[ ExprLit (Just $ zeros) $ ExprBitVector $ take zeros$ repeat F
		, ExprVar nm
		]
	]
  where
     zeros = typeWidth tO - typeWidth tI
     nm = case toStdLogicExpr tI w of
	    ExprVar n -> n
	    other -> error $ " problem with coerce: " ++ show (w,tI,other)

--------------------------------------------------------------------------------
-- Arith
--------------------------------------------------------------------------------

genInst env i (Entity (Prim "*") outs@[("o0",U n)] ins) =
        genInst env i $ Entity (External "lava_unsigned_mul")
                                outs 
                                (ins ++ [("width",GenericTy,Generic $ fromIntegral n)])
genInst env i (Entity (Prim "*") outs@[("o0",S n)] ins) =
        genInst env i $ Entity (External "lava_signed_mul")
                                outs 
                                (ins ++ [("width",GenericTy,Generic $ fromIntegral n)])

-- negate of unsigned things (under Haskell) treats the bits not like logicial negate, 
-- but 2s complement negate. So we treat it as such.
genInst env i (Entity (Prim "negate") [("o0",U n)] [("i0",U m,dr)]) =
        genInst env i (Entity (Prim "negate") [("o0",S n)] [("i0",S m,dr)])

-- The specials (from a table). Only Prim's can be special.
-- To revisit RSN.

genInst env i (Entity (Prim ".==.") 
                [("o0",B)]
                [ ("i0",ty0,dr0)
                , ("i1",ty1,dr1)
                ]) | typeWidth ty0 == 0
        =
        [ NetAssign (sigName "o0" i) (ExprLit Nothing (ExprBit T))
        ]


genInst env i (Entity n@(Prim _) [("o0",oTy)] ins)
        | Just (NetlistOp arity f) <- lookup n specials, arity == length ins =
          [NetAssign  (sigName "o0" i)
                  (f oTy [(inTy, driver)  | (_,inTy,driver) <- ins])]


--------------------------------------------------------------------------------
-- Clocked primitives
--------------------------------------------------------------------------------

{-
genInst env i (Entity (Prim "delay") 
                outs@[("o0",_)] 
                (("i0",ty2,Port "o0" read_id):ins_reg)) 
  | Maybe.isJust async =        -- TODO: need to also check default for undefine-ness
        case async_ins of
          [("i0",ty,Port "o0" write_id),("i1",ty2,dr2)] ->
            case M.lookup write_id env of
              Just (Entity (Prim "write") _ ins_write) -> 
                genInst env i $ Entity (Prim "RAM") 
                                 outs
                                 (checkClock ins_write ++ 
                                        [ ("sync",GenericTy,Generic 1)
                                        , ("rAddr",ty2,dr2)
                                        ])
             
              o -> error ("found a sync/read without a write in code generator " ++ show (i,write_id,o))
   where
          -- TODO: add check for same clock domain
        checkClock ins_write = ins_write  
        async = case M.lookup read_id env of
                   Just (Entity (Prim "asyncRead") _ ins) -> Just ins
                   _ -> Nothing 
        async_ins = Maybe.fromJust async
-}

genInst env i (Entity (Prim "write") [ ("o0",ty) ]
                                     [ ("clk_en",B,clk_en)
                                     , ("clk",ClkTy,clk)
                                     , ("rst",B,rst)
                                     , ("wEn",B,wEn)
                                     , ("wAddr",wAddrTy,wAddr)
                                     , ("wData",wDataTy,wData)
                                      ,("element_count",GenericTy,n)            -- now ignored?
                                      ]) =
        [ ProcessDecl
         [ ( Event (toStdLogicExpr B clk) PosEdge
           , If (isHigh (toStdLogicExpr B clk_en))
                (If (isHigh (toStdLogicExpr B wEn))
                    (statements 
                       [Assign (ExprIndex (sigName "o0" i)
                                          (toMemIndex wAddrTy wAddr))
                               (stdLogicToMem wDataTy $ toStdLogicExpr wDataTy wData)
                       ])
                       Nothing)
                Nothing
           )
         ]
        ]                                              


-- assumes single clock
genInst env i (Entity (Prim "delay") outs@[("o0",ty)]    [ ("i0",tI,d)
                                                         , ("clk_en",B,clk_en)
                                                         , ("clk",ClkTy,clk)
                                                         , ("rst",B,rst)
                                                         ]) =
        [ ProcessDecl
         [ ( Event (toStdLogicExpr B clk) PosEdge
           , If (isHigh (toStdLogicExpr B clk_en))
                (statements [Assign (ExprVar $ sigName "o0" i) (toStdLogicExpr tI d)])
                Nothing
           )
         ]
        ]

genInst env i (Entity (Prim "register") outs@[("o0",ty)] [ ("i0",tI,d)
                                                         , ("def",GenericTy,n)
                                                         , ("clk_en",B,clk_en)
                                                         , ("clk",ClkTy,clk)
                                                         , ("rst",B,rst)
                                                         ]) =
        [ ProcessDecl
         [ (Event (toStdLogicExpr B rst) AsyncHigh
           , statements [Assign (ExprVar $ sigName "o0" i) (toTypedExpr ty n)]
           )
         , ( Event (toStdLogicExpr B clk) PosEdge
           , If (isHigh (toStdLogicExpr B clk_en))
		(statements [Assign (ExprVar $ sigName "o0" i) (toStdLogicExpr tI d)])
                Nothing
           )
         ]
        ]

{-
-- OLD CODE
genInst env i (Entity (Prim "delay") outs@[("o0",ty)] ins) =
     case toStdLogicTy ty of
	B   -> genInst env i $ boolTrick ["i0","o0"] (inst 1)
	V n -> genInst env i $ inst n
	_ -> error $ "delay typing issue (should not happen)"
  where 
        inst n = Entity 
                    (External "lava_delay") 
                    outs 
		    (ins ++ [("width",GenericTy,Generic $ fromIntegral n)])

genInst env i (Entity (Prim "register") outs@[("o0",ty)] ins) =
     case toStdLogicTy ty of
	B   -> genInst env i $ boolTrick ["i0","o0"] (inst 1)
	V n -> genInst env i $ inst n
	_ -> error $ "register typing issue  (should not happen)"
  where 
        inst n = Entity 
                    (External "lava_register") 
                    outs 
		    (ins ++ [("width",GenericTy,Generic $ fromIntegral n)])
-}

-- A bit of a hack to handle Bool or zero-width arguments.
genInst env i (Entity (Prim "RAM") outs@[("o0",data_ty)] ins) | goodAddrType addr_ty = 
   case (toStdLogicTy data_ty,toStdLogicTy addr_ty) of
	(V n, V 0) -> genInst env i $ zeroArg $ inst n 1
	(B  , V 0) -> genInst env i $ boolTrick ["wData","o0"] $ zeroArg $ inst 1 1
	(B  , V m) -> genInst env i $ boolTrick ["wData","o0"] $ inst 1 m
	(V n, V m) -> genInst env i $ inst n m
	_ -> error $ "RAM typing issue (should not happen)"
 where
        ("rAddr",addr_ty,d) = last ins

{-
        rAddr = case d of
                  Port "o0" register_id ->
                    case M.lookup register_id env of
                        Just (Entity (Prim "register") _ ins) -> 
                       _ -> 
                  _ -> error $ ("rAddr",d)
-}
        inst n m = Entity 
                    (External "lava_bram") 
                    outs 
		    (ins ++ [("data_width",GenericTy,Generic $ fromIntegral n)
			    ,("addr_width",GenericTy,Generic $ fromIntegral m)
			    ])
        zeroArg (Entity nm outs ins) = 
                        Entity nm outs $
                               [ (n,V 1,Lit $ RepValue [WireVal False])
                               | n <- ["wAddr","rAddr"]
                               ] ++
                               [ (n,t,d) | (n,t,d) <- ins, n /= "wAddr" 
                                                        && n /= "rAddr"
                               ]
        goodAddrType ty = 
                case ty of
                  U n -> True
                  _   -> error $ "unsupported address type for BRAMs: " ++ show ty
        


-- For read, we find the pairing write, and call back for "RAM".
-- This may produce multiple RAMs, if there are multiple reads.

-- This will be called index later.

genInst env i (Entity (Prim "asyncRead") 
                outs@[("o0",ty)] 
                [ ("i0",ty1@MatrixTy {},dr1)
                , ("i1",ty2,dr2)
                ]) =
   case dr1 of
     Port v n ->
        [NetAssign  (sigName "o0" i)
                    (memToStdLogic ty $
                       ExprIndex (sigName v (fromIntegral n))
                                  (toMemIndex ty2 dr2)
                    )
      ]
     _ -> error "bad array as input to asyncRead"
 where
    MatrixTy x (V y) = toStdLogicTy ty1

{-
genInst env i (Entity (Prim "asyncRead") 
                outs@[("o0",ty)] 
                [ ("i0",ty1,Port "o0" read_id)
                , ("i1",ty2,dr2)
                ]) =
  case M.lookup read_id env of
     Just (Entity (Prim "write") _ ins) -> 
        genInst env i (Entity (Prim "RAM") outs (ins ++ [ ("sync",GenericTy,Generic 0)
                                                        , ("rAddr",ty2,dr2)
                                                        ]))
     o -> error ("found a read without a write in code generator " ++ show (i,read_id,o))
-}


genInst env i (Entity (Prim "rom") [("o0",MatrixTy {})] [(_,RomTy {},_)]) = 
        [ CommentDecl (sigName "o0" i ++ " is a constant array") ]

--------------------------------------------------------------------------------

-- And the defaults

-- Right now, we *assume* that every external entity
-- has in and outs of type std_logic[_vector].

genInst env i (Entity n@(External nm) outputs inputs) =
	trace (show ("mkInst",n,[ t | (_,t) <- outputs ],[ t | (_,t,_) <- inputs ])) $
          [ InstDecl nm ("inst" ++ show i)
		[ (n,case x of
			Generic v -> ExprLit Nothing (ExprNum v)
			_ -> error $ "genInst, Generic, " ++ show (n,nTy,x)
	          )
		| (n,nTy,x) <- inputs, isGenericTy nTy
		]
                [ (n,toStdLogicExpr nTy x)  | (n,nTy,x) <- inputs, not (isGenericTy nTy) ]
		[ (n,ExprVar $ sigName (fixName nTy n) i) | (n,nTy)   <- outputs ]
          ]
   where isGenericTy GenericTy = True
         isGenericTy _         = False

         -- A hack to match 'boolTrick'. Should think again about this
         -- Think of this as a silent (0) at the end of the right hand size.
         fixName B nm | "(0)" `isSuffixOf` nm = reverse (drop 3 (reverse nm))
         fixName _ nm = nm



genInst env i (Entity n@(Name mod_nm nm) outputs inputs) =
	trace (show ("mkInst",n,[ t | (_,t) <- outputs ],[ t | (_,t,_) <- inputs ])) $
          [ InstDecl (mod_nm ++ "_" ++ sanitizeName nm) ("inst" ++ show i)
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

genInst env i tab@(Entity (Function mp) [(vout,tyout)] [(vin,tyin,d)]) =
	[ NetAssign (sigName vout i)
		(ExprCase (toStdLogicExpr tyin d)
			[ ([toStdLogicExpr tyin ix],toStdLogicExpr tyout val)
			| (ix,val) <- mp
			]
			(Just $ toStdLogicExpr tyout (0 :: Integer))	-- replace with unknowns
		)
	]

genInst env i other = error $ show ("genInst",i,other)


--------------------------------------------------------------

data NetlistOperation = NetlistOp Int (Type -> [(Type,Driver Unique)] -> Expr)

mkSpecialUnary
	:: (Type -> Expr -> Expr)
	-> (Type -> Driver Unique -> Expr)
	-> [(String, UnaryOp)]
	-> [(Id, NetlistOperation)]
mkSpecialUnary coerceR coerceF ops =
       [( Prim lavaName
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
       [( Prim lavaName
	, NetlistOp 2 $ \ fTy [(lty,l),(rty,r)] ->
		coerceR fTy (ExprBinary netListOp
					(coerceF lty l)
					(coerceF rty r))

	)
         | (lavaName,netListOp) <- ops
         ]

mkSpecialShifts ops =
    [(Prim lavaName
      , NetlistOp 2 ( \ fTy [(lty,l),(rty,r)] ->
                          toStdLogicExpr fTy $ ExprFunCall funName [toTypedExpr lty l, toIntegerExpr rty r])
     )
    | (lavaName, funName) <- ops
    ]

-- testBit returns the bit-value at a specific (constant) bit position
-- of a bit-vector.
-- This generates:    invar(indexVal);
mkSpecialTestBit =
    [(Prim lavaName
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
	, ("/", Divide)
	]
   ++ mkSpecialBinary (\ _ e -> e) toStdLogicExpr
        [ (".|.",Or), (".&.",And), (".^.",Xor)
	, ("or2",Or), ("and2",And), ("xor2",Xor)
	]
   ++ mkSpecialUnary  toStdLogicExpr toTypedExpr
	[("negate",Neg)]
   ++ mkSpecialUnary  (\ _ e -> e) toStdLogicExpr
	[("not",LNeg)
	,("complement",LNeg)
	]
   ++   mkSpecialTestBit
   ++   mkSpecialShifts
        [ ("shiftL", "shift_left")
        , ("shiftR", "shift_right")
        , ("rotateL", "rotate_left")
        , ("rotateR", "rotate_right")
        ]
