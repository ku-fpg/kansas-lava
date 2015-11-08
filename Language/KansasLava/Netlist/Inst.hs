{-# LANGUAGE PatternGuards #-}
-- | The 'Inst' module generates Netlist instances for each 'Entity' in a Lava
-- circuit.
module Language.KansasLava.Netlist.Inst(genInst') where

import Language.KansasLava.Types
import Language.Netlist.AST hiding (U)
import Language.Netlist.Util
import Language.KansasLava.Rep
import qualified Data.Map as M
import Data.Bits

import Data.List
import Data.Reify.Graph (Unique)

import Language.KansasLava.Netlist.Utils

import Debug.Trace

-- | Generate Netlist Insts for Lava entities.
genInst' :: M.Map Unique (Entity Unique)
         -> Unique
         -> Entity Unique
         -> [Decl]
genInst' env i e =
--	(CommentDecl $ show (i,e)):
	genInst env i e
genInst :: M.Map Unique (Entity Unique) -> Unique -> Entity Unique -> [Decl]

-- (Commented out) debugging hook
-- genInst env i en | trace (show ("genInst",en)) False = undefined

-- Some entities never appear in output (because they are virtual)
--genInst env i (Entity nm ins outs) | nm `elem` isVirtualEntity = []

-- You never actually write something that is zero width.
genInst _ _ (Entity _ [(_,ty)] _) | toStdLogicTy ty == V 0 = []

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


-- Blackbox nodes should have been removed by reification, but alas, no.
genInst env i (Entity (BlackBox _) ins outs) =
  genInst env i (Entity (Prim "id") ins outs)

genInst env i (Entity (Prim "retime") outs [("i0",ty,dr),("pulse",_,_)]) =
    genInst env i (Entity (Prim "id") outs [("i0",ty,dr)])

genInst _ _ (Entity (Comment comments) [] []) =
        [ CommentDecl (unlines comments)
        ]
genInst env i (Entity (Comment comments) ins@[_] outs@[_]) =
        CommentDecl (unlines comments) :
	genInst env i (Entity (Prim "id") ins outs)

genInst env i (Entity (Prim "const") outputs [in0,_])
	= genInst env i (Entity (Prim "id") outputs [in0])

genInst env i (Entity (Prim "pair") outputs inputs)
	= genInst' env i (Entity (Prim "concat") outputs inputs)
genInst env i (Entity (Prim "triple") outputs inputs)
	= genInst' env i (Entity (Prim "concat") outputs inputs)


genInst env i (Entity (Prim "fst") outputs inputs)
	= genInst env i (Entity (Prim "project") outputs (addNum 0 inputs))
genInst env i (Entity (Prim "snd") outputs inputs)
	= genInst' env i (Entity (Prim "project") outputs (addNum 1 inputs))
genInst env i (Entity (Prim "fst3") outputs inputs)
	= genInst env i (Entity (Prim "project") outputs (addNum 0 inputs))
genInst env i (Entity (Prim "snd3") outputs inputs)
	= genInst env i (Entity (Prim "project") outputs (addNum 1 inputs))
genInst env i (Entity (Prim "thd3") outputs inputs)
	= genInst env i (Entity (Prim "project") outputs (addNum 2 inputs))


-- identity

genInst _ i (Entity (Prim "id") [(vO,tyO)] [(_,tyI,d)]) =
        case toStdLogicTy tyO of
           MatrixTy n (V _)
             -- no need to coerce n[B], because both sides have the
             -- same representation
             -> [  MemAssign (sigName vO i) (ExprLit Nothing $ ExprNum j)
                        $ ExprIndex varname
                                (ExprLit Nothing $ ExprNum j)
                | j <- [0..(fromIntegral n - 1)]
                ]
           _ -> [  NetAssign (sigName vO i) $ toStdLogicExpr tyI d ]
  where
     -- we assume the expression is a var name for matrix types (no constants here)
     (ExprVar varname) =  toStdLogicExpr tyI d

-- Concat and index (join, project)

genInst _ i (Entity (Prim "concat") [("o0",ty)] ins)
        | case toStdLogicTy ty of
            MatrixTy {} -> True
            _ -> False
        =
        [ MemAssign
                (sigName "o0" i)
                (ExprLit Nothing $ ExprNum j)
                (stdLogicToMem tyIn $ toStdLogicExpr tyIn dr)
    | (j,(_,tyIn,dr)) <- zip [0..] ins
    ]

-- hack to handle bit to vector with singleton bools.
genInst env i (Entity (Prim "concat") outs ins@[(_,B,_)]) =
        genInst env i (Entity (Prim "concat")
                              outs
                              (ins ++ [("_",V 0,Lit (RepValue []))]))

genInst _ i (Entity (Prim "concat") [("o0",_)] inps) =
                  [ CommentDecl (show inps)
		 ,  NetAssign (sigName "o0" i) val]
  where val = ExprConcat
                -- Note the the layout is reversed, because the 0 bit is on the right hand size
                [ toStdLogicExpr ty s | (_,ty, s) <- reverse inps ]

genInst _ i (Entity (Prim "index")
		  [("o0",_)]
		  [("i0", GenericTy, Generic idx),
		   ("i1",ty@MatrixTy {},dr)
		  ]) =
    [ NetAssign (sigName "o0" i)
		(reverse vs !! (fromIntegral idx))
    ]
   where
           -- we assume the expression is a var name (no constants here, initiaized at startup instead).
	   ExprConcat vs = toStdLogicExpr ty dr

genInst _ i e@(Entity (Prim "index")
		  [("o0",t)]
		  [("i0",  ixTy, ix),
		   ("i1",ty@MatrixTy {},dr)
		  ]) =
    [ NetAssign (sigName "o0" i)
                (memToStdLogic t
                   (ExprIndex varname
                      (toMemIndex ixTy ix)))
    ]
   where
           -- we assume the expression is a var name (no constants here, initiaized at startup instead).
	   varname = case toStdLogicExpr ty dr of
		        ExprVar v -> v
			other -> case dr of
				   Port v n -> sigName v (fromIntegral n)
				   _ -> error (show ("genInst/index",e,other))

genInst _ i (Entity (Prim "unconcat")  outs [("i0", ty@(MatrixTy n inTy), dr)])
   | length outs == n =
    [ NetAssign (sigName ('o':show j) i)
                (memToStdLogic inTy
                  (ExprIndex varname
                    (ExprLit Nothing $ ExprNum j)))
    | (j,_) <- zip [0..] outs
    ]
   where
           -- we assume the expression is a var name (no constants here, initiaized at startup instead).
           (ExprVar varname) = toStdLogicExpr ty dr

genInst _ i e@(Entity (Prim "project")
		  [("o0",tyOut)]
		  [("i0", GenericTy, Generic ix),
		   ("i1",TupleTy tys,input)]) =
  case toStdLogicType tyOut of
     SL ->
        [ NetAssign (sigName "o0" i)
                    (prodSlices input tys !! fromIntegral ix)
	]
     SLV _n ->
        [ NetAssign (sigName "o0" i)
                    (prodSlices input tys !! fromIntegral ix)
	]
     SLVA n _ ->
	    -- The trick here is to expand out the matrix to be
	    -- imbeaded in the tuple, then to project from there.
	    -- So (B,2[U4]) ==> (B,U4,U4)
	let tys' = concat
		   [ case ty of
		       MatrixTy n' ty' | j == ix -> replicate n' ty'
		       _               | j == ix -> error "found a non-Matrix project to a Matrix"
		       _ -> [ ty ]
		   | (ty,j) <- zip tys [0..] ]
	    slices = prodSlices input tys'
	    select = take n . drop (fromIntegral ix)
	in
	    [ MemAssign (sigName "o0" i)
			(ExprLit Nothing $ ExprNum $ j)
			(stdLogicToMem ty' slice)
    	   | (j,(ty',slice)) <- zip [0..]
			      (select (zip tys' slices))
    	   ]
     _ -> error $ show ("project",e)

{-
genInst _ i (Entity (Prim "index")
		  [("o0",outTy)]
		  [("i0", ixTy, ix),
		   ("i1",eleTy,input)]) =
	[ NetAssign (sigName "o0" i)
		(ExprCase (toStdLogicExpr ixTy ix)
			[ ([toStdLogicExpr ixTy (idx :: Integer)],toStdLogicExpr outTy val)
			| (idx,val) <- zip [0..] $ prodSlices input tys
			]
			(Just $ toStdLogicExpr outTy (0 :: Integer))
		)
	]
  where tys = case eleTy of
                -- Not sure about way this works over two different types.
		MatrixTy sz eleTy' -> replicate sz eleTy'
		TupleTy tys' -> tys'
		other -> error $ show ("genInst/index",other)
-}

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

-- Muxes
genInst _ i (Entity (Prim "mux") [("o0",ty)] [("i0",_,Lit (RepValue [Just True])),("i1",fTy,_),("i2",tTy,t)])
	| ty == tTy && ty == fTy
	= assignDecl "o0" i ty $ \ toExpr -> toExpr t
genInst _ i (Entity (Prim "mux") [("o0",ty)] [("i0",_,Lit (RepValue [Just False])),("i1",fTy,f),("i2",tTy,_)])
	| ty == tTy && ty == fTy
	= assignDecl "o0" i ty $ \ toExpr -> toExpr f
genInst _ i (Entity (Prim "mux") [("o0",ty)] [("i0",cTy,c),("i1",fTy,f),("i2",tTy,t)])
	| ty == tTy && ty == fTy
	= assignDecl "o0" i ty $ \ toExpr ->
                     (ExprCond cond
                      (toExpr t)
                      (toExpr f))
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
				                        Generic $ fromIntegral $ n - log2 m)
					        , ("width",GenericTy, Generic $ fromIntegral n)
					        ]))


-- For compares, we need to use one of the arguments.
-- With fixed width, we can just consider the bits to be "signed".
genInst env i (Entity (Prim op) [("o0",B)] [("i0",SampledTy m n,d0),("i1",SampledTy m' n',d1)])
	| op `elem` [".>.",".<.",".>=.",".<=."] && m == m' && n == n
        = genInst env i $ Entity (Prim op) [("o0",B)] [("i0",S n,d0),("i1",S n',d1)]

-- This is only defined over constants that are powers of two.
genInst _ i (Entity (Prim "/") [("o0",SampledTy m n)] [ ("i0",iTy,v), ("i1",_,Lit lit)])
--	= trace (show n)
        | (val' `mod` (2^frac_width) == 0) && (2^(log2 val - 1) == val)
	= [ InstDecl "Sampled_fixedDivPowOfTwo" ("inst" ++ show i)
  		[ ("shift_by",ExprLit Nothing (ExprNum $ log2 val - 1))
                , ("frac_width", ExprLit Nothing  $ ExprNum $ fromIntegral frac_width)
		, ("width", ExprLit Nothing  $ ExprNum $ fromIntegral n)
                ]
                [ ("i0",toStdLogicExpr iTy v)
                ]
		[ ("o0",ExprVar $ sigName "o0" i) ]
          ]
  where val' = fromRepToInteger lit
        val  = val' `div` (2 ^ frac_width)
        frac_width = n - log2 m

-- Logic assignments
{-
genInst _ i (Entity (Prim "fromStdLogicVector") [("o0",t_out)] [("i0",t_in,w)]) =
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
genInst _ i (Entity (Prim "toStdLogicVector") [("o0",t_out)] [("i0",t_in,w)]) =
	case (t_in,t_out) of
	   (U n,V m) | n == m ->
		[ NetAssign  (sigName "o0" i) $ toStdLogicExpr t_in w
		]
	   (V n,V m) | n == m ->
		[ NetAssign  (sigName "o0" i) $ toStdLogicExpr t_in w
		]
	   (SampledTy _ n,V m) | n == m ->
		[ NetAssign  (sigName "o0" i) $ toStdLogicExpr t_in w
		]
	   (MatrixTy n B,V m) | n == m ->
		[ NetAssign  (sigName "o0" i) $
                    ExprConcat [ memToStdLogic B
                                 (ExprIndex (slvVarName t_in w)
                                  (ExprLit Nothing $ ExprNum $ fromIntegral j)
                                 )
                                 | j <- reverse [0..(m-1)]
                               ]
		]
	   (B,V 1) ->
		[ NetAssign  (sigName "o0" i ++ "(0)") $ toStdLogicExpr t_in w -- complete hack
		]
	   _ -> error $ "fatal : converting from " ++ show t_in ++ " to " ++ show t_out ++ " using toStdLogicVector failed"
-}

-- <= x(7 downto 2)

genInst _ i (Entity (Prim "spliceStdLogicVector") [("o0",V outs)] [("i0",_,Generic x),("i1",V ins,w)])
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
	[ NetAssign  (sigName "o0" i) slice
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
	    _ -> error $ " problem with spliceStdLogicVector " ++ show w



--------------------------------------------------------------------------------
-- Basic Coerce, with truncation and zero padding
--------------------------------------------------------------------------------

-- coerce only works betwen things of the same width.
-- 9 possible coercions, because we have 3 representaitions.

genInst env i (Entity (Prim "coerce") [("o0",tO)] [("i0",tI,w)])
        | typeWidth tI == typeWidth tO =
        case (toStdLogicTy tI,toStdLogicTy tO) of
          (a,b) | a == b -> genInst env i (Entity (Prim "id") [("o0",tO)] [("i0",tI,w)])
          (MatrixTy 1 (V 1),B) ->
		[ NetAssign  (sigName "o0" i)
		             (toStdLogicExpr' tI w)
		]
          (MatrixTy _ _,V _) ->
		[ NetAssign  (sigName "o0" i)
			     (toStdLogicExpr tI w)
		]

          (B,MatrixTy 1 (V 1)) ->
                [  MemAssign (sigName "o0" i) (ExprLit Nothing $ ExprNum 0)
                        $ stdLogicToMem B
                        $ toStdLogicExpr tI w
                ]
          (B,V 1) ->
                [ NetAssign  (sigName "o0" i)
                        $ stdLogicToMem B
                        $ toStdLogicExpr tI w
                ]

          (V _,MatrixTy n0 (V n1)) ->
                [  MemAssign (sigName "o0" i) (ExprLit Nothing $ ExprNum $ fromIntegral $j)
                        -- This is 'B' because a V is split into an array of B.
                        $ ExprSlice (slvVarName tI w)
                                (ExprLit Nothing $ ExprNum $ fromIntegral $ (j + 1) * n1 - 1)
                                (ExprLit Nothing $ ExprNum $ fromIntegral $ j * n1)
                | j <- [0..(n0 - 1)]
                ]
          (V 1,B) ->
                [ NetAssign  (sigName "o0" i)
                        $ memToStdLogic B
                        $ toStdLogicExpr tI w
                ]
          other -> error $ "coerce failure: " ++ show other

        | otherwise = error $ "coerce attempting to resize : " ++ show (tO,tI)

genInst _ i (Entity (Prim "unsigned") [("o0",tO)] [("i0",tI,w)])
        | isMatrixStdLogicTy tI = error "input of unsigned uses matrix representation"
        | isMatrixStdLogicTy tO = error "output of unsigned uses matrix representation"
        | typeWidth tI == typeWidth tO && tI == B && isStdLogicVectorTy tO =
	[ NetAssign  (sigName "o0" i) $ mkExprConcat $ [(tI,ExprVar nm)]
	]
        | typeWidth tI == typeWidth tO =
	[ NetAssign  (sigName "o0" i) $ toStdLogicExpr tI w
	]
        | typeWidth tI > typeWidth tO =
	[ NetAssign  (sigName "o0" i) $
                case toStdLogicExpr tI w of
                  ExprVar nm' -> ExprSlice nm' (ExprLit Nothing (ExprNum (fromIntegral (typeWidth tO - 1))))
                                               (ExprLit Nothing (ExprNum 0))
                  ExprLit _ (ExprNum n) ->
                                toTypedExpr
                                        tO
                                        n -- TODO: should mod with 2^(width of tO)
                  other -> error $ "(signed) problem , tI > tO, "  ++ show (w,tI,tO,other)
	]
        | typeWidth tI < typeWidth tO =
	[ NetAssign  (sigName "o0" i) $	ExprConcat
		[ ExprLit (Just zeros) $ ExprBitVector $ replicate zeros F
		, ExprVar nm
		]
	]
  where
     zeros = typeWidth tO - typeWidth tI
     nm = case toStdLogicExpr tI w of
	    ExprVar n -> n
	    other -> error $ " problem with unsigned: " ++ show (w,tI,tO,other)
{-
     lit = case opt_lit of
            Just v -> v
            _ -> error "not lit"

     isLit = isJust opt_lit

     opt_lit = case toStdLogicExpr tI w of
	          (ExprLit _ (ExprNum n)) -> return n
                  _ -> fail "not ExprLit _ (ExprNum _)"
-}

genInst _ i (Entity (Prim "signed") [("o0",tO)] [("i0",tI,w)])
        | isMatrixStdLogicTy tI = error "input of signed uses matrix representation"
        | isMatrixStdLogicTy tO = error "output of signed uses matrix representation"
        | typeWidth tI == typeWidth tO && tI == B && isStdLogicVectorTy tO =
	[ NetAssign  (sigName "o0" i) $ mkExprConcat $ [(tI,ExprVar nm)]
	]
        | typeWidth tI == typeWidth tO =
	[ NetAssign  (sigName "o0" i) $ toStdLogicExpr tI w
	]
        | typeWidth tI > typeWidth tO =
	[ NetAssign  (sigName "o0" i) $
                ExprSlice nm (ExprLit Nothing (ExprNum (fromIntegral (typeWidth tO - 1)))) (ExprLit Nothing (ExprNum 0))
	]
        | otherwise =
	[ NetAssign  (sigName "o0" i) $	ExprConcat $
                replicate zeros
                  (ExprIndex nm (ExprLit Nothing (ExprNum (fromIntegral (typeWidth tI - 1)))))
                  ++
		[ ExprVar nm
		]
	]
  where
     zeros = typeWidth tO - typeWidth tI
     nm = case toStdLogicExpr tI w of
	    ExprVar n -> n
	    other -> error $ " problem with signed: " ++ show (w,tI,tO,other)


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

genInst _ i (Entity (Prim ".==.")
                [("o0",B)]
                [ ("i0",ty0,_)
                , ("i1",_,_)
                ]) | typeWidth ty0 == 0
        =
        [ NetAssign (sigName "o0" i) (ExprLit Nothing (ExprBit T))
        ]


genInst _ i (Entity n@(Prim _) [("o0",oTy)] ins)
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

genInst _ i (Entity (Prim "write") [ ("o0",_) ]
                                     [ ("clk",ClkTy,clk)
                                     , ("rst",B,_)
                                     , ("wEn",B,wEn)
                                     , ("wAddr",wAddrTy,wAddr)
                                     , ("wData",wDataTy,wData)
                                     , ("element_count",GenericTy,_)            -- now ignored?
				     , ("clk_en",B,clk_en)
                                      ]) =
        [ mkProcessDecl
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
genInst _ i (Entity (Prim "delay") [("o0",ty)]    [ ("i0",tI,d)
                                                  , ("clk",ClkTy,clk)
                                                  , ("rst",B,_)
                                                  , ("clk_en",B,clk_en)
                                                  ]) | ty == tI =
        [ mkProcessDecl
         [ ( Event (toStdLogicExpr B clk) PosEdge
           , If (isHigh (toStdLogicExpr B clk_en))
		(assignStmt "o0" i tI d)
                Nothing
           )
         ]
        ]

genInst _ i (Entity (Prim "register") [("o0",ty)] [ ("i0",tI,d)
                                                  , ("def",GenericTy,n)
                                                  , ("clk",ClkTy,clk)
                                                  , ("rst",B,rst)
                                                  , ("clk_en",B,clk_en)
                                                  ]) | ty == tI =
        [ ProcessDecl
           (Event (toStdLogicExpr B clk) PosEdge)
	   (let rst_code = Just ( Event (toStdLogicExpr B rst) PosEdge
                 	        , assignStmt "o0" i ty n
                 	        )
	    in case rst of
	      Port {} -> rst_code
	      Pad {} -> rst_code
	      Lit (RepValue [Just False]) -> Nothing
	      _ -> error "genInst 'register' has strange reset value"
           )
           ( If (isHigh (toStdLogicExpr B clk_en))
		(assignStmt "o0" i tI d)
                Nothing
           )
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
genInst env i (Entity (Prim "RAM") outputs@[("o0",data_ty)] inputs) | goodAddrType addr_ty =
   case (toStdLogicTy data_ty,toStdLogicTy addr_ty) of
	(V n, V 0) -> genInst env i $ zeroArg $ inst n 1
	(B  , V 0) -> genInst env i $ boolTrick ["wData","o0"] $ zeroArg $ inst 1 1
	(B  , V m) -> genInst env i $ boolTrick ["wData","o0"] $ inst 1 m
	(V n, V m) -> genInst env i $ inst n m
	_ -> error "RAM typing issue (should not happen)"
 where
        ("rAddr",addr_ty,_) = last inputs

{-
        rAddr = case d of
                  Port "o0" register_id ->
                    case M.lookup register_id env of
                        Just (Entity (Prim "register") _ ins) ->
                       _ ->
                  _ -> error $ ("rAddr",d)
-}
        inst :: Int -> Int -> Entity Int
        inst n m = Entity
                    (External "lava_bram")
                    outputs
		    (inputs ++ [("data_width",GenericTy,Generic $ fromIntegral n)
			       ,("addr_width",GenericTy,Generic $ fromIntegral m)
			       ])
        zeroArg (Entity nm outs ins) =
                        Entity nm outs $
                               [ (n,V 1,Lit $ RepValue [Just False])
                               | n <- ["wAddr","rAddr"]
                               ] ++
                               [ (n,t,d) | (n,t,d) <- ins, n /= "wAddr"
                                                        && n /= "rAddr"
                               ]
        goodAddrType ty =
                case ty of
                  U _ -> True
                  _   -> error $ "unsupported address type for BRAMs: " ++ show ty


        -- | External entitites will sometimes have inputs and outputs that are
        -- std_logic_vectors (rather than std_logic) for 1-bit signals. This function
        -- adds the appropriate indexing.
        boolTrick :: [String] -> Entity s -> Entity s
        boolTrick nms (Entity (External nm) outs ins) =
          Entity (External nm)
                   [ (trick n,t) | (n,t) <- outs ]
                   [ (trick n,t,d) | (n,t,d) <- ins ]
            where
              trick n | n `elem` nms = n ++ "(0)"
                      | otherwise    = n

        boolTrick _ _ = error "applying bool Trick to non-external entity"



-- For read, we find the pairing write, and call back for "RAM".
-- This may produce multiple RAMs, if there are multiple reads.

-- This will be called index later.

genInst _ i (Entity (Prim "asyncRead")
                [("o0",ty)]
                [ ("i0",ty1@MatrixTy {},dr1)
                , ("i1",ty2,dr2)
                ]) =
   case (dr1,toStdLogicType ty) of
     (Port v n,SLV _) ->
        [NetAssign  (sigName "o0" i)
                    (memToStdLogic ty $
                       ExprIndex (sigName v (fromIntegral n))
                                  (toMemIndex ty2 dr2)
                    )
        ]
     _ -> error "bad array as input to asyncRead or strange output type"
 where
    MatrixTy _ (V _) = toStdLogicTy ty1

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


genInst _ i (Entity (Prim "rom") [("o0",MatrixTy {})] [(_,RomTy {},_)]) =
        [ CommentDecl (sigName "o0" i ++ " is a constant array") ]

--------------------------------------------------------------------------------

-- And the defaults

-- Right now, we *assume* that every external entity
-- has in and outs of type std_logic[_vector].

genInst _ i (Entity name@(External nm) outputs inputs) =
	trace (show ("mkInst",name,[ t | (_,t) <- outputs ],[ t | (_,t,_) <- inputs ]))
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
         fixName B n | "(0)" `isSuffixOf` n = reverse (drop 3 (reverse n))
         fixName _ n = n

-- Idea: table that says you take the Width of i/o Var X, and call it y, for the generics.

genInst _ i (Entity (Function mp) [(vout,tyout)] [(_,tyin,d)]) =
	[ NetAssign (sigName vout i)
		(ExprCase (toStdLogicExpr tyin d)
			[ ([toStdLogicExpr tyin ix],toStdLogicExpr tyout val)
			| (ix,val) <- mp
			]
			(Just $ toStdLogicExpr tyout (0 :: Integer))	-- replace with unknowns
		)
	]

genInst _ i other = error $ show ("genInst",i,other)


--------------------------------------------------------------

data NetlistOperation = NetlistOp Int (Type -> [(Type,Driver Unique)] -> Expr)

mkSpecialUnary
	:: (Type -> Expr -> Expr)
	-> (Type -> Driver Unique -> Expr)
	-> [(String, UnaryOp)]
	-> [(Id, NetlistOperation)]
mkSpecialUnary coerceR coerceF ops =
       [( Prim lavaName
	, NetlistOp 1 (uop netListOp)
	)
         | (lavaName,netListOp) <- ops
         ]
  where uop op fTy [(ity,i)] = coerceR fTy (ExprUnary op (coerceF ity i))
        uop op _ _ = error $ "unary op " ++ show op ++ " can have only one argument"


mkSpecialBinary
	:: (Type -> Expr -> Expr)
	-> (Type -> Driver Unique -> Expr)
--	-> [String]
	-> [(String, BinaryOp)]
	-> [(Id, NetlistOperation)]
mkSpecialBinary coerceR coerceF ops =
       [( Prim lavaName
	, NetlistOp 2 (binop netListOp)
	)
       | (lavaName,netListOp) <- ops
       ]
  where
    -- re-sign a number, please
    resign sz n = if n >= 2^(sz-1) then n - 2^sz else n
    mkBool True  = ExprLit Nothing (ExprBit T)
    mkBool False = ExprLit Nothing (ExprBit F)
    binop op fTy [(lty,l),(rty,r)] =
      case (l,r) of
        (Lit ll,Lit rl)
          -> let il = fromRepToInteger ll
                 ir = fromRepToInteger rl
             in case (op,lty,rty) of
                  (GreaterThan,S x,S y) -> mkBool (resign x il > resign y ir)
                  (Minus,U x,U y)       | x == y && il >= ir
                                       -> toStdLogicExpr fTy (il - ir)
                  (And,U x,U y)         | x == y
                                       -> toStdLogicExpr fTy (il .&. ir)
                  other -> error $ show ("mkSpecialBinary (constant)",op,il,ir,other)
        _ -> coerceR fTy (ExprBinary op (coerceF lty l)(coerceF rty r))
    binop op _ _ = error $ "Binary op " ++ show op ++ " must have exactly 2 arguments"


mkSpecialShifts :: [(String, Ident)] -> [(Id, NetlistOperation)]
mkSpecialShifts ops =
    [(Prim lavaName
      , NetlistOp 2 (binop funName)
     )
    | (lavaName, funName) <- ops
    ]
  where
    binop op fTy [(lty,l),(rty,r)] =
      toStdLogicExpr fTy $ ExprFunCall op [toTypedExpr lty l, toIntegerExpr rty r]
--      toStdLogicExpr fTy $ ExprBinary op (toTypedExpr lty l) (toIntegerExpr rty r)
    binop op _ _ = error $ "Binary op " ++ show op ++ " must have exactly 2 arguments"


-- testBit returns the bit-value at a specific (constant) bit position
-- of a bit-vector.
-- This generates:    invar(indexVal);
mkSpecialTestBit :: [(Id, NetlistOperation)]
mkSpecialTestBit =
    [(Prim lavaName
      , NetlistOp 2 binop
     )
    | lavaName <- ["testBit"]
    ]
  where binop _ [(lty,l),(rty,r)] =
          let (ExprVar varname) =  toStdLogicExpr lty l
          in (ExprIndex varname (toIntegerExpr rty r))
        binop _ _ = error "Binary op testBit must have exactly 2 arguments"


specials :: [(Id, NetlistOperation)]
specials =
      mkSpecialBinary (const active_high) toTypedExpr
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
        [ ("or2",Or), ("and2",And), ("xor2",Xor)
        , ("nand2",Nand), ("nor2",Nor)
	]
   ++ mkSpecialUnary  toStdLogicExpr toTypedExpr
	[("negate",Neg)]
   ++ mkSpecialUnary  (\ _ e -> e) toStdLogicExpr
	[("not",LNeg)
	,("complement",LNeg)
	]
   ++   mkSpecialTestBit
-- See: http://homepage.ntlworld.com/jonathan.deboynepollard/FGA/bit-shifts-in-vhdl.html
   ++   mkSpecialShifts
        [ ("shiftL", "shift_left")
        , ("shiftR", "shift_right")
        , ("shiftLA", "shift_left")	-- overloaded in VHDL
        , ("shiftRA", "shift_right")	-- overloaded in VHDL
        , ("rotateL", "rotate_left")
        , ("rotateR", "rotate_right")
        ]



slvVarName :: (Show v, ToStdLogicExpr v) => Type -> v -> Ident
slvVarName tI w = case toStdLogicExpr tI w of
                    ExprVar varname -> varname
                    _ -> error $ "Can't get the name of variable " ++ show w

mkProcessDecl :: [(Event, Stmt)] -> Decl
mkProcessDecl [(e,s)] = ProcessDecl e Nothing s
mkProcessDecl _ = error "mkProcessDecl"
