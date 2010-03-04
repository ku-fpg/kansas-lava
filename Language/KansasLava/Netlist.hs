{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.Netlist(netlistCircuit,delay') where


-- import Language.KansasLava hiding (Seq)
-- import qualified Language.KansasLava as KL
import qualified Language.KansasLava.Entity as E
import qualified Language.KansasLava.Seq as KL
import Language.KansasLava.Comb
import Language.KansasLava.Utils
import Language.KansasLava.Reify
  (reifyCircuit,Ports,ReifyOptions(..),ReifiedCircuit(..))
import Language.KansasLava.Entity hiding (name)
import Language.KansasLava.Type

import Data.Sized.Unsigned

import Data.Reify.Graph

import Text.PrettyPrint
import Data.List(intersperse,find)
import Data.Maybe(fromJust)
import qualified Data.Map as M

import Debug.Trace



import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL



-- | The 'netlistCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
netlistCircuit :: (Ports o) =>
               [ReifyOptions] -- ^ Options for controlling the observable-sharing reification.
            -> String         -- ^ The name of the generated entity.
            -> o              -- ^ The Lava circuit.
            -> IO Module
netlistCircuit opts name circuit = do
  (ReifiedCircuit nodes srcs sinks) <- reifyCircuit opts circuit

  let inports = -- need size info for each input, to declare length of std_logic_vector
            [ (nm,sizedRange ty) | (Var nm, ty) <- srcs]
              -- need size info for each output, to declare length of std_logic_vector
  let  outports =
            [ (nm,sizedRange ty) | (Var nm,ty,_) <- sinks]

  let mod = Module name inports outports ( genDecls nodes ++   genInsts nodes ++ genFinals sinks)
      mod' = inlineModule mod

  return mod


-- genDecls :: [(Unique,Entity BaseTy Unique)] -> [DeclDescriptor]
genDecls nodes =
    -- need size info for each output, to declare length of std_logic_vector
    [ NetDecl (sigName n i) (sizedRange nTy) Nothing
                          | (i,Entity (Name mod nm) outputs _ _) <- nodes
                          , (Var n,nTy) <- outputs, not (nm `elem` ["delay","register"])
                          ] ++
    concat [ [ NetDecl ((sigName n i) ++ "_next") (sizedRange nTy) Nothing
             , MemDecl (sigName  n i) Nothing (sizedRange  nTy)
     	     ]
           | (i,Entity (Name "Memory" "register") [(Var n,nTy)] _ _) <- nodes
           ] ++
    concatMap memDecl nodes

  where memDecl (i,Entity (Name "Lava" "BRAM") [(Var n,_)] inputs _) =
          let Just (_,aTy,_) = find (\(Var v,_,_) -> v == "ain") inputs
              Just (_,dTy,_) = find (\(Var v,_,_) -> v == "din") inputs
          in [ MemDecl (sigName n i) (sizedRange aTy) (sizedRange dTy)  ]
        memDecl _ = []



-- genFinals :: [(Var,BaseTy,Driver Unique)] -> [Inst]
genFinals args = [ NetAssign  n (sigExpr x) | (Var n,_,x) <- args ]

-- genInsts :: [(Unique,Entity BaseTy Unique)] -> [Inst]
genInsts nodes = concat [
               mkInst i ent
                 | (i,ent) <- nodes ] ++
              (synchronous nodes)

-- A funciton to render a Driver as a valid VHDL expression (of the appropriate type)
type SigToVhdl = BaseTy -> Driver Unique -> String
slvCast :: a -> Maybe String
slvCast = const $ Just "std_logic_vector"

active_high :: a -> Maybe String
active_high = const $ Just "active_high"

-- to_integer :: SigToVhdl
to_integer _ d@(Lit l) =  sigExpr d
to_integer ty  p =  ExprFunCall "to_integer" [sigTyped ty p ]

type Cast = BaseTy -> Expr -> Expr

cast :: Cast
cast (U _) d = ExprFunCall "unsigned" [d]
cast (S _) d = ExprFunCall "signed" [d]
cast _ d = d



data OpFix = Prefix | Infix | PostFix | NoFix deriving (Show)
data VhdlOperation =
     VHDLOp OpFix String (BaseTy -> Maybe String) [SigToVhdl]

data NetlistOperation =
   NetlistOp Int ([(BaseTy,Expr)] -> Expr)


-- the static list, specials, describes entities
-- that can be directly converted VHDL behavioral expressions
--

bOp op ocast [(lty,l), (rty,r)] =
  ocast (ExprBinary op (cast lty l) (cast rty r))

uOp op ocast [(lty,l)] =
  ocast (ExprUnary op (cast lty l))

uOp op _ args = error $ show op ++ show args

activeHigh d  = ExprFunCall "active_high" [d]
to_std_logic_vector d = ExprFunCall "std_logic_vector" [d]



mkSpecialUnary mtys ops =
       [(Name moduleName lavaName, NetlistOp 1 (uOp netListOp to_std_logic_vector))
         | (moduleName, ty) <- mtys
         , (lavaName,netListOp) <- ops
         ]
mkSpecialBinary mtys ops =
       [(Name moduleName lavaName, NetlistOp 2 (bOp netListOp to_std_logic_vector))
         | (moduleName, ty) <- mtys
         , (lavaName,netListOp) <- ops
         ]


specials :: [(Name, NetlistOperation)]
specials =
       (mkSpecialBinary
        [("Unsigned",U 0), ("Signed", S 0), ("Bool",B)]
        [(".<.",LessThan), (".>.",GreaterThan),(".==.",Equals),(".<=.",LessEqual), (".>=.",GreaterEqual) ])
       ++
       (mkSpecialBinary
        [("Unsigned",U 0), ("Signed", S 0), ("Bool",B)]
        [("+",Plus), ("-",Minus), (".|.",Or), (".&.",And), (".^.",Xor)])
       ++
       (mkSpecialUnary
        [("Signed", S 0)] [("negate",Neg)])


  -- [(Name moduleName lavaName, NetlistOp (bOp netListOp to_std_logic_vector)
  --      | moduleName <- [("Unsigned",U 0), ("Signed", S 0),("Int",IntegerTy)]
  --      , (lavaName,vhdlName) <- [("+","+"), ("-","-"), (".|.","or"), (".&.","and"), (".^.","xor")]]
  -- ++
  -- [(Name moduleName lavaName, (VHDLOp Prefix vhdlName slvCast [sigTyped]))
  --      | moduleName <- ["Unsigned", "Signed","Int"]
  --      , (lavaName,vhdlName) <- [("negate", "-"), ("complement", "not")]]
  -- ++
  -- [(Name moduleName lavaName, (VHDLOp Infix vhdlName (const Nothing)  [sigTyped, sigTyped]))
  --      | moduleName <- ["Bool"]
  --         , (lavaName,vhdlName) <- [("or2","or"), ("and2","and"), ("xor2","xor")]]
  -- ++
  -- [(Name moduleName lavaName, (VHDLOp Prefix vhdlName (const Nothing) [sigTyped]))
  --      | moduleName <- ["Bool"]
  --      , (lavaName,vhdlName) <- [("not", "not")]]
  -- ++

       -- [(Name moduleName lavaName, NetlistOp 2 (bOp netListOp activeHigh))
       --   | (moduleName, ty) <- [("Unsigned",U 0), ("Signed", S 0), ("Bool",B)]
       --   , (lavaName,netListOp) <-
       --     [(".<.",LessThan),
       --      (".>.",GreaterThan),
       --      (".==.",Equals),
       --      (".<=.",LessEqual),
       --      (".>=.",GreaterEqual) ]
       --   ]
  -- [(Name moduleName lavaName, (VHDLOp Infix vhdlName active_high  [sigTyped, sigTyped]))
  --      | moduleName <- ["Unsigned", "Signed","Int","Bool"]
  --      , (lavaName,vhdlName) <- [(".<.","<"), (".>.",">"),(".==.","="),(".<=.","<="), (".>=.",">=") ]]
  -- ++
  --   -- The VHDL operator "sll" (Shift Left Logical)
  --   -- actually performs sign extension IF the first argument is Signed
  --   -- and the effective shift direction is to the "right"
  --   -- (i.e. the shift count is negative)-}
  -- [(Name moduleName lavaName, (VHDLOp Infix vhdlName slvCast  [sigTyped, to_integer]))
  --      | moduleName <- ["Unsigned", "Signed"]
  --      , (lavaName,vhdlName) <- [("shift","sll")]]


mkInst :: Unique -> Entity BaseTy Unique -> [Decl]
mkInst i (Entity n@(Name _ _) [(Var "o0",oTy)] ins _)
        | Just (NetlistOp arity f) <- lookup n specials, arity == length ins =
          [NetAssign  (sigName "o0" i)
                  (f [(inTy, (sigExpr driver))  | (_,inTy,driver) <- ins])]

-- Delay Circuits
mkInst i e@(Entity (Name "Memory" "delay") [(Var "o0",_)]
			[ (Var "i0",_,inp)] _) =
          [NetAssign input (sigExpr $ lookupInput "i0" e) ]
  where output = sigName "o0" i
        input =  output ++ "_next"

mkInst i e@(Entity (Name "Memory" "register") [(Var "o0",_)] _ _) =
          [NetAssign input (sigExpr (lookupInput "i0" e)) ]
  where output = sigName "o0" i
        input =  output ++ "_next"

-- Muxes
mkInst i (Entity (Name _ "mux2") [(Var "o0",_)] [(Var i0,cTy,c),(Var i1 ,tTy,t),(Var i2,fTy,f)] _)
-- FIXME: Figure out the casts
	= [NetAssign output
                     (ExprCond cond
                      (sigExpr f)
                      (sigExpr t))]
  where cond = isLow (sigTyped cTy c)
        output = (sigName "o0" i)

mkInst _ (Entity (Name "Bool" "mux2") x y _) = error $ show (x,y)
mkInst _ (Entity m2@(Name _ "mux2") x y _) = error $ show (x,y) ++ show m2

mkInst  i (Entity (Name "Lava" "concat") [(Var "o0",_)] inps _) =
                  [NetAssign (sigName "o0" i) val]
  where val = ExprConcat
                -- Note the the layout is reversed
                [sigExpr s | (Var _,_, s) <- reverse inps]


-- The 'Top' entity should not generate anything, because we've already exposed
-- the input drivers via theSinks.
mkInst _ (Entity (Name "Lava" "top") _ _ _) = []

mkInst  _ (Entity (Name "Lava" "BRAM") _ _ _) = []

mkInst i (Entity (Name "Lava" "index") [(Var "o0",_)] [(Var "i", _, input),(Var "index",_,idx)] _) =
    [NetAssign (sigName  "o0" i) (ExprIndex iname (sigExpr idx))]
  where (ExprVar iname) = sigExpr input


mkInst i (Entity (Name "Lava" "fst") [(Var "o0",_)] [(Var "i0", pTy@(TupleTy tys), input)] _) =
  [NetAssign  (sigName "o0" i)
          (ExprSlice iname (ExprNum high) (ExprNum low))]
  where high = fromIntegral $ baseTypeLength pTy - 1
        low = fromIntegral $ high - fromIntegral (baseTypeLength target)
        target = tys !! 0
        ExprVar iname = sigExpr input

mkInst i (Entity (Name "Lava" "snd") [(Var "o0",_)] [(Var "i0", pTy@(TupleTy tys), input)] _) =
  [NetAssign  (sigName "o0" i)
          (ExprSlice inpName (ExprNum high) (ExprNum low))]
  where high = fromIntegral $ baseTypeLength target - 1
        low =  0
        target = tys !! 1
        ExprVar inpName = sigExpr input

mkInst i (Entity (Name "Lava" "pair") [(Var "o0",_)]
                   [(Var "i0", _, i0),(Var "i1", _, i1)] _)  =
  [NetAssign  (sigName "o0" i)
          (ExprConcat [sigExpr i0, sigExpr i1])]





mkInst i (Entity (Name "probe" _) [(Var "o0",_)]
                  [(Var "i0", _, input)] _) =
  [NetAssign (sigName "o0" i)
          (sigExpr input)]


-- Catchall for everything else
mkInst i (Entity (Name _ nm) outputs inputs _) =
          [ InstDecl nm ("inst" ++ show i)
                []
                [ (n,cast nTy  (sigExpr x)) | (Var n,nTy,x) <- inputs ]
                [ (n,sigExpr (Port (Var n) i)) | (Var n,_) <- outputs ]
          ]
mkInst _ e@(Entity _ _ _ _) = error $ show e




{-
mkInst i tab@(Table (vout,tyout) (vin,tyin,d) mp) =
	[ Comment $ show tab
	,  Case (sigTyped tyin d)
			[ (show ix,Assign (sig (Port (vout) i))
					  (ocast tyout (Lit (fromIntegral val)))
			  )
			| (ix,_,val,_) <- mp
			]
{-
		       [ (ocast tyout (Lit (fromIntegral val)), "???")
		       | (ix,_,val,_) <- mp
		       ]
--                       [(ocast fTy f, sigTyped cTy c ++ "= '0'")]
                       (ocast tyout (Lit 0))
-}
        ]
-}


-- The 'synchronous' function generates a synchronous process for the
-- delay elements
-- synchronous :: [(Unique, Entity BaseTy Unique)] -> [Inst]


lookupInput i (Entity _ _ inps _) = case find (\(Var v,_,_) -> v == i) inps of
                                      Just (_,_,d) -> d
                                      Nothing -> error "lookupInput: Can't find input"

lookupInputType i (Entity _ _ inps _) = case find (\(Var v,_,_) -> v == i) inps of
                                          Just (_,ty,_) -> ty
                                          Nothing -> error "lookupInputType: Can't find input"



synchProc (clk,rst) [] = []
synchProc (clk,rst) es =
 [ProcessDecl
  [(Event (sigExpr clk) PosEdge,
          (If (isHigh (sigExpr rst))
              (statements [Assign (outName i) (defaultDriver e) |  (i,e) <- es])
              (Just (statements [Assign (outName i) (driver e) | (i,e) <- es]))))]]
  where outName i = sigExpr (Port (Var "o0") i)
        defaultDriver e = sigTyped (defaultDriverType e) $ lookupInput "def" e
        defaultDriverType e = lookupInputType "def" e
        driver e = sigExprNext $  lookupInput "i0" e



{-
  | null delays && null brams  = []
  | otherwise = [ProcessDecl -- ([sig clk, sig rst] ++  inputs ++ bramWE ++  bramData)
                 [ (Event clk PosEdge,
        	    (Seq (if null delays
                          then []
                          else
                             [If (isHigh rst)
        	                   (Seq (zipWith Assign outputs inits))
                                   (Just (Seq (zipWith Assign outputs inputs)))
                             ] ++
                     [If (isHigh (ExprVar we))
                           (Seq [Assign (ExprVar target) (ExprVar dat),
                                 Assign (ExprVar o) zeros])
                            (Just (Assign (ExprVar o) (ExprVar target)))
                              | we <- bramWE
                              | o <- bramOuts
                              | target <- bramTargets
                              | dat <- bramData
                     ])))]]
-}

synchronous nodes
   | M.null synchs = error "No synch nodes"
   | otherwise = concatMap (uncurry synchProc) $ M.toList synchs
  where -- Handling registers
        synchs = getSynchs ["register", "delay"] nodes


 {-
   FIXME: Add in the support for this stuff.
        -- Handling BRAMS
        brams = [(i,e) | (i,e@(Entity (Name "Lava" "BRAM") _ _ _)) <- nodes]
        bramOuts =[sig (Port (Var "o0") i) | (i,_) <- brams]
        bramSigs = [(sig (Port (Var n) i) ++ "_ram")
                      | (i,Entity _ [(Var n,_)] _ _) <- nodes]
        bramAddr = ["conv_integer(" ++ sig (inputDriver "ain" e) ++ ")"
                   | (_,e) <- brams]
        bramData = [sig (inputDriver "din" e)
                   | (_,e) <- brams]
        bramWE = [sig (inputDriver "we" e)
                   | (_,e) <- brams]
        bramTargets = [s ++ "(" ++ a ++ ")" |  s <- bramSigs | a <- bramAddr]

-}

-- Utility functions

-- Grab all of the synchronous elements (listed in 'nms') and return a map keyed
-- on clk input, with the value including a list of associated entities.
getSynchs nms  ents = M.fromListWith (++) synchs
  where
        synchs = [((getInput "clk" is,getInput "rst" is),[e])  | e@(i,Entity (Name "Memory" n) _ is _) <- ents, n `elem` nms]
        getInput nm is = case find (\(Var c,_,_) -> c == nm) is of
                      Just (Var _,_,d) -> d
                      Nothing -> error "getSynchs: Can't find a signal"



sigName :: Show t => String -> t -> String
sigName v d = "sig_" ++  show d ++ "_" ++ v

sigExpr, sigExprNext :: Show t => Driver t -> Expr
sigExprNext = sigExpr' True
sigExpr = sigExpr' False

sigExpr' True (Port (Var v) d) = ExprVar ((sigName v d) ++ "_next")
sigExpr' False (Port (Var v) d) = ExprVar (sigName v d)
sigExpr' _ (Pad (Var n)) = ExprVar n
sigExpr' _ (Lit x) = ExprNum x
sigExpr' nxt (BitIndex i b) = ExprIndex v (ExprNum (fromIntegral i))
  where (ExprVar v) = sigExpr' nxt b
sigExpr' nxt (BitSlice hi lo b) =
    ExprSlice v (ExprNum (fromIntegral hi)) (ExprNum (fromIntegral lo))
  where (ExprVar v) = sigExpr' nxt b


-- sigTyped adds the type casts, especially for literals.
to_unsigned x w = ExprFunCall "to_unsigned" [x, w]
unsigned x = ExprFunCall "unsigned" [x]
to_signed x w = ExprFunCall "to_signed" [x, w]
signed x = ExprFunCall "signed" [x]


sigTyped :: BaseTy -> Driver Unique -> Expr
sigTyped (U width) d@(Lit n) = to_unsigned (sigExpr d) (ExprNum $ fromIntegral width)
sigTyped (S width) d@(Lit n) = to_signed (sigExpr d) (ExprNum $ fromIntegral width)
sigTyped B         (Lit b) = ExprBit (fromInteger b)
sigTyped B     s = sigExpr s
sigTyped (U _) s = unsigned (sigExpr s)
sigTyped (S _) s = signed (sigExpr s)
sigTyped ClkTy s = sigExpr s
sigTyped RstTy s = sigExpr s
sigTyped ty s = error $ "sigtyped :" ++ show ty ++ "/" ++ show s


isHigh d = (ExprBinary Equals d (ExprBit 1))
isLow d = (ExprBinary Equals d (ExprBit 0))
allLow ty = ExprLit (baseTypeLength ty) 0
zeros = ExprString "(others => '0')"


sizedRange B = Nothing
sizedRange ty = ran -- trace ("sizedRange: " ++ show ty ++ " " ++ show ran) ran
  where size = baseTypeLength ty
        ran = Just $ Range (ExprNum (fromIntegral size - 1)) (ExprNum 0)


delay' :: KL.Seq SysEnv -> KL.Seq U8 -> KL.Seq U8
delay' env i = register env (0 :: Comb (U8)) i
