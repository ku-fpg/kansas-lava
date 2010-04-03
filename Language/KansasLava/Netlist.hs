{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.Netlist where -- (netlistCircuit, NetlistOption(..) ) where


-- import Language.KansasLava hiding (Seq)
-- import qualified Language.KansasLava as KL
import qualified Language.KansasLava.Entity as E
import qualified Language.KansasLava.Seq as KL
import Language.KansasLava.Comb
import Language.KansasLava.Utils
import Language.KansasLava.Reify(reifyCircuit,Ports)
import Language.KansasLava.Circuit(ReifyOptions(..),ReifiedCircuit(..))
import Language.KansasLava.Entity hiding (name)
import Language.KansasLava.Type

import Data.Sized.Unsigned

import Data.Reify.Graph

import Text.PrettyPrint
import Data.List(intersperse,find,mapAccumL,nub)
import Data.Maybe(fromJust)
import qualified Data.Map as M

import Debug.Trace



import Language.Netlist.AST
import Language.Netlist.Util
import Language.Netlist.Inline
import Language.Netlist.GenVHDL



data NetlistOption = LoadEnable | AsynchResets deriving (Eq, Show, Ord)
type NetlistOptions = [NetlistOption]

addEnabled opts = LoadEnable `elem` opts
asynchResets opts = AsynchResets `elem` opts


-- | The 'netlistCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
netlistCircuit :: (Ports o) =>
               [ReifyOptions] -- ^ Options for controlling the observable-sharing reification.
            -> NetlistOptions -- ^ Options for controlling netlist generation
            -> String         -- ^ The name of the generated entity.
            -> o              -- ^ The Lava circuit.
            -> IO Module
netlistCircuit opts nlOpts name circuit = do
  (ReifiedCircuit nodes srcs sinks) <- reifyCircuit opts circuit

  let loadEnable = if addEnabled nlOpts then [("enable",Nothing)] else []
  let inports = -- need size info for each input, to declare length of std_logic_vector
            loadEnable ++ [ (nm,sizedRange ty) | (Var nm, ty) <- srcs]
              -- need size info for each output, to declare length of std_logic_vector

  let  outports =
            [ (nm,sizedRange ty) | (Var nm,ty,_) <- sinks]

  let mod = Module name inports outports ( genDecls nodes ++   genInsts nlOpts nodes ++ genFinals sinks)
      -- mod' = inlineModule mod

  return mod


-- genDecls :: [(Unique,Entity BaseTy Unique)] -> [DeclDescriptor]
genDecls nodes =
    -- need size info for each output, to declare length of std_logic_vector
    [ NetDecl (sigName n i) (sizedRange nTy) Nothing
                          | (i,Entity (Name mod nm) outputs _ _) <- nodes
                          , (Var n,nTy) <- outputs, not (nm `elem` ["delay","register"])
                          ] ++
    [ NetDecl (sigName n i) (sizedRange nTy) Nothing
                          | (i,Table (Var n,nTy) _ _) <- nodes
                          ] ++
    concat [ [ NetDecl (sigName n i ++ "_next") (sizedRange nTy) Nothing
             , MemDecl (sigName  n i) Nothing (sizedRange  nTy)
     	     ]
           | (i,Entity (Name "Memory" "register") [(Var n,nTy)] _ _) <- nodes
           ] ++
    concat [ [ NetDecl ((sigName n i) ++ "_next") (sizedRange nTy) Nothing
             , MemDecl (sigName  n i) Nothing (sizedRange  nTy)
     	     ]
           | (i,Entity (Name "Memory" "delay") [(Var n,nTy)] _ _) <- nodes
           ] ++

    concatMap memDecl nodes

  where memDecl (i,Entity (Name "Memory" "BRAM") [(Var n,_)] inputs _) =
          let Just (_,aTy,_) = find (\(Var v,_,_) -> v == "wAddr") inputs
              Just (_,dTy,_) = find (\(Var v,_,_) -> v == "wData") inputs
          in [ MemDecl (sigName n i) (memRange aTy) (sizedRange dTy)  ]
        memDecl _ = []

-- genFinals :: [(Var,BaseTy,Driver Unique)] -> [Inst]
genFinals args = [ NetAssign  n (sigExpr x) | (Var n,_,x) <- args ]

-- genInsts :: [(Unique,Entity BaseTy Unique)] -> [Inst]
genInsts nlOpts nodes = concat [
               mkInst i ent
                 | (i,ent) <- nodes ] ++
              (synchronous nlOpts nodes)

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


-- IEEE numerical to std_logic[_vector] format
--assignCast ty (Lit x) = toBinary ty x
assignCast (U _) d         = to_std_logic_vector d
assignCast (S _) d         = to_std_logic_vector d
assignCast (TupleTy {}) d  = to_std_logic_vector d
assignCast _  d = d

data OpFix = Prefix | Infix | PostFix | NoFix deriving (Show)
data VhdlOperation =
     VHDLOp OpFix String (BaseTy -> Maybe String) [SigToVhdl]

data NetlistOperation =
   NetlistOp Int ([(BaseTy,Driver Unique)] -> Expr)


-- the static list, specials, describes entities
-- that can be directly converted VHDL behavioral expressions
--

bOp op ocast [(lty,l), (rty,r)] =
  ocast (ExprBinary op (sigTyped lty l) (sigTyped rty r))

uOp op ocast [(lty,l)] =
  ocast (ExprUnary op (sigTyped lty l))

uOp op _ args = error $ show op ++ show args

activeHigh d  = ExprFunCall "active_high" [d]
to_std_logic_vector d = ExprFunCall "std_logic_vector" [d]



mkSpecialUnary ocast mtys ops =
       [(Name moduleName lavaName, NetlistOp 1 (uOp netListOp ocast))
         | (moduleName, ty) <- mtys
         , (lavaName,netListOp) <- ops
         ]
{- mkSpecialBinary
  :: (Expr -> Expr)
     -> [(String, t)]
     -> [(String, BinaryOp)]
     -> [(Name, NetlistOperation)]
-}
mkSpecialBinary ocast mtys ops =
       [(Name moduleName lavaName, NetlistOp 2 (bOp netListOp ocast))
         | (moduleName, ty) <- mtys
         , (lavaName,netListOp) <- ops
         ]


specials :: [(Name, NetlistOperation)]
specials =
       (mkSpecialBinary activeHigh
        [("Unsigned",U 0), ("Signed", S 0), ("X32",S 5), ("Bool",B)]	-- Ugly solution to X32 issue
        [(".<.",LessThan), (".>.",GreaterThan),(".==.",Equals),(".<=.",LessEqual), (".>=.",GreaterEqual)])
       ++
       (mkSpecialBinary id	-- TODO: review 
        [ ("Bool",B)]
        [ ("xor2",Xor), ("or2", Or), ("and2", And) ])
       ++
       (mkSpecialBinary to_std_logic_vector
        [("Unsigned",U 0), ("Signed", S 0)]
        [("+",Plus), ("-",Minus), (".|.",Or), (".&.",And), (".^.",Xor)])
       ++
       (mkSpecialUnary to_std_logic_vector
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
                  (f [(inTy, driver)  | (_,inTy,driver) <- ins])]

-- Delay Circuits
mkInst i e@(Entity (Name "Memory" "delay") [(Var "o0",_)]
			[ (Var "i0",_,inp)] _) =
          [NetAssign input (sigExpr $ lookupInput "i0" e) ]
  where output = sigName "o0" i
        input =  output ++ "_nextd"


mkInst i e@(Entity (Name "Memory" "register") [(Var "o0",_)] _ _) =
          [NetAssign input (sigExpr (lookupInput "i0" e)) ]
  where output = sigName "o0" i
        input =  output ++ "_next"


-- Muxes
mkInst i (Entity (Name _ "mux2") [(Var "o0",_)] [(Var i0,cTy,c),(Var i1 ,tTy,t),(Var i2,fTy,f)] _)
-- FIXME: Figure out the casts
	= [NetAssign output
                     (ExprCond cond
                      (assignCast fTy $ sigTyped fTy f)
                      (assignCast tTy $ sigTyped tTy t))]
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

-- This gets generated seperately by the sync. finder
mkInst  _ (Entity (Name "Memory" "BRAM") _ _ _) = []

mkInst i (Entity (Name "Lava" "index") [(Var "o0",_)] [(Var "i", _, input),(Var "index",_,idx)] _) =
    [NetAssign (sigName  "o0" i) (ExprIndex iname (sigExpr idx))]
  where (ExprVar iname) = sigExpr input


mkInst i e@(Entity (Name "Lava" "fst") [(Var "o0",_)] [(Var "i0", pTy@(TupleTy tys), input)] _) =
  [NetAssign  (sigName "o0" i) (prodSlices input tys !! 0)]


mkInst i e@(Entity (Name "Lava" "snd") [(Var "o0",_)] [(Var "i0", pTy@(TupleTy tys), input)] _) =
  [NetAssign  (sigName "o0" i) (prodSlices input tys !! 1)]

-- VHDL bits are (by Lava convention) MSB to LSB, so the first element
-- of a pair comes first on the *right* hand side.
mkInst i (Entity (Name "Lava" "pair") [(Var "o0",_)]
                   [(Var "i0", ty0, i0),(Var "i1", ty1, i1)] _)  =
  [NetAssign  (sigName "o0" i)
          (ExprConcat [ asStdLogic ty1 i1,  asStdLogic ty0 i0])]


mkInst i (Entity (Name "probe" _) [(Var "o0",_)]
                  [(Var "i0", _, input)] _) =
  [NetAssign (sigName "o0" i)
          (sigExpr input)]

-- Hack for April fools
mkInst i (Entity n@(Name "X32" "+") outputs inputs dyn) =
	mkInst i (Entity (Name "Unsigned" "+") outputs inputs dyn)
mkInst i (Entity n@(Name "X32" "-") outputs inputs dyn) =
	mkInst i (Entity (Name "Unsigned" "-") outputs inputs dyn)
	

-- Catchall for everything else
mkInst i (Entity n@(Name mod_nm nm) outputs inputs _) =
	trace (show ("mkInst",n)) $ 
          [ InstDecl (mod_nm ++ "_" ++ cleanupName nm) ("inst" ++ show i)
                []
                [ (n,asStdLogic nTy x) | (Var n,nTy,x) <- inputs ]
                [ (n,sigExpr (Port (Var n) i)) | (Var n,_) <- outputs ]
          ]

-- TODO: Table should have a default, for space reasons
mkInst i tab@(Table (Var vout,tyout) (vin,tyin,d) mp) =
	[ NetAssign (sigName vout i)
		(ExprCase (sigExpr d) -- (cast tyin (sigExpr d))
			[ ([toBinary tyin ix],toBinary tyout val)
			| (ix,_,val,_) <- mp
			]
			(Just $ toBinary tyout 0)
		)
	]



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

asStdLogic :: BaseTy -> Driver Unique -> Expr
asStdLogic ty (Lit i) = assignCast ty $ sigTyped ty (Lit i)
asStdLogic ty e = sigExpr e		-- hmm?

cleanupName :: String -> String
cleanupName "+" = "addition"
cleanupName "-" = "subtraction"
cleanupName "*" = "multiplication"
cleanupName other = other

-- The 'synchronous' function generates a synchronous process for the
-- delay elements
-- synchronous :: [(Unique, Entity BaseTy Unique)] -> [Inst]

synchronous nlOpts nodes  = (concatMap (uncurry $ regProc nlOpts ) $ M.toList regs) ++
                            (concatMap (uncurry bramProc ) $ M.toList brams)
  where -- Handling registers
        regs = getSynchs ["register", "delay"] nodes
        brams = getSynchs ["BRAM"] nodes


regProc _ (clk,rst) [] = []
regProc nlOpts (clk,rst) es
  | asynchResets nlOpts =
    [ProcessDecl
     [(Event (sigExpr rst) PosEdge,
               (statements [Assign (outName i) (defaultDriver e) |  (i,e) <- es])),
      (Event (sigExpr clk) PosEdge,
              regNext)]]
  | otherwise =
    [ProcessDecl
     [(Event (sigExpr clk) PosEdge,
               (If (case rst of
		     Lit 0 -> ExprVar "false"
		     Lit 1 -> error "opps, bad delay code"
	 	     _ -> isHigh (sigTyped B rst))
                     (statements [Assign (outName i) (defaultDriver e) |  (i,e) <- es])
                     (Just regNext)))]]

  where outName i = sigExpr (Port (Var "o0") i)
        nextName i = sigExprNext (Port (Var "o0") i)
        defaultDriver e = let ty = defaultDriverType e
                          in assignCast ty $ sigTyped ty $ lookupInput "def" e
        defaultDriverType e = lookupInputType "def" e
        driver e = sigExprNext $  lookupInput "i0" e
        regAssigns = statements [Assign (outName i) (nextName i)  | (i,e) <- es]
        regNext
          | addEnabled nlOpts = If (isHigh (ExprVar "enable")) regAssigns Nothing
          | otherwise = regAssigns

bramProc (clk,rst) [] = []
bramProc (clk,rst) es =
  [ProcessDecl
   [(Event (sigExpr clk) PosEdge,
           statements
             [ If (isHigh (wEn e))
                   (Assign (writeIndexed i e) (wData e))
                   Nothing
		-- TODO: will need extra delay
	     , Assign (outName i) (readIndexed i e)
	     ])
   | (i,e) <- es ]]	
    where outName i = sigExpr (Port (Var "o0") i)
          ramName i = "sig_" ++ show i ++ "_o0_ram"
--          driver e = sigExprNext $  lookupInput "i0" e
          wEn e = sigExpr $ lookupInput "wEn" e
          wAddr e = sigExpr $ lookupInput "wAddr" e
          rAddr e = sigExpr $ lookupInput "rAddr" e
          wData e = sigExpr $ lookupInput "wData" e
          -- FIXME: This is a hack, to get around not having dynamically
          -- indexed exprs.
          writeIndexed i e = ExprIndex 
                         (ramName i) 
			 (conv_integer (wAddr e))
          readIndexed i e = ExprIndex (ramName i) (conv_integer (rAddr e))



-- Grab all of the synchronous elements (listed in 'nms') and return a map keyed
-- on clk input, with the value including a list of associated entities.
getSynchs nms  ents = M.fromListWith (++) synchs
  where
        synchs = [((getInput "clk" is,getInput "rst" is),[e])  | e@(i,Entity (Name "Memory" n) _ is _) <- ents, n `elem` nms]
        getInput nm is = case find (\(Var c,_,_) -> c == nm) is of
                      Just (Var _,_,d) -> d
                      Nothing -> error $ "getSynchs: Can't find a signal " ++ show (nm,is,ents)


-- Utility functions

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


conv_integer x = ExprFunCall "conv_integer" [x]


sigTyped :: BaseTy -> Driver Unique -> Expr
sigTyped (U width) d@(Lit n) = to_unsigned (sigExpr d) (ExprNum $ fromIntegral width)
sigTyped (S width) d@(Lit n) = to_signed (sigExpr d) (ExprNum $ fromIntegral width)
sigTyped B         (Lit b) = ExprBit (fromInteger b)
sigTyped ty@(TupleTy tys) d@(Lit b) = to_unsigned (sigExpr d) (ExprNum $ fromIntegral $ baseTypeLength ty)
sigTyped B     s = sigExpr s
sigTyped (U _) s = unsigned (sigExpr s)
sigTyped (S _) s = signed (sigExpr s)
sigTyped ClkTy s = sigExpr s
sigTyped RstTy s = sigExpr s
sigTyped (TupleTy tys) s = unsigned (sigExpr s)
sigTyped ty s = error $ "sigtyped :" ++ show ty ++ "/" ++ show s


-- Make a constant vhdl signal, in binary format.
toBinary :: BaseTy -> Integer -> Expr
toBinary B         n = ExprBit (fromInteger n)
toBinary (U width) n = ExprLit width n
toBinary (S width) n = ExprLit width n
toBinary ty@(TupleTy {}) n = ExprLit (baseTypeLength ty) n
toBinary other n = error $ show ("toBinary",other,n)


isHigh d = (ExprBinary Equals d (ExprBit 1))
isLow d = (ExprBinary Equals d (ExprBit 0))
allLow ty = ExprLit (baseTypeLength ty) 0
zeros = ExprString "(others => '0')"


sizedRange B = Nothing
sizedRange ClkTy = Nothing
sizedRange RstTy = Nothing
sizedRange ty = ran -- trace ("sizedRange: " ++ show ty ++ " " ++ show ran) ran
  where size = baseTypeLength ty
        ran = Just $ Range (ExprNum (fromIntegral size - 1)) (ExprNum 0)


-- like sizedRange, but allowing 2^n elements (for building memories)
memRange ty = ran -- trace ("sizedRange: " ++ show ty ++ " " ++ show ran) ran
  where size = baseTypeLength ty
        ran = Just $ Range (ExprNum (2^(fromIntegral size) - 1)) (ExprNum 0)



-- The BaseTy here goes from left to right, but we use it right to left.
-- So [B,U4] => <XXXX:4 to 1><X:0>, because of the convension ordering in our generated VHDL.
prodSlices :: Driver Unique -> [BaseTy] -> [Expr]
prodSlices d tys = snd $ mapAccumL f size $ reverse tys
  where ExprVar v = sigExpr d
        size = fromIntegral $ sum (map baseTypeLength tys) - 1
        f i B = (i-1,ExprIndex v (ExprNum i))
        f i ty = let w = fromIntegral $ baseTypeLength ty
                     next = i - w
                 in (next, ExprSlice v (ExprNum i) (ExprNum (next + 1)))
lookupInput i (Entity _ _ inps _) = case find (\(Var v,_,_) -> v == i) inps of
                                      Just (_,_,d) -> d
                                      Nothing -> error $ "lookupInput: Can't find input" ++ show (i,inps)

lookupInputType i (Entity _ _ inps _) = case find (\(Var v,_,_) -> v == i) inps of
                                          Just (_,ty,_) -> ty
                                          Nothing -> error "lookupInputType: Can't find input"



--delay' :: KL.Seq SysEnv -> KL.Seq U8 -> KL.Seq U8
--delay' env i = register env (0 :: Comb (U8)) i
