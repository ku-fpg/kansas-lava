{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.VHDL(vhdlCircuit) where


-- import qualified Language.KansasLava.Entity as E
import Language.KansasLava.Reify(reifyCircuit,Ports,ReifyOptions,ReifiedCircuit(..))
import Language.KansasLava.Entity hiding (name)


import Data.Reify.Graph

import Text.PrettyPrint
import Data.List(intersperse)
import Data.List(find)

import Language.KansasLava.Type

-- | The 'vhdlCircuit' function converts a Lava circuit into a VHDL
--   entity/architecture pair. The circuit type must implement the 'Ports'
--   class.  If the circuit type is a function, the function arguments will be
--   exposed as input ports, and the result will be exposed as an output port
--   (or ports, if it is a compound type).
vhdlCircuit :: (Ports o) =>
               [ReifyOptions] -- ^ Options for controlling the observable-sharing reification.
            -> String         -- ^ The name of the generated entity.
            -> o              -- ^ The Lava circuit.
            -> IO String
vhdlCircuit opts name circuit = do
  (ReifiedCircuit nodes srcs sinks) <- reifyCircuit opts circuit

  let ports = -- need size info for each input, to declare length of std_logic_vector
            [ (nm,"in",vhdlTypes ty) | (Var nm, ty) <- srcs] ++
              -- need size info for each output, to declare length of std_logic_vector
            [ (nm,"out",vhdlTypes ty) | (Var nm,ty,_) <- sinks]

  let ent = VhdlEntity name [] ports

  let arch = VhdlArchitecture name (genDecls nodes) (genInsts nodes ++ genFinals sinks)
  let rendered = render $ pretty ent $$ pretty arch
  return rendered

vhdlTypes :: BaseTy -> String
vhdlTypes B     = "std_logic"
vhdlTypes CB	= "std_logic"			-- control bit
vhdlTypes ClkTy	= "std_logic"			-- control bit
vhdlTypes RstTy	= "std_logic"			-- control bit
vhdlTypes (S n) = "std_logic_vector(" ++ show (n - 1) ++ " downto 0)"
vhdlTypes (U n) = "std_logic_vector(" ++ show (n - 1) ++ " downto 0)"
vhdlTypes ty = error "vhdlTypes: " ++ show ty

genDecls :: [(Unique,Entity BaseTy Unique)] -> [DeclDescriptor]
genDecls nodes =
    -- need size info for each output, to declare length of std_logic_vector
    [ SigDecl (sig (Port (Var n) i)) (vhdlTypes nTy) Nothing
                          | (i,Entity _ outputs _ _) <- nodes
                          , (Var n,nTy) <- outputs
                          ] ++
    concat [ [ SigDecl (sig (Port (Var n) i) ++ "_next") (vhdlTypes nTy) Nothing
     	     ]
           | (i,Entity (Name "Lava" "delay") [(Var n,nTy)] _ _) <- nodes
           ] ++
    concatMap memDecl nodes
  where memDecl (i,Entity (Name "Lava" "BRAM") [(Var n,_)] inputs _) =
          let Just (_,aTy,_) = find (\(Var v,_,_) -> v == "ain") inputs
              Just (_,dTy,_) = find (\(Var v,_,_) -> v == "din") inputs
              asize = baseTypeLength $ aTy
              dsize = baseTypeLength $ dTy
          in [ TypeDecl (sig (Port (Var n) i) ++ "_ram_type")
               ("array(0  to " ++ show (2^asize - 1)  ++ ") of std_logic_vector(" ++ show (dsize - 1)  ++ " downto 0)")
             , SigDecl (sig (Port (Var n) i) ++ "_ram")
                       (sig (Port (Var n) i) ++ "_ram_type") Nothing
             ]
        memDecl _ = []



genFinals :: [(Var,BaseTy,Driver Unique)] -> [Inst]
genFinals args = [ Assign n (sig x) | (Var n,_,x) <- args ]

genInsts :: [(Unique,Entity BaseTy Unique)] -> [Inst]
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

to_integer :: SigToVhdl
to_integer _ (Lit l) =  show l
to_integer ty  p =  "to_integer(" ++ sigTyped ty p ++ ")"


data VhdlOperation =
     VHDLOp OpFix String (BaseTy -> Maybe String) [SigToVhdl]

-- the static list, specials, describes entities
-- that can be directly converted VHDL behavioral expressions
--
specials :: [(Name, VhdlOperation)]
specials =
  [(Name moduleName lavaName, (VHDLOp Infix vhdlName slvCast  [sigTyped, sigTyped]))
       | moduleName <- ["Unsigned", "Signed","Int"]
       , (lavaName,vhdlName) <- [("+","+"), ("-","-"), (".|.","or"), (".&.","and"), (".^.","xor")]]
  ++
  [(Name moduleName lavaName, (VHDLOp Prefix vhdlName slvCast [sigTyped]))
       | moduleName <- ["Unsigned", "Signed","Int"]
       , (lavaName,vhdlName) <- [("negate", "-"), ("complement", "not")]]
  ++
  [(Name moduleName lavaName, (VHDLOp Infix vhdlName (const Nothing)  [sigTyped, sigTyped]))
       | moduleName <- ["Bool"]
          , (lavaName,vhdlName) <- [("or2","or"), ("and2","and"), ("xor2","xor")]]
  ++
  [(Name moduleName lavaName, (VHDLOp Prefix vhdlName (const Nothing) [sigTyped]))
       | moduleName <- ["Bool"]
       , (lavaName,vhdlName) <- [("not", "not")]]
  ++
  [(Name moduleName lavaName, (VHDLOp Infix vhdlName active_high  [sigTyped, sigTyped]))
       | moduleName <- ["Unsigned", "Signed","Int","Bool"]
       , (lavaName,vhdlName) <- [(".<.","<"), (".>.",">"),(".==.","="),(".<=.","<="), (".>=.",">=") ]]
  ++
    -- The VHDL operator "sll" (Shift Left Logical)
    -- actually performs sign extension IF the first argument is Signed
    -- and the effective shift direction is to the "right"
    -- (i.e. the shift count is negative)-}
  [(Name moduleName lavaName, (VHDLOp Infix vhdlName slvCast  [sigTyped, to_integer]))
       | moduleName <- ["Unsigned", "Signed"]
       , (lavaName,vhdlName) <- [("shift","sll")]]


mkInst :: Unique -> Entity BaseTy Unique -> [Inst]
mkInst i (Entity n@(Name _ _) [(Var "o0",oTy)] ins _)
        | Just (VHDLOp opFix opName outConv inConvs) <- lookup n specials =
          [ BuiltinInst opFix (sig (Port (Var "o0") i)) opName
                        [f inTy driver | f <- inConvs
                                       | (_, inTy, driver) <- ins]
                        (outConv oTy)]

-- Delay Circuits
mkInst i (Entity (Name "Lava" "delay") [(Var "o0",_)]
			[ (Var "clk",_,_)
			, (Var "rst",_,_)
			, (Var "init",_,_)
			, (Var "i",_,y)
			] _) =
          [ Assign input (sig y) ]
  where output = sig (Port (Var "o0") i)
        input =  output ++ "_next"

-- Muxes
mkInst i (Entity (Name "Bool" "mux2") [(Var "o0",_)] [(Var "c",cTy,c),(Var "t",tTy,t),(Var "f",fTy,f)] _)
	= [ CondAssign (sig (Port (Var "o0") i))
                       [(ocast fTy f, sigTyped cTy c ++ "= '0'")]
                       (ocast tTy t)
         ]
mkInst _ (Entity (Name "Bool" "mux2") x y _) = error $ show (x,y)

mkInst  i (Entity (Name "Lava" "concat") [(Var "o0",_)] inps _) =
                  [Assign (sig (Port (Var "o0") i)) val]
  where val = concat $ intersperse "&"
              -- Note the the layout is reversed
              [sig s | (Var _,_, s) <- reverse inps]


-- The 'Top' entity should not generate anything, because we've already exposed
-- the input drivers via theSinks.
mkInst _ (Entity (Name "Lava" "top") _ _ _) = []

mkInst  _ (Entity (Name "Lava" "BRAM") _ _ _) = []

mkInst i (Entity (Name "Lava" "index") [(Var "o0",_)] [(Var "i", _, input),(Var "index",_,idx)] _) =
  [Assign (sig (Port (Var "o0") i))
          (sig input ++ "(" ++ sig idx ++ ")")]

mkInst i (Entity (Name "Lava" "slice") [(Var "o0",B)]
                  [(Var "i0", _, input),(Var "low",_,low),(Var "high",_,high)] _) 
    | high == low =   [Assign (sig (Port (Var "o0") i))
              (sig input ++ "(" ++ sig high ++ ")")]
    | otherwise = error "high and low indices do not match for bit slice"

mkInst i (Entity (Name "Lava" "slice") [(Var "o0",_)]
                  [(Var "i0", _, input),(Var "low",_,low),(Var "high",_,high)] _) =
  [Assign (sig (Port (Var "o0") i))
          (sig input ++ "(" ++ sig high ++ " downto " ++ sig low ++ ")")]


mkInst i (Entity (Name "probe" _) [(Var "o0",_)]
                  [(Var "i0", _, input)] _) =
  [Assign (sig (Port (Var "o0") i))
          (sig input)]


-- Catchall for everything else
mkInst i (Entity (Name _ nm) outputs inputs _) =
          [ Inst ("inst" ++ show i) nm
                []
                ( [ (n,sig (Port (Var n) i)) | (Var n,_) <- outputs ] ++
                  [ (n,sigTyped nTy x) | (Var n,nTy,x) <- inputs ]
                )
          ]
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


-- The 'synchronous' function generates a synchronous process for the
-- delay elements
synchronous :: [(Unique, Entity BaseTy Unique)] -> [Inst]
synchronous nodes
  | null delays && null brams  = []
  | otherwise = [Process ([sig clk, sig rst] ++  inputs ++ bramWE ++  bramData)
                       [ Cond ("rising_edge(" ++ sig clk ++ ")")
		         ((if null delays
                          then []
                          else
                            [Cond (sig rst ++ "='1'")
		             (zipWith Assign outputs inits)
                             (zipWith Assign outputs inputs)
	                    ])  ++
                         [(Cond (we ++ "='1'")
                                [Assign target dat,
                                 Assign o "(others => '0')"]
                                [Assign o target])
                           | we <- bramWE
                           | o <- bramOuts
                           | target <- bramTargets
                           | dat <- bramData
                          ]) -- Then
                         [] -- Else
	               ]
                     ]
  where -- Handling registers
        delays = [(i,e) | (i,e@(Entity (Name "Lava" "delay") [(Var _,_)] _ _)) <- nodes]
        ((_,_):_) = delays
        outputs = [sig (Port (Var "o0") i) | (i,_) <- delays]
        inputs = [o ++ "_next" | o <- outputs]
        inits = [ocast ty (inputDriver "init" e)
                 | (_,e@(Entity (Name "Lava" "delay") [(Var _,ty)] _ _)) <- delays]


        Just (_,_,clk)  = lookupInput "clk" (head (map snd (delays ++ brams)))
        Just (_,_,rst) =  lookupInput "rst" (head (map snd (delays ++ brams)))

        inputDriver i e = let (Just (_,_,d)) = lookupInput i e
                          in d
        lookupInput i (Entity _ _ inps _) = find (\(Var v,_,_) -> v == i) inps

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


-- Helper functions, for converting drivers to VHDL
sig :: Driver Unique -> String
-- sig (Port i Sink)   = show i	-- pre normalized
-- sig (Port i Source) = show i
-- sig (Port i (Uq v)) = "sig_" ++ show i ++ "_" ++ show v
sig (Port (Var v) d) = "sig_" ++  show d ++ "_" ++ v
sig (Pad (Var n)) = n
sig (Lit n) = show n
sig p = error $ "sig: " ++ show p

-- sigTyped allows the type casts, especially for literals.
sigTyped :: BaseTy -> Driver Unique -> [Char]
sigTyped (U width) (Lit n) = "to_unsigned(" ++ show n ++ "," ++ show width ++ ")"
sigTyped (S width) (Lit n) = "to_signed(" ++ show n ++ "," ++ show width ++ ")"
sigTyped B         (Lit 0) = "'0'"
sigTyped B         (Lit 1) = "'1'"
sigTyped B     s = sig s
sigTyped (U _) s = "unsigned(" ++ sig s ++ ")"
sigTyped (S _) s = "signed(" ++ sig s ++ ")"
sigTyped ClkTy s = sig s
sigTyped RstTy s = sig s

sigTyped ty s = error $ "sigtyped :" ++ show ty ++ "/" ++ show s

-- ocast is for assigments where the driver may need to be cast
ocast :: BaseTy -> Driver Unique -> [Char]
ocast B (Lit i) = "'" ++ show i ++ "'"
ocast CB (Lit i) = "'" ++ show i ++ "'"
ocast ClkTy (Lit i) = "'" ++ show i ++ "'"
ocast RstTy (Lit i) = "'" ++ show i ++ "'"
ocast B x = "std_logic(" ++ sig x ++ ")"
ocast CB x = "std_logic(" ++ sig x ++ ")"
ocast ClkTy x = "std_logic(" ++ sig x ++ ")"
ocast RstTy x = "std_logic(" ++ sig x ++ ")"
ocast ty driver = "std_logic_vector" ++ "(" ++ sigTyped ty driver ++ ")"




-- * The VHDL data structure and pretty-printing routines

type GenericDescriptor = (String,String,Maybe String) -- (name,type,default)
type PortDescriptor = (String,String,String) -- Name, Mode (in or out),Type
data DeclDescriptor = SigDecl String String (Maybe String) -- name, type, default
                    | TypeDecl String String -- name, value
                    deriving Show


data VhdlStruct = VhdlEntity String [GenericDescriptor] [PortDescriptor]
                | VhdlArchitecture String -- VhdlEntity
                  [DeclDescriptor]
                  [Inst]
            deriving Show
data OpFix = Prefix | Infix | PostFix | NoFix deriving (Show)

data Inst = Assign String String
          -- a <= b;
          | BuiltinInst OpFix String String [String] (Maybe String) -- output,opname, [in1,in2],conv
          -- a <= conv (op (b));
--          | BuiltinInst String String String String (Maybe String) -- a,b,op,c,conv
          -- a <= conv (b op c);
          | Inst String String [(String,String)] [(String,String)]
            -- String, VhdlEntity, DeclDescriptor Map, PortDescriptor Map
	  | Cond String	-- condition
		 [Inst]
		 [Inst]
	  | CondAssign String
		        [(String,String)]	-- (Val, Cond)
			String
	  | Case String 
		       [(String,Inst)]	-- Match, Val
          | Comment String
          | Process [String]
		    [Inst]
          deriving Show


class Pretty a where
  pretty :: a -> Doc

instance Pretty VhdlStruct where
  pretty (VhdlEntity name generics ports) =
    imports $$
    (text "entity" <+> text name <+> text "is") $$
    gdoc $$
    (nest 2 ((text "port") <+> parens pdoc) <> semi) $$
    (text "end entity") <+> text name <> semi
   where pdoc = (cat $ punctuate semi $ map printPort ports)
         printPort (n,portDir,ty) = text n <+> colon <+> text portDir <+> text ty
         gdoc | null generics = empty
              | otherwise = nest 2 $ (text "generic") <+>
                                     parens (vcat $ map printGen generics)
         printGen (n,ty,Nothing) = text n <+> colon <+> text ty <> semi
         printGen (n,ty,Just def) = text n <+> colon <+> text ty <+> text ":=" <+> text def <> semi
         imports = vcat $ map text ["library IEEE;",
                                    "use IEEE.STD_LOGIC_1164.ALL;",
                                    "use IEEE.NUMERIC_STD.ALL;",
--                                    "use work.lava.all",
                                    "use work.all;"]


  pretty (VhdlArchitecture entity decls insts) =
      (text "architecture str of") <+> text entity <+> text "is" $$
      ddoc $$
      hang (text "begin") 2 (vcat $ map pretty insts) $$
      text "end architecture" <> semi
   where ddoc = vcat $ map printDecl decls
         printDecl (SigDecl name ty Nothing) =
           text "signal" <+> text name <+> colon <+> text ty <> semi
         printDecl (SigDecl  name ty (Just def)) =
           text "signal" <+> text name <+> colon <+> text ty <+> text ":=" <+> text def <> semi
         printDecl (TypeDecl name ty) =
           text "type" <+> text name <+> text "is" <+> text ty <> semi


instance Pretty Inst where
  pretty (Assign a b) = text a <+> text "<=" <+> text b <> semi
--  pretty (BuiltinInst a b op c Nothing) = text a <+> text "<=" <+> text b <+> text op <+> text c <> semi
  -- pretty (BuiltinInst a op [in1, in2] Nothing) = text a <+> text "<=" <+> text in1 <+> text op <+> text in2 <> semi
  pretty (BuiltinInst Infix a op [in1, in2] cval) = text a <+> text "<=" <+> (cast (text in1 <+> text op <+> text in2)) <> semi
    where cast t = maybe t (\s -> (text s) <> parens t) cval
  pretty (BuiltinInst Prefix a op [in1] cval) = text a <+> text "<=" <+> (cast (text op <+> text in1)) <> semi
    where cast t = maybe t (\s -> (text s) <> parens t) cval

  pretty (BuiltinInst PostFix a op [in1] cval) = text a <+> text "<=" <+> (cast (text in1 <+> text op)) <> semi
    where cast t = maybe t (\s -> (text s) <> parens t) cval

  pretty (BuiltinInst NoFix a op ins cval) =
      text a <+> text "<=" <+> (cast (text op <> parens (cat (punctuate comma (map text ins))))) <> semi
    where cast t = maybe t (\s -> (text s) <> parens t) cval

  pretty i@(BuiltinInst _ _ _ _ _) = error $ "Unmatched pattern in Pretty.pretty(Inst)" ++ show i


--  pretty (BuiltinInst1 a b op  Nothing) = text a <+> text "<="  <+> text op <> parens (text b)  <> semi
--  pretty (BuiltinInst1 a b op  (Just conv)) = text a <+> text "<=" <+>  text conv <> parens (text op <> parens (text b) ) <> semi

  pretty (Cond cond ifT []) =
    text "if" <+> text cond <+> text "then" $$
      nest 4 (vcat $ map pretty ifT) $$
      (text "end if") <> semi
  pretty (Cond cond ifT ifF) =
    text "if" <+> text cond <+> text "then" $$
      nest 4 (vcat $ map pretty ifT) $$
      (text "else") $$
      nest 4 (vcat $ map pretty ifF) $$
      (text "end if") <> semi

  pretty (Inst lab ent generics ports) =
    text lab <+> colon <+> text "entity" <+> text ent $$
      nest 2 gmap $$
      nest 2 pmap <> semi
    where gmap | null generics = empty
               | otherwise = text "generic map" <> parens (hcat $ intersperse comma $ map printMap generics)
          pmap | null ports = empty
               | otherwise = text "port map" <> parens (hcat $ intersperse comma $ map printMap ports)
          printMap (p,t) = text p <+> text "=>" <+> text t
  pretty (Comment str) = text "--" <+> text str
  pretty (Process watch insts) =
	hang (text "process" <+> parens (hcat $ intersperse comma $ map text watch)
	 	       <+> text "is") 2
	   (hang (text "begin") 2 (vcat $ map pretty insts) $$
	   text "end process" <> semi)

  pretty (CondAssign a alts def) =
	text a <+> text "<=" <+> (sep $ (map prettyAlt alts ++ [text def <> semi]))
    where prettyAlt (val,cond) = text val <+> text "when" <+> parens (text cond) <+> text "else"

  pretty (Case e alts) =
	text "CASE" <+> text e <+> text "IS" $$
	   nest 2 (sep $ map prettyAlt alts) $$
	text "END CASE" <> semi
      where prettyAlt (match,cmd) = text "WHEN" <+> text match <+> text "=>" <+> pretty cmd









