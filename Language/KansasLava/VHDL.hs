{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances, PatternGuards,ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-} -- REMOVE
module Language.KansasLava.VHDL(vhdlCircuit) where


-- import qualified Language.KansasLava.Entity as E
import Language.KansasLava.Reify
import Language.KansasLava.Signal
import Language.KansasLava.IO
import Language.KansasLava.Entity hiding (name)


import Data.Reify.Graph

import Text.PrettyPrint
import Data.List(intersperse)
import Data.Bits
import Data.List(mapAccumL,find)
import Control.Monad(liftM)
import Data.Maybe(catMaybes,fromJust)

import Language.KansasLava.Type
import qualified Data.Set as Set


-- fixed_ports = []--  [("clk,rst", "in", "std_logic")]           --- error, hack?


vhdlCircuit :: (Ports o) =>  [ReifyOptions] -> String -> o -> IO String
vhdlCircuit opts name circuit = do
  (ReifiedCircuit nodes srcs sinks) <- reifyCircuit opts circuit

  let ports = -- need size info for each input, to declare length of std_logic_vector
            [ (nm,"in",vhdlTypes ty) | (Var nm, ty) <- srcs] ++
              -- need size info for each output, to declare length of std_logic_vector
            [ (nm,"out",vhdlTypes ty) | (Var nm,ty,_) <- sinks]

  let ent = VhdlEntity name [] ports

  let arch = VhdlArchitecture name (decls nodes) (insts nodes ++ finals sinks)
  let rendered = render $ pretty ent $$ pretty arch
  putStrLn rendered
  return rendered

ports :: (Graph (Entity a)) -> [PortDescriptor]
ports (Graph nodes out) =
--  [(n,"in","std_logic_vector") | (i,Pad (Var n)) <- nodes, notConst n] ++
 -- [("clk,rst", "in", "std_logic")] ++
  [("sig" ++ show out, "out", "std_logic_vector")]

{-
notConst "low" = False
notConst "high" = False
notConst _ = True
-}

vhdlTypes :: BaseTy -> String
vhdlTypes B     = "std_logic"
vhdlTypes CB	= "std_logic"			-- control bit
vhdlTypes ClkTy	= "std_logic"			-- control bit
vhdlTypes RstTy	= "std_logic"			-- control bit
vhdlTypes (S n) = "std_logic_vector(" ++ show (n - 1) ++ " downto 0)"
vhdlTypes (U n) = "std_logic_vector(" ++ show (n - 1) ++ " downto 0)"

decls :: [(Unique,Entity BaseTy Unique)] -> [DeclDescriptor]
decls nodes =
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
  where memDecl (i,Entity (Name "Lava" "BRAM") [(Var n,nTy)] inputs _) =
          let Just (_,aTy,_) = find (\(Var v,_,_) -> v == "ain") inputs
              Just (_,dTy,_) = find (\(Var v,_,_) -> v == "din") inputs
              asize = baseTypeLength $ aTy
              dsize = baseTypeLength $ dTy
              outputType = vhdlTypes $ dTy
          in [ TypeDecl (sig (Port (Var n) i) ++ "_ram_type")
                        ("array(0  to " ++ show (2^asize -1)  ++ ") of std_logic_vector(" ++ show (dsize - 1)  ++ " downto 0)")
             , SigDecl (sig (Port (Var n) i) ++ "_ram")
                       (sig (Port (Var n) i) ++ "_ram_type") Nothing
             ]
        memDecl _ = []



finals :: [(Var,BaseTy,Driver Unique)] -> [Inst]
finals args = [ Assign n (sig x) | (Var n,_,x) <- args ]

insts :: [(Unique,Entity BaseTy Unique)] -> [Inst]
insts nodes = concat [
               mkInst i ent
                 | (i,ent@(Entity _ outs ins _)) <- nodes ] ++
              (synchronous nodes)

fixName ".&." = "AND"
fixName "xor2" = "XOR"
fixName ".|." = "OR"
fixName xs    = xs

-- infix specials
isSpecialName :: Name -> Maybe String
isSpecialName (Name "Bool" "+") = return "XOR"
isSpecialName (Name "Bool" "*") = return "AND"
isSpecialName _ = Nothing


specials =
  [(Name moduleName lavaName,
         (vhdlName, Just "std_logic_vector"))
   | moduleName <- ["Unsigned", "Signed","Int"]
  , (lavaName,vhdlName) <-
    [("+","+"), ("-","-")]] ++
  [(Name moduleName lavaName,
         (vhdlName, Nothing))
   | moduleName <- ["Unsigned", "Signed","Bool"]
  , (lavaName,vhdlName) <-
    [(".<.","<"), (".>.",">"),(".==.","=")]] ++

  [(Name moduleName lavaName,
         (vhdlName, Just "std_logic_vector"))
   | moduleName <- ["Unsigned", "Signed"]
  , (lavaName,vhdlName) <-
    [(".|.","or"), (".&.","and"), (".^.","xor")]] ++
  [(Name moduleName lavaName,
         (vhdlName, Nothing))
   | moduleName <- ["Bool"]
  , (lavaName,vhdlName) <-
    [("or2","or"), ("and2","and"), ("xor2","xor")]]





mkInst :: Unique -> Entity BaseTy Unique -> [Inst]
-- mkInst _ e = error $ show e
--    mkInst i (Pad (Var "low")) = Assign (sig i) "'0'"
--    mkInst i (Pad (Var "high")) = Assign (sig i) "'1'"

mkInst i e@(Entity n@(Name mod nm) [(Var "o0",oTy)] [(Var "i0",xTy,x),(Var "i1",yTy,y)] _)
        | Just (nm',cast) <- lookup n specials =
          [ BuiltinInst (sig (Port (Var "o0") i))
                        (sigTyped xTy x)
                        nm'
                        (sigTyped yTy y) cast
          ]

{-
mkInst tyEnv i e@(Entity nm [Var "o0"] [(Var "i0",x),(Var "i1",y)] _)
	| Just op <- isSpecialName nm
	  = [ BuiltinInst (sig (Port (Var "o0") (Uq i)))
                        (sig x)
                        (op)
                        (sig y) Nothing
            ]
mkInst tyEnv i e@(Entity (Name mod nm) [Var "o0"] [(Var "i0",x),(Var "i1",y)] _)
        | mod == "Bool" && nm `elem` (["xor",".&.",".|."] ++ ["xor2","and2","or2"])
        = [ BuiltinInst (sig (Port (Var "o0") (Uq i)))
                        (sig x)
                        (fixName nm)
                        (sig y) Nothing
          ]

mkInst tyEnv i e@(Entity (Name mod nm) [Var "o0"] [(Var "i0",x),(Var "i1",y)] _)
        | {- mod == "Unsigned"  && -} nm `elem` ["+", "-"] =
          [ BuiltinInst (sig (Port (Var "o0") (Uq i)))
                        (sigTyped (getTy tyEnv i "i0") x)
                        (fixName nm)
                        (sigTyped (getTy tyEnv i "i1") y) (Just "std_logic_vector")
          ]

-}



-- Delay Circuits
mkInst i e@(Entity (Name "Lava" "delay") [(Var "o0",oTy)]
			[ (Var "clk",clkTy,clk)
			, (Var "rst",rstTy,rst)
			, (Var "init",initTy,x)
			, (Var "i",iTy,y)
			] _) =
          [ Assign input (sig y) ]
  where output = sig (Port (Var "o0") i)
        input =  output ++ "_next"

-- Muxes
mkInst i e@(Entity (Name "Bool" "mux2") [(Var "o0",oTy)] [(Var "c",cTy,c),(Var "t",tTy,t),(Var "f",fTy,f)] _)
	= [ CondAssign (sig (Port (Var "o0") i))
                       [(sigTyped fTy f, sigTyped cTy c ++ "= '0'")]
                       (sigTyped tTy t)
         ]
mkInst i e@(Entity (Name "Bool" "mux2") x y _) = error $ show (x,y)

mkInst  i e@(Entity (Name "Lava" "concat") [(Var "o0",oTy)] inps _) =
                  [Assign (sig (Port (Var "o0") i)) val]
  where val = concat $ intersperse "&"
              -- Note the the layout is reversed
              [sigTyped sigTy sig | (Var v,sigTy, sig) <- reverse inps]


-- The 'Top' entity should not generate anything, because we've already exposed
-- the input drivers via theSinks.
mkInst _ (Entity (Name "Lava" "top") _ _ _) = []

mkInst  _ e@(Entity (Name "Lava" "BRAM") _ _ _) = []

mkInst i e@(Entity (Name "Lava" "index") [(Var "o0",oTy)] [(Var "i", iTy, input),(Var "index",indexTy,idx)] _) =
  [Assign (sig (Port (Var "o0") i))
          (sig input ++ "(" ++ sig idx ++ ")")]

mkInst i e@(Entity (Name "Lava" "slice") [(Var "o0",oTy)]
                  [(Var "i", inputTy, input),(Var "low",lowTy,low),(Var "high",highTy,high)] _) =
  [Assign (sig (Port (Var "o0") i))
          (sig input ++ "(" ++ sig high ++ " downto " ++ sig low ++ ")")]


-- Catchall for everything else
mkInst i e@(Entity (Name mod nm) outputs inputs _) =
          [ Inst ("inst" ++ show i) nm
                []
                ( [ (n,sig (Port (Var n) i)) | (Var n,nTy) <- outputs ] ++
                  [ (n,sigTyped nTy x) | (Var n,nTy,x) <- inputs ]
                )
          ]



-- The 'synchronous' function generates a synchronous process for the
-- delay elements
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
        delays = [(i,e) | (i,e@(Entity (Name "Lava" "delay") [(Var n,_)] _ _)) <- nodes]
        ((_,d):_) = delays
        outputs = [sig (Port (Var "o0") i) | (i,_) <- delays]
        inputs = [o ++ "_next" | o <- outputs]
        inits = [sigTyped (inputType "init" e) (inputDriver "init" e)
                 | (i,e@(Entity (Name "Lava" "delay") [(Var n,_)] _ _)) <- delays]

        Just (_,clkTy,clk)  = lookupInput "clk" (head (map snd (delays ++ brams)))
        Just (_,rstTy,rst) =  lookupInput "rst" (head (map snd (delays ++ brams)))

        inputDriver i e = let (Just (_,_,d)) = lookupInput i e
                          in d
        inputType i e = let (Just (_,ty,_)) = lookupInput i e
                        in ty
        lookupInput i (Entity _ outputs inputs _) = find (\(Var v,_,_) -> v == i) inputs

        -- Handling BRAMS
        brams = [(i,e) | (i,e@(Entity (Name "Lava" "BRAM") _ _ _)) <- nodes]
        bramOuts =[sig (Port (Var "o0") i) | (i,_) <- brams]
        bramSigs = [(sig (Port (Var n) i) ++ "_ram")
                      | (i,Entity _ [(Var n,_)] _ _) <- nodes]
        bramAddr = ["conv_integer(" ++ sig (inputDriver "ain" e) ++ ")"
                   | (i,e) <- brams]
        bramData = [sig (inputDriver "din" e)
                   | (i,e) <- brams]
        bramWE = [sig (inputDriver "we" e)
                   | (i,e) <- brams]
        bramTargets = [s ++ "(" ++ a ++ ")" |  s <- bramSigs | a <- bramAddr]


-- Helper functions, for converting drivers to VHDL
sig :: Driver Unique -> String
-- sig (Port i Sink)   = show i	-- pre normalized
-- sig (Port i Source) = show i
-- sig (Port i (Uq v)) = "sig_" ++ show i ++ "_" ++ show v
sig (Port (Var v) d) = "sig_" ++  show d ++ "_" ++ v
sig (Pad (Var n)) = n
sig (Lit n) = show n

-- sigTyped allows the type casts, especially for literals.
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

data Inst = Assign String String
          -- a <= b;
          | BuiltinInst String String String String (Maybe String) -- a,b,op,c,conv
          -- a <= conv (b op c);
          | Inst String String [(String,String)] [(String,String)]
            -- String, VhdlEntity, DeclDescriptor Map, PortDescriptor Map
	  | Cond String	-- condition
		 [Inst]
		 [Inst]
	  | CondAssign String
		        [(String,String)]	-- (Val, Cond)
			String
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
         printPort (n,mode,ty) = text n <+> colon <+> text mode <+> text ty
         gdoc | null generics = empty
              | otherwise = nest 2 $ (text "generic") <+>
                                     parens (vcat $ map printGen generics)
         printGen (n,ty,Nothing) = text n <+> colon <+> text ty <> semi
         printGen (n,ty,Just def) = text n <+> colon <+> text ty <+> text ":=" <+> text def <> semi
         imports = vcat $ map text ["library IEEE;",
                                    "use IEEE.STD_LOGIC_1164.ALL;",
                                    "use IEEE.NUMERIC_STD.ALL;",
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
  pretty (BuiltinInst a b op c Nothing) = text a <+> text "<=" <+> text b <+> text op <+> text c <> semi
  pretty (BuiltinInst a b op c (Just conv)) = text a <+> text "<=" <+> text conv <> parens (text b <+> text op <+> text c) <> semi
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
	text a <+> text "<=" <+> (hsep $ map prettyAlt alts) <+> text def <> semi
    where prettyAlt (val,cond) = text val <+> text "when" <+> parens (text cond) <+> text "else"









