{-# LANGUAGE FlexibleInstances,TypeFamilies, UndecidableInstances #-}
module Language.KansasLava.VHDL where


-- import qualified Language.KansasLava.Entity as E
import Language.KansasLava.Reify
import Language.KansasLava.Signal
import Language.KansasLava.IO
import Language.KansasLava.Entity hiding (name)


import Data.Reify.Graph

import Text.PrettyPrint
import Data.List(intersperse)
import Data.Bits
import Data.List(mapAccumL)
import Control.Monad(liftM)

import Language.KansasLava.Type
import qualified Data.Set as Set


fixed_ports = 
  [("clk,rst", "in", "std_logic")]           --- error, hack?


vhdlCircuit :: (REIFY o) =>  [ReifyOptions] -> String -> o -> IO ()
vhdlCircuit opts name circuit = do
{-
  let (ins',g) = output' circ
  let ins = map fst ins'
  -- for debugging (not part of VHDL output)

--  print (ins,outs)
  -- This could be better
-}
  (ReifiedCircuit nodes ins outs' types) <- reifyCircuit opts circuit
                        -- a sort of comsumer for the entity being generated
  -- for debugging (not part of VHDL output)
  print (nodes,ins,outs',types)

  let findTyFor :: QVar -> Ty ()
      findTyFor v = case lookup v types of
		    Nothing -> error $ "can not find type for : " ++ show v
		    Just ty -> ty

--  print $ map findTyFor [(-1,v) | v <- ins ]
  print $ map findTyFor [(Uq i,v) | (_,Port v i) <- outs' ]

  let ports = fixed_ports ++ 
              -- need size info for each input, to declare length of std_logic_vector
            [ (nm,"in",vhdlTypes (findTyFor (Source,Var nm))) | Var nm <- ins] ++
              -- need size info for each output, to declare length of std_logic_vector
            [ (nm,"out",vhdlTypes (findTyFor (Uq i,v))) | (Var nm,Port v i) <- outs'] ++ 
            [ (nm,"out",vhdlTypes (findTyFor (Source,v))) | (Var nm,Pad v) <- outs'] ++ 
            []

  let ent = VhdlEntity name [] ports
  putStrLn $ render $ pretty ent
  let arch = VhdlArchitecture name (decls findTyFor nodes) (insts nodes ++ finals outs')
  putStrLn $ render $ pretty arch

ports :: (Graph (Entity a)) -> [PortDescriptor]
ports (Graph nodes out) =
--  [(n,"in","std_logic_vector") | (i,Pad (Var n)) <- nodes, notConst n] ++
  [("clk,rst", "in", "std_logic")] ++
  [("sig" ++ show out, "out", "std_logic_vector")]

{-
notConst "low" = False
notConst "high" = False
notConst _ = True
-}

vhdlTypes :: Ty () -> String
vhdlTypes B     = "std_logic"
vhdlTypes (S n) = "std_logic_vector(" ++ show n ++ ")"
vhdlTypes (U n) = "std_ulogic_vector(" ++ show n ++ ")"

decls :: (QVar -> Ty ()) -> [(Unique,Entity ty Unique)] -> [DeclDescriptor]
decls tyEnv nodes = 
    -- need size info for each output, to declare length of std_logic_vector
    [ (sig (Port (Var n) i),vhdlTypes (tyEnv (Uq i,Var n)),Nothing) 
                          | (i,Entity _ outputs _ _) <- nodes
                          , Var n <- outputs
                          ]

finals :: [(Var,Driver Unique)] -> [Inst]
finals args = [ Assign n (sig x) | (Var n,x) <- args ] 

insts :: [(Unique,Entity ty Unique)] -> [Inst]
insts nodes = concat [ 
{-        if i == root
        then [ Assign n (sig x) | (Var n,x) <- ins ] 
        else -} mkInst i ent 
     | (i,ent@(Entity _ outs ins _)) <- nodes ]

fixName ".&." = "AND"
fixName "xor" = "XOR"
fixName ".|." = "OR"
fixName xs    = xs

mkInst :: Int -> Entity ty Int -> [Inst]
--    mkInst i (Pad (Var "low")) = Assign (sig i) "'0'"
--    mkInst i (Pad (Var "high")) = Assign (sig i) "'1'"
mkInst i e@(Entity (Name mod nm) [Var "o0"] [(Var "i0",x),(Var "i1",y)] _)
        | nm `elem` ["xor",".&.",".|."]
        = [ BuiltinInst (sig (Port (Var "o0") i)) 
                        (sig x)
                        (fixName nm)
                        (sig y)
          ]
mkInst i e@(Entity (Name "Bool" "mux2") [Var "o0"] [(Var "c",c),(Var "t",t),(Var "f",f)] _)
	= [ Cond (sig c)
		[ Assign (sig (Port (Var "o0") i)) (sig t)
		]
		[ Assign (sig (Port (Var "o0") i)) (sig f)
		]
         ]
mkInst i e@(Entity (Name "Bool" "mux2") x y z) = error $ show (x,y)

mkInst i e@(Entity (Name mod nm) outputs inputs _) =
          [ Inst ("inst" ++ show i) nm
                []
                ( [ (n,sig (Port (Var n) i)) | Var n <- outputs ] ++
                  [ (n,sig x) | (Var n,x) <- inputs ]
                )
          ]
{-
        mkInst i (Entity (Name _ "mux") inputs) =
          inst i "mux" "o" ["s","d0","d1"] inputs
        mkInst i (Entity (Name _ "not") [j]) =
          Assign (sig i) ("not " ++  (sig j))
        mkInst i (Entity (Name _ "xor2") inputs) =
          inst i "xor2" "o" ["i0","i1"] inputs
        mkInst i (Entity (Name "Bool" ".&.") inputs) =
          inst i "and2" "o" ["i0","i1"] inputs
        mkInst i (Entity (Name _ "xor") inputs) =
          inst i "xor2" "o" ["i0","i1"] inputs
        mkInst i (Entity (Name "$" n) inputs) = Comment $ show (n,inputs)
        mkInst i (Pad (Var n)) = Assign (sig i) n
        mkInst i (Lit x) = Assign (sig i) ("conv_std_logic(" ++ show x ++ ")")
        mkInst i (Entity (Name "Int" "+") [l,r]) = Assign (sig i)  (sig l ++ " + " ++ sig r)
        mkInst i (Entity (Name "Int" "fromInteger") [x]) = Assign (sig i) (sig x)

        mkInst i ent = error $ "BAD INSTRUCTION: " ++ show i ++ " " ++ show (ent,nodes)
-}
sig :: Driver Int -> String
sig (Port i 1) = "io_sig_" ++ show i
sig (Port i v) = "sig_" ++ show i ++ "_" ++ show v
sig (Pad (Var n)) = n
sig (Lit n) = show n

--        inst i entity o formals actuals =
--          Inst ("inst" ++ show i) entity [] ((o,sig i):(zip formals (map sig actuals)))


--implode' :: Ex (a,b) -> Signal (a,b)
--implode' = implode 
{-  
instance Compile b => Compile (Signal a -> b) where
  compile f = compile (f port)
    where port = Signal (error "input port") $ Wire $ Pad (Var ("input" ++ show (pos f)))
  pos f = pos (undefined `asTypeOf` f undefined) + 1
-}


type GenericDescriptor = (String,String,Maybe String)
-- (name,type,default)
type PortDescriptor = (String,String,String)
-- Name, Mode (in or out),Type
type DeclDescriptor = (String,String,Maybe String)
-- (name,type,default)

data VhdlStruct = VhdlEntity String [GenericDescriptor] [PortDescriptor]
                | VhdlArchitecture String -- VhdlEntity
                  [DeclDescriptor]
                  [Inst]
            deriving Show

data Inst = Assign String String
          -- a <= b;
          | BuiltinInst String String String String
          -- a <= b op c;
          | Inst String String [(String,String)] [(String,String)]
            -- String, VhdlEntity, DeclDescriptor Map, PortDescriptor Map
	  | Cond String	-- condition
		 [Inst]
		 [Inst]
          | Comment String
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
                                    "use IEEE.STD_LOGIC_ARITH.ALL;",
                                    "use IEEE.STD_LOGIC_UNSIGNED.ALL;",
                                    "use work.all;"]


  pretty (VhdlArchitecture entity decls insts) =
      (text "architecture str of") <+> text entity <+> text "is" $$
      ddoc $$
      hang (text "begin") 2 (vcat $ map pretty insts) $$
      text "end architecture" <> semi
   where ddoc = vcat $ map printDecl decls
         printDecl (name,ty,Nothing) =
           text "signal" <+> text name <+> colon <+> text ty <> semi
         printDecl (name, ty, Just def) =
           text "signal" <+> text name <+> text ty <+> text ":=" <+> text def <> semi

instance Pretty Inst where
  pretty (Assign a b) = text a <+> text "<=" <+> text b <> semi
  pretty (BuiltinInst a b op c) = text a <+> text "<=" <+> text b <+> text op <+> text c <> semi
  pretty (Cond cond ifT ifF) =
    text "if" <+> text cond <+> text "= '0'" <+> text "then" $$
      nest 4 (vcat $ map pretty ifF) $$	-- false, because we cmp with '0'
      (text "else") $$
      nest 4 (vcat $ map pretty ifT) $$
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





