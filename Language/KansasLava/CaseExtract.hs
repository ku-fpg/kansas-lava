{-# LANGUAGE TemplateHaskell, ParallelListComp, UndecidableInstances #-}
module Language.KansasLava.CaseExtract(lava) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote as Q
import qualified Language.Haskell.Exts as Ext



import Control.Monad


parseExprExp :: String -> Q Exp
parseExprExp str =
  case Ext.parseExp str of
    Ext.ParseFailed _ err -> fail err
    Ext.ParseOk val -> convertExpr val

parseExprPat :: String -> Q Pat
parseExprPat str =
  case Ext.parsePat str of
    Ext.ParseFailed _ err -> fail err
    Ext.ParseOk val -> convertPat val


lava = QuasiQuoter parseExprExp parseExprPat


-- Conversion of (Ext) expressions to Q expressions
convertExpr :: Ext.Exp -> Q Exp
convertExpr (Ext.Case e alts) = do
    lcase (convertExpr e) alts'
  where
        alts' :: [(String,[String],ExpQ)]
        alts' = [] -- [| map procAlt alts |]

convertExpr (Ext.Var (Ext.UnQual (Ext.Ident n))) =
  varE (mkName n)
convertExpr e = fail $ show e
convertPat :: Ext.Pat -> Q Pat
convertPat e = fail $ show e


procAlt :: Ext.Alt -> Q (String, [Pat], Exp)
procAlt (Ext.Alt _ (Ext.PApp (Ext.UnQual (Ext.Ident c)) args)  (Ext.UnGuardedAlt body) (Ext.BDecls [])) = do
  body' <- convertExpr body
  return (c,[VarP (mkName n) | Ext.PVar (Ext.Ident n) <- args],body')


-- lcase :: ExpQ -> [(String,[String],ExpQ)] -> ExpQ
lcase dis alts = [|
   let tagWidth = getTag $dis
       tagRange = (tagWidth - 1, 0)
       cons = getCons $dis
       inputs = $(listE $ map (mkAlt [| cons |]) alts)
   in inputs
 |]
  where mkAlts cs [] = [| [] |]
        mkAlts cs (a:as) =   [| $(mkAlt cs a): $(mkAlts cs as) |] --  (mkAlt cs) a `consE` (mkAlts cs as)
        mkAlt cs (n,[],body) = body -- no pattern means no lookup/let binding
        mkAlt cs (n,pvars,body) =
          [| let Just slices = lookup $(litE (stringL n))  $cs
             in  $(mkPats [| slices |] pvars body)
           |]

        consE a b = [| $a : $b |]


mkPats  slices pvars body
  = letE [valD (varP (mkName v)) (normalB [| $slices !! s |]) []
          |  v <- pvars
          |  s <- [0..]]
       body


foo = [d|v = n |]
  where v = "a"
        n :: Int
        n = 0



data Ent = Mux (Int,Int) [String]
         | Slice (Int,Int) Ent
         | Pad String
           deriving Show



type ConsMap = [(String,[(Int,Int)])]
class CType a where
  getTag :: a -> Int
  getCons :: a -> ConsMap

instance Sized a => CType (Maybe a) where
 getTag _ = 1

 getCons _ = [("Nothing",[]),
              ("Just", [(size (undefined :: a), 1)])]


class Sized a where
 size :: a -> Int

instance Sized Bool where
 size _ = 1

instance Sized a => Sized (Maybe a) where
  size _ = 1 + size  (undefined :: a)

instance Sized Int where
  size _ = 32

instance Sized Char where  size _ = 8





mux sel args = [| Mux sel args |]




altLet dis ctor = [| let Just fields = lookup ctor (getCons $dis) in fields |]
  where body = [| () |]
        genDecls :: [(Int,Int)] -> [DecQ]
        genDecls _ =  []



test = (lcase dise alts)
  where dis :: Maybe Int
        dis = Just 0
        dise = [| dis |]
        alts = [("Nothing",[],dise),
                ("Just", ["v"],dise)]


