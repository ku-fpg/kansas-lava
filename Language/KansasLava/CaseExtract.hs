{-# LANGUAGE TemplateHaskell, ParallelListComp, UndecidableInstances #-}
module Language.KansasLava.CaseExtract(lava,Ent(..)) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote as Q
import Language.Haskell.Meta.Parse as Ext


import Data.List(intersperse)
import Control.Monad


parseExprExp :: String -> Q Exp
parseExprExp str =
  case parseExp str of
    Left err -> fail err
    Right val -> convertExpr val

parseExprPat :: String -> Q Pat
parseExprPat str =
  case parsePat str of
    Left err -> fail err
    Right val -> return val


lava = QuasiQuoter parseExprExp parseExprPat


-- Conversion of 'Haskell' to 'Lava'
convertExpr :: Exp -> Q Exp
convertExpr (CaseE e alts) = do
    lcase (convertExpr e) alts
  where
        alts' :: [(String,[String],ExpQ)]
        alts' = []

convertExpr e = return e

-- lcase :: ExpQ -> [(String,[String],ExpQ)] -> ExpQ
lcase dis alts = [|
   let tagWidth = getTag $dis
       tagRange = (tagWidth - 1, 0)
       cons = getCons $dis
       inputs = $(listE $ map (mkAlt [| cons |]) alts)
   in Mux (Slice tagRange $dis) inputs
 |]
  where mkAlt cs (n,[],body) = body -- no pattern vars means no lookup/let binding
        mkAlt cs (n,pvars,body) =
          [| let Just slices = lookup $(litE (stringL n))  $cs
             in  $(mkPats dis [| slices |] pvars body)
           |]

mkPats dis slices pvars body
  = letE [valD (varP (mkName v)) (normalB [| Slice $dis ($slices !! s) |]) []
          |  v <- pvars
          |  s <- [0..]]
       body





data Ent = Mux Ent [Ent]
         | Slice (Int,Int) Ent
         | Lit Int
         | Pad String


instance Show Ent where
 show (Mux sel args) = "mux(s=> " ++ show sel ++ "," ++  (concat $ intersperse "," (map show args)) ++ ")"
 show (Slice (low,high) val) = show val ++  "(" ++ show high ++ " downto " ++ show low ++ ")"
 show (Pad p) = p
 show (Lit x) = show x



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


instance CType Ent where
  getTag _ = getTag (undefined :: (Maybe Int))
  getCons _ = getCons (undefined :: (Maybe Int))




test = (lcase dise alts)
  where dis :: Maybe Int
        dis = Just 0
        dise = [| dis |]
        alts = [("Nothing",[],dise),
                ("Just", ["v"],dise)]


