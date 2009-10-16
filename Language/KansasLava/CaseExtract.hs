{-# LANGUAGE TemplateHaskell, ParallelListComp, UndecidableInstances #-}
module Language.KansasLava.CaseExtract(
  lava,
  Ent(..),Signal(..),pad,lit
  ) where

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
    lcase (return e) alts

convertExpr e = return e

lcase :: ExpQ -> [Match] -> ExpQ
lcase dis alts = [|
   let tagWidth = tagSize $dis
       tagRange = (tagWidth - 1, 0)
       cons = getCons $dis
       inputs = $(listE $ map (mkAlt' [| cons |]) alts)
   in mux (slice tagRange $dis) inputs
 |]
  where -- no pattern vars means no lookup/let binding
        mkAlt' :: ExpQ -> Match -> ExpQ
        mkAlt' cs (Match (ConP n []) (NormalB body) []) =
          return body
        mkAlt' cs (Match (ConP n pvars) (NormalB body) []) =
          [| let Just slices = lookup $(litE (stringL (nameBase n)))  $cs
             in  $(mkPats' dis [| slices |] pvars body)
           |]
        mkAlt' cs m = error $ "mkAlt: can't convert  " ++ show m

mkPats' :: ExpQ -> ExpQ -> [Pat] -> Exp -> ExpQ
mkPats' dis slices pvars body
  = letE [valD (return v) (normalB [| slice  ($slices !! s) $dis |]) []
          |  v <- pvars
          |  s <- [0..]]
       (return body)




data Ent = Mux Ent [Ent]
         | Slice (Int,Int) Ent
         | Lit Int
         | Pad String


instance Show Ent where
 show (Mux sel args) = "mux(s=> " ++ show sel ++ "," ++  (concat $ intersperse "," (map show args)) ++ ")"
 show (Slice (high,low) val) = show val ++  "(" ++ show high ++ " downto " ++ show low ++ ")"
 show (Pad p) = p
 show (Lit x) = show x

data Signal a = Sig Ent

slice ran (Sig m) = Sig (Slice ran m)
mux :: Signal a -> [Signal b] -> Signal b
mux (Sig sel) args = Sig $ Mux sel [a | Sig a <- args]
pad = Sig . Pad

lit :: Int -> Signal Int
lit = Sig . Lit




instance Show (Signal a) where
  show (Sig x) = show x

instance CType a => CType (Signal a) where
  tagSize _ = tagSize (undefined :: a)
  getCons _ = getCons (undefined :: a)


type ConsMap = [(String,[(Int,Int)])]
class CType a where
  tagSize :: a -> Int
  getCons :: a -> ConsMap

instance Sized a => CType (Maybe a) where
 tagSize _ = 1

 getCons _ = [("Nothing",[]),
              ("Just", [(size (undefined :: a), 1)])]

instance (Sized a, Sized b) => CType (Either a b) where
  tagSize _ = 1
  getCons _ = [("Left",[(size (undefined :: a), 1)]),
               ("Right",[(size (undefined :: b), 1)])]


class Sized a where
 size :: a -> Int

instance Sized Bool where
 size _ = 1

instance Sized Int where
  size _ = 32

instance Sized Char where  size _ = 8

instance (CType (f a), Sized a) => Sized (f a) where
  size x = tagSize x + size  (undefined :: a)

instance (CType (f a b), Sized a, Sized b) => Sized (f a b) where
  size x = tagSize x + (sa `max` sb)
   where sa = size (undefined :: a)
         sb = size (undefined :: b)


