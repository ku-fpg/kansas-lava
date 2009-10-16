{-# LANGUAGE TemplateHaskell, ParallelListComp, UndecidableInstances #-}
module Language.KansasLava.CaseExtract(
  lava,
  Ent(..),Signal(..),pad,lit,cons,op,
  CType(..), Sized(..)

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

convertExpr (ConE n) = lcons n []
convertExpr a@(AppE e1 e2) =
    case simpApp a [] of
      Just (n, args) -> lcons n (map return args)
      _ ->  return a
  where simpApp (AppE (ConE n) a) as = Just (n,a:as)
        simpApp (AppE e a) as = simpApp e (a:as)
        simpApp _ _ = Nothing

convertExpr e = return e

lcase :: ExpQ -> [Match] -> ExpQ
lcase dis alts = [|
   let tagWidth = getTagWidth $dis
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


lcons :: Name -> [ExpQ] -> ExpQ
lcons n args = do
  let start = appE (varE (mkName "cons")) (genConsValue n)
  appE start (listE args)


data Ent = Mux Ent [Ent]
         | Slice (Int,Int) Ent
         | Lit Int
         | Cons (Int,Int) [Ent]
         | Pad String
         | Op String [Ent] deriving (Eq)


instance Num (Signal a) where
 fromInteger x = Sig (Lit (fromInteger x))
 (Sig a) + (Sig b) = Sig (Op "+" [a,b])
 (Sig a) - (Sig b) = Sig (Op "-" [a,b])

instance Show Ent where
 show (Mux sel args) = "mux(s=> " ++ show sel ++ "," ++  (concat $ intersperse "," (map show args)) ++ ")"
 show (Slice (high,low) val) = show val ++  "(" ++ show high ++ " downto " ++ show low ++ ")"
 show (Pad p) = p
 show (Lit x) = show x
 show (Cons name ents) = show name ++ " " ++ (concat $ intersperse " " $ map show ents)
 show (Op op [a,b]) = show a ++ " " ++ op ++ " " ++ show b
 show (Op op as) = show op ++ " " ++ (concat $ intersperse " " $ map show as)

data Signal a = Sig Ent deriving Eq

slice ran (Sig m) = Sig (Slice ran m)
mux :: Signal a -> [Signal b] -> Signal b
mux (Sig sel) args = Sig $ Mux sel [a | Sig a <- args]
pad = Sig . Pad
cons n as = Sig (Cons n [a | Sig a <- as])
op n as = Sig (Op n [a | Sig a <- as])



lit :: Int -> Signal Int
lit = Sig . Lit




instance Show (Signal a) where
  show (Sig x) = show x

instance CType a => CType (Signal a) where
  getTagWidth _ = getTagWidth (undefined :: a)
  getCons _ = getCons (undefined :: a)
  getTag (Sig n) = error "GetTag on signal"


type ConsMap = [(String,[(Int,Int)])]
class CType a where
  getTagWidth :: a -> Int
  getCons :: a -> ConsMap
  getTag :: a -> Int

instance Sized a => CType (Maybe a) where
 getTagWidth _ = 1

 getCons _ = [("Nothing",[]),
              ("Just", [(size (undefined :: a), 1)])]
 getTag (Just _) = 1
 getTag Nothing = 0

instance (Sized a, Sized b) => CType (Either a b) where
  getTagWidth _ = 1
  getCons _ = [("Left",[(size (undefined :: a), 1)]),
               ("Right",[(size (undefined :: b), 1)])]

  getTag (Left _) = 0
  getTag (Right _) = 1

class Sized a where
 size :: a -> Int

instance Sized Bool where
 size _ = 1

instance Sized Int where
  size _ = 32

instance Sized Char where  size _ = 8

instance Sized () where
  size _ = error "Size on unit is crazy."

instance (CType (f a), Sized a) => Sized (f a) where
  size x = getTagWidth x + size  (undefined :: a)

instance (CType (f a b), Sized a, Sized b) => Sized (f a b) where
  size x = getTagWidth x + (sa `max` sb)
   where sa = size (undefined :: a)
         sb = size (undefined :: b)


genConsValue ::Name -> ExpQ
genConsValue nm = do
  (DataConI _ dty _ _) <- reify nm
  val <- newName "val"
  let (value,ty) = genValue (ConE nm) dty
  return $ LetE [SigD val ty, ValD (VarP val) (NormalB value) []]
           (TupE [(AppE (VarE (mkName "getTag")) (VarE val)),
                  (AppE (VarE (mkName "getTagWidth")) (VarE val))])


-- genValue :: Type -> Exp
genValue acc (ForallT _ _ ty) = genValue acc ty
genValue acc ty = case split ty of
                    (ArrowT, [VarT _,arg2]) -> genValue (AppE acc unit) arg2
                    (ArrowT, [_,arg2]) -> genValue (AppE acc undef) arg2
                    (ConT nm, args) -> (acc,foldl AppT (ConT nm)  (map monoType args))

  where unit = ConE $ (mkName "()") -- tupleDataName 0 -- removed because of GHC.Tuple.() warning. This seems to work.
        undef = VarE $ mkName "undefined"
        unitType = TupleT 0
        monoType (VarT _) = unitType
        monoType t =  t


-- This was taken directly from the TH pretty print module.
split :: Type -> (Type, [Type])    -- Split into function and args
split t = go t []
    where go (AppT t1 t2) args = go t1 (t2:args)
          go ty           args = (ty, args)


