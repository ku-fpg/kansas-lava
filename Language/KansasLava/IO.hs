{-# LANGUAGE FlexibleInstances #-}

module Language.KansasLava.IO where

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Seq

class INPUT i where
        input :: ([Var] -> i, [Var])         -- based on var names

class OUTPUT o where
        output :: o -> ([Var],[(Var,Driver E)])   

instance (INPUT i,OUTPUT o) => OUTPUT (i -> o) where
         -- output :: (i -> o) -> [(String,Driver E)]
         output f = (theseVars' ++ o_iVars,o_oVars) 
            where (i,theseVars) = input
                  (o_iVars,o_oVars) = output (f $ i theseVars')
                  theseVars' = map mkUniq theseVars
                  mkUniq v | v `elem` o_iVars || v `elem` (map fst o_oVars)
                                       = mkUniq (case v of Var v' -> Var $ v' ++ "0")
                           | otherwise = v

instance OUTPUT (Signal a,Signal b) where
        output (~(Signal _ c),~(Signal _ d)) = ([],[(Var "c",c),(Var "d",d)])

instance OUTPUT (Signal a) where
        output (~(Signal _ a)) = ([],[(Var "o0",a)])

instance INPUT (Signal a) where
        input = (  \ (a:_) ->
                (
                         (Signal undefinedSeq $ Pad $  a)
                )
                , [Var "i0"]
                )

instance INPUT (Signal a,Signal b) where
        input = (  \ (a:b:_) ->
                (
                         (Signal undefinedSeq $ Pad $  a)
                ,        (Signal undefinedSeq $ Pad $  b)
                )
                , [Var "a",Var "b"]
                )
instance INPUT (Signal a,Signal b,Signal c) where
        input = (  \ (a:b:c:_) ->
                (
                         (Signal undefinedSeq $ Pad $  a)
                ,        (Signal undefinedSeq $ Pad $  b)
                ,        (Signal undefinedSeq $ Pad $  c)
                )
                , [Var "a",Var "b",Var "c"]
                )
