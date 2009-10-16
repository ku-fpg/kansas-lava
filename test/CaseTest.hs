{-# LANGUAGE QuasiQuotes, TemplateHaskell,ScopedTypeVariables	 #-}
module CaseTest where

import Language.KansasLava.CaseExtract


j x = [$lava|Just x|]
n = [$lava|Nothing|]
test x =  [$lava|
             case x of
               Just v -> v
               Nothing -> pad "nothing"
           |]
  where y :: Int
        y = 1

test1 x =  [$lava|
             case x of
               Left v -> pad "integer"
               Right v -> pad "char"
           |]
  where y :: Int
        y = 1

simple :: Signal (Maybe Int)
simple = Sig (Pad "simple")

another :: Signal (Either Int Char)
another = pad "another"



data Instr = Add Int Int
           | Sub Int Int

instance CType Instr where
  getTagWidth _ = 1
  getCons _ = [("Add",[(64,33),(32,1)]),
               ("Sub",[(64,33),(32,1)])]
  getTag (Add _ _) = 0
  getTag (Sub _ _) = 1


cpu :: Signal Instr -> Signal Int
cpu istrm = [$lava|
             case istrm of
              Add x y -> x + y
              Sub x y -> x - y
            |]


triple istrm = op "triple" [cpu istrm,cpu istrm,cpu istrm]
