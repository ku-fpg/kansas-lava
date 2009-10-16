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



