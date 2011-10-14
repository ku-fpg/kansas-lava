-- | This module is for internal functions used in more than one place
module Language.KansasLava.Internal where

takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe = maybe id take
