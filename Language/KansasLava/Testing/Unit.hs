module Language.KansasLava.Testing.Unit where

import Language.KansasLava
import Language.KansasLava.Testing.Probes

import Control.Applicative

import Data.Default

import System.Directory
import System.FilePath.Posix

data Options = Options { cycles :: Maybe Int
                       , forceWrite :: Bool
                       , ignoreFile :: Bool
                       , testDir :: FilePath
                       , runTests :: [String]
                       , testAction :: Bool -> String -> Trace -> IO ()
                       }

instance Default Options where
    def = Options (Just 100) False False "tests/" [] defAction

defAction :: Bool -> String -> Trace -> IO ()
defAction True name _ = putStrLn $ name ++ " passed."
defAction False name _ = putStrLn $ name ++ " failed!"

{-
forceAction :: Bool -> String -> Trace -> IO ()
forceAction True = defAction True
forceAction False = do
    putStrLn $ "Writing execution result to: " ++ tFile
    writeFile tFile $ serialize result
    defAction
-}

testGroup _ = sequence_
{-
testGroup name tests = do
    results <- sequence tests
    if and results
        putStrLn $ "All tests in
-}

unitTest :: (Ports a) => Options -> String -> Thunk a -> IO ()
unitTest opts name thunk@(Thunk circuit fn) = do
    let path = testDir opts
        tFile = path </> name <.> "test"
        ignore = ignoreFile opts

    createDirectoryIfMissing True path
    exists <- doesFileExist tFile

    trace <- if exists && (not ignore)
        then deserialize <$> readFile tFile
        else mkTrace (cycles opts) thunk

    let (equal, result) = test circuit trace
    (testAction opts) equal name result

    if ignore || (forceWrite opts)
        then do
            putStrLn $ "Writing execution result to: " ++ tFile
            writeFile tFile $ serialize result
        else return ()

