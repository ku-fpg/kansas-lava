{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Language.KansasLava.Testing.Unit (debug, test) where

import Language.KansasLava hiding (head)
import Language.KansasLava.Testing.Thunk
import Language.KansasLava.Testing.Trace

import Data.Default()
import qualified Data.Map as M


debug :: (Ports b) => String -> Int -> Thunk b -> IO ()
debug name cycles thunk = do
    trace <- mkTrace (return cycles) thunk
    putStrLn $ "Debug output for " ++ name
    print trace

-- TODO: Remove restriction to Seq b that we have on Thunks
test :: (Rep b) => String -> Int -> Thunk (Seq b) -> Seq b -> IO ()
test name c t e = test' c t e (defAction name)

test' :: forall b. (Rep b) => Int -> Thunk (Seq b) -> Seq b -> ((Bool, Trace) -> IO ()) -> IO ()
test' cycles thunk (Seq s _) action = do
    trace <- mkTrace (return cycles) thunk

    let e = setCycles cycles $ trace { outputs = M.adjust (\_ -> toTrace s) k outs }
        k = head $ M.keys outs
        outs = outputs trace

    action (trace == e, trace)

defAction :: String -> (Bool, Trace) -> IO ()
defAction name (True,_) = putStrLn $ name ++ " passed."
defAction name (False,trace) = do
    putStrLn $ name ++ " failed!"
    putStrLn "Actual trace results:"
    print trace
{-
data Options = Options { cycles :: Maybe Int
                       , forceWrite :: Bool
                       , ignoreFile :: Bool
                       , testDir :: FilePath
                       , runTests :: [String]
--                       , testAction :: Bool -> String -> Trace -> IO ()
                       }

instance Default Options where
    def = Options (return 100) False False "tests/" [] -- defAction

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
-}
