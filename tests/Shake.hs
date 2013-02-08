import Development.Shake

import Test
import qualified Data.Map as M
import qualified Matrix
import qualified Memory
import qualified Coerce
import qualified Others
import qualified Protocols
import qualified Regression


import Data.List
import Language.KansasLava.Fabric
import System.Directory
import System.Process
import System.Environment
--import System.Environment.Executable
import Development.Shake.FilePath

import Control.Concurrent
import System.Exit

tests seq = do
        Matrix.tests seq
        Matrix.tests seq
        Memory.tests seq
        Coerce.tests seq
        Others.tests seq
	Protocols.tests seq
	Regression.tests seq


data What
        = ShowTests
        | Clean
        | BuildShallow
        | CompileVHDL
        deriving (Eq,Ord)

instance Show What where
        show str = case lookup str $ map (\(a,b) -> (b,a)) whatDB of
                    Nothing -> "??"
                    Just v -> v

whatDB :: [(String,What)]
whatDB =
        [ ("show-tests",    ShowTests)
        , ("clean",         Clean)
        , ("build-shallow", BuildShallow)
        , ("compile-vhdl",  CompileVHDL)
        ]

what :: String -> IO What
what str = case lookup str whatDB of
             Just v  -> return v
             Nothing -> do
                putStrLn $ "can not find operation for " ++ show str
                usage

usage :: IO a
usage = do
        putStrLn $ "usage: kansas-lava-test <what> [to-whom] where what = " ++ show (map fst whatDB)
        exitFailure

main = do
        args <- getArgs
        case args of
          [doWhat]        -> do
                  w <- what doWhat
                  db <- allTests
                  doAllBuild w db
          [doWhat,toWhom] -> do
                  w <- what doWhat
                  db <- allTests
                  case M.lookup toWhom db of
                    Nothing -> do putStrLn $ "can not find test: " ++ show toWhom
                                  usage
                    Just st -> doSpecificBuild w st
          _              -> usage

allTests :: IO (M.Map String SingleTest)
allTests = do
        var <- newEmptyMVar
        let sq = TestSeq (\ nm count fab tb_fab -> putMVar var $ Just $ SingleTest nm count fab tb_fab) ()

        forkIO $ do
                tests sq
                putMVar var Nothing

        let loop xs = do
                v <- takeMVar var
                case v of
                  Nothing -> return xs
                  Just x  -> loop (x : xs)
        xs <- loop []

        -- End of hack

        return $ M.fromList [ (str,s) | s@(SingleTest str _ _ _) <- reverse xs
                                      , not ("max" `isPrefixOf` str)
                                      , not ("abs" `isPrefixOf` str)
                                      , not ("min" `isPrefixOf` str)
                                      , not ("signum" `isPrefixOf` str)
--                                      , ("matrix" `isPrefixOf` str)
                            ]

doSpecificBuild ShowTests st = do
        putStrLn $ show st ++ " is a valid test"


doSpecificBuild BuildShallow st = do
        runShallowTest st
doSpecificBuild w st = do
        print ("SpecificBuild",w,st)
        return ()

doAllBuild ShowTests db = do
        putStrLn $ show (M.keys db)
doAllBuild Clean db = do
        rawSystem "rm" ["-Rf","sims"]
        return ()
doAllBuild w db = do
        let targets = take 1 $
                [ "sims" </> t </> case w of
                                     BuildShallow -> "dut.in.tbf"
                                     CompileVHDL  -> "dut.vhd"
                | t <- M.keys db
                ]

        print targets
        me <- getExecutablePath

        shake shakeOptions { shakeReport = Nothing -- return "report.html"
                           , shakeThreads = 1
                           } $ do
                want targets

                "*//dut.in.tbf" *> \ out -> do
                        let tst = dropDirectory1 $ takeDirectory out
                        system' me [show w,tst]
{-
                "*//dut.vhd" *> \ out -> do
                        -- Generate the vhd
                        let tst = dropDirectory1 $ takeDirectory out
                        case M.lookup tst db of
                          Nothing -> error $ "can not find " ++ tst
                          Just st -> liftIO $ runVHDLGeneratorTest st
-}

--        doSpecificBuild w db (M.keys db)
{-
        -- This decides what we need to find

        print $ targets

        shake shakeOptions { shakeReport = Nothing -- return "report.html"
                           , shakeThreads = 1
                           } $ do
                want targets

                "*//dut.in.tbf" *> \ out -> do
                        -- The shallow
                        let tst = dropDirectory1 $ takeDirectory out
                        case M.lookup tst db of
                          Nothing -> error $ "can not find " ++ tst
                          Just st -> liftIO $ runShallowTest st

                "*//dut.vhd" *> \ out -> do
                        -- Generate the vhd
                        let tst = dropDirectory1 $ takeDirectory out
                        case M.lookup tst db of
                          Nothing -> error $ "can not find " ++ tst
                          Just st -> liftIO $ runVHDLGeneratorTest st



{-
 Test.TestSeq (String
                  -> Int
                  -> Language.KansasLava.Fabric.Fabric ()
                  -> Language.KansasLava.Fabric.Fabric (Int -> Maybe String)
                  -> IO ())
                 ()
-}

-}
