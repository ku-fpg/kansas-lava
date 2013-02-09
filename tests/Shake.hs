import Development.Shake

import Test
import qualified Data.Map as M
import qualified Matrix
import qualified Memory
import qualified Coerce
import qualified Others
import qualified Protocols
import qualified Regression

import System.IO
import System.IO.Error
import qualified Control.Exception as C
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
        | ExecuteVHDL
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
        , ("execute-vhdl",  ExecuteVHDL)
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
                  doAllBuild w db (M.keys db)
          [doWhat,toWhom] -> do
                  w <- what doWhat
                  db <- allTests
                  case M.lookup toWhom db of
                    Nothing -> do putStrLn $ "can not find test: " ++ show toWhom
                                  usage
                    Just st -> case w of
                                 ShowTests    -> putStrLn $ show st ++ " is a valid test"
                                 BuildShallow -> runShallowTest st
                                 CompileVHDL  -> runVHDLGeneratorTest st
                                 ExecuteVHDL  -> doAllBuild w db [toWhom]
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
doAllBuild ShowTests db _ = do
        putStrLn $ show (M.keys db)
doAllBuild Clean db _ = do
        rawSystem "rm" ["-Rf","sims"]
        return ()
doAllBuild w db to_test = do
        let targets =
                [ "sims" </> t </> case w of
                                     BuildShallow -> "dut.in.tbf"
                                     CompileVHDL  -> "dut.vhd"
                                     ExecuteVHDL  -> "dut.out.tbf"
                | t <- to_test
                ]

        print targets
        me <- getExecutablePath

        vsim <- newResource "vsim" 48

        shake shakeOptions { shakeReport = return "report.html"
                           , shakeThreads = 4
                           } $ do
                want targets

                "*//dut.in.tbf" *> \ out -> do
                        let tst = dropDirectory1 $ takeDirectory out
                        system' me [show BuildShallow,tst]
                "*//dut.vhd" *> \ out -> do
                        let tst = dropDirectory1 $ takeDirectory out
                        system' me [show CompileVHDL,tst]

                "*//dut.out.tbf" *> \ out -> do
                        let tst = dropDirectory1 $ takeDirectory out
                        need [ "sims" </> tst </> "dut.vhd"
                             , "sims" </> tst </> "dut.in.tbf"
                             ]
                        withResource vsim 1 $ liftIO $ do
                                (_,Just outh,_,pid) <- createProcess $
                                        (proc "vsim" -- vsim
                                              ["-c","-do","dut.do"])
                                        { cwd = return $ "sims" </> tst
                                        , std_in = Inherit
                                        , std_out = CreatePipe
                                        , std_err = Inherit
                                        }
                                -- now write and flush any input
                                output  <- hGetContents outh
                                outMVar <- newEmptyMVar
                                _ <- forkIO $ C.evaluate (length output) >> putMVar outMVar ()
                                -- wait on the output
                                takeMVar outMVar
                                hClose outh

                                ex <- waitForProcess pid

                                case ex of
                                  ExitSuccess   -> return ()
                                  ExitFailure r -> ioError (userError $ "failed to run vsim")


