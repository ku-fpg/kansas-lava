{-# LANGUAGE DeriveDataTypeable #-}
import Development.Shake as Shake

import Test
import qualified Data.Map as M
import qualified Matrix
import qualified Memory
import qualified Coerce
import qualified Others
import qualified Protocols
import qualified Regression

import Data.Char
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
import Data.Default
import System.Exit
import System.Console.CmdArgs hiding (Default,def,name,summary,opt,Quiet, Loud)

data TestOpt = TestOpt
        { cores         :: Int
        , sims          :: Int
        , testArgs      :: [String]
        , hpcHack       :: Bool
        } deriving (Show,Data,Typeable)

instance Default TestOpt where
        def = TestOpt
                { cores       = 1      &= help "number of cores to use"
                , sims        = 1      &= help "number of simulator licenses to use"
                , hpcHack     = False  &= help "appends '-rec' to recusive calls, allowing hpc to be used"
                , testArgs    = []     &= args
                } &= details ["usage: kansas-lava-test <what> [to-whom] where what = " ++ show (map fst whatDB)]
                  &= program "kansas-lava-test"

data What
        = ShowTests
        | Clean
        | TestShallow
        | GenerateVHDL
        | ExecuteVHDL
        | VerifyVHDL
        | TestAll
        deriving (Eq,Ord,Enum,Data,Typeable)

instance Show What where
        show str = case lookup str $ map (\(a,b) -> (b,a)) whatDB of
                    Nothing -> "??"
                    Just v -> v

whatDB :: [(String,What)]
whatDB =
        [ ("show-tests",    ShowTests)
        , ("clean",         Clean)
        , ("test-shallow", TestShallow)
        , ("generate-vhdl",  GenerateVHDL)
        , ("execute-vhdl",  ExecuteVHDL)
        , ("verify-vhdl",   VerifyVHDL)
        , ("test-all",      TestAll)
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
        opt <- cmdArgs (def :: TestOpt)
        main2 opt
              (testArgs opt)

--main2 opts (('-':n):rest) | all isDigit n && length n > 0 = main2 (opts { shakeOpts = (shakeOpts opts) { shakeThreads = read n }  }) rest
main2 opts [doWhat] = do
                  w <- what doWhat
                  let db = allTests
                  doAllBuild opts w db (M.keys db)
main2 opts [doWhat,toWhom] = do
                  w <- what doWhat
                  let db = allTests
                  case M.lookup toWhom db of
                    Nothing -> do putStrLn $ "can not find test: " ++ show toWhom
                                  usage
                    Just st -> case w of
                                 ShowTests    -> putStrLn $ show st ++ " is a valid test"
                                 TestShallow -> runShallowTest st
                                 GenerateVHDL  -> runVHDLGeneratorTest st
                                 ExecuteVHDL  -> doAllBuild opts w db [toWhom]
                                 VerifyVHDL   -> doAllBuild opts w db [toWhom]
main2 _ _ = usage

allTests :: M.Map String SingleTest
allTests = M.fromList [ (str,s) | s@(SingleTest str _ _ _) <- f []
                                      , not ("max" `isPrefixOf` str)
                                      , not ("abs" `isPrefixOf` str)
                                      , not ("min" `isPrefixOf` str)
                                      , not ("signum" `isPrefixOf` str)
--                                 , ("matrix" `isPrefixOf` str)
                     ]
    where (Tests _ f) = do
                    Matrix.tests
                    Memory.tests
                    Coerce.tests
                    Others.tests
                    Protocols.tests
                    Regression.tests

doAllBuild _ ShowTests db _ = do
        putStrLn $ show (M.keys db)
doAllBuild _ Clean db _ = do
        rawSystem "rm" ["-Rf","sims"]
        return ()
doAllBuild opts w db to_test = do
        let targets =
                [ "sims" </> t </> case w of
                                     TestShallow -> "dut.in.tbf"
                                     GenerateVHDL  -> "dut.vhd"
                                     ExecuteVHDL  -> "dut.out.tbf"
                                     VerifyVHDL   -> "dut.result"
                | t <- to_test
                , w /= TestAll
                ] ++ [ "kansas-lava.report"
                     | TestAll <- [w]
                     ]

        me <- getExecutablePath
        let me2 = if hpcHack opts
                  then me ++ "-2"
                  else me

        vsim <- newResource "vsim" (sims opts)

        shake (shakeOptions
                            { shakeReport = return "report.html"
                            , shakeThreads = cores opts
                            , shakeVerbosity = Loud -- Diagnostic -- Quiet
--                            , shakeProgress = progressDisplay 1 (\ str -> putStr ("\r" ++ init str) >> hFlush stdout)
                            }) $ do
                want targets

                "*//dut.in.tbf" *> \ out -> do
                        let tst = dropDirectory1 $ takeDirectory out
                        liftIO $ do
                          e <- rawSystem me2 [show TestShallow,tst]
                          case e of
                            ExitSuccess -> return ()
--                            _ | runAllTests opts -> writeFile out ""
                            _ -> return ()
                "*//dut.vhd" *> \ out -> do
                        let tst = dropDirectory1 $ takeDirectory out
                        liftIO $ do
                          e <- rawSystem me2 [show GenerateVHDL,tst]
                          case e of
                            ExitSuccess -> return ()
--                            _ | runAllTests opts -> writeFile out ""
                            _ -> return ()

                "*//dut.out.tbf" *> \ out -> do
                        let tst = dropDirectory1 $ takeDirectory out
                        need [ "sims" </> tst </> "dut.vhd"
                             , "sims" </> tst </> "dut.in.tbf"
                             ]
                        putLoud $ "vsim for " ++ tst
                        withResource vsim 1 $ traced "vsim" $ liftIO $ do
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
                                  ExitSuccess -> return ()
--                                  _ | runAllTests opts
--                                        -> writeFile ("sims" </> tst </> "dut.out.tbf") ""
                                  _     -> ioError (userError $ "failed to run vsim")

                "*//dut.result" *> \ out -> do
                        let tst = dropDirectory1 $ takeDirectory out
                        need [ "sims" </> tst </> "dut.in.tbf"
                             , "sims" </> tst </> "dut.out.tbf"
                             ]
                        shallow <- readFile' $ "sims" </> tst </> "dut.in.tbf"
                        deep    <- readFile' $ "sims" </> tst </> "dut.out.tbf"
                        writeFile' ("sims" </> tst </> "dut.result")
                                $  compareLines 1 (lines shallow) (lines deep)

                "kansas-lava.report" *> \ out -> do
                        let files = [ "sims" </> t </> "dut.result"
                                    | t <- to_test
                                    ]
                        need files
                        xs <- liftIO
                            $ sequence [ do txt <- readFile file
                                            if "success:" `isPrefixOf` txt
                                              then return []
                                              else return (file ++ ":\n" ++ unlines (map ("  " ++) $ lines txt))
                                       | file <- files
                                       ]
                        writeFile' out $ concat xs
                                ++ "[" ++ show (length files) ++ " tests checked]\n"
