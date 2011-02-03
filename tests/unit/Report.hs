{-# LANGUAGE ScopedTypeVariables #-}
module Report where

import Language.KansasLava.Internals
import Language.KansasLava.Testing.Trace

import Control.Applicative
import Control.Monad
import qualified Control.Exception as E
import System.Directory
import System.Environment
import System.FilePath
import qualified System.IO.Strict as Strict

import Types
import Utils

data Report = Report Summary [TestCase]

instance Show Report where
    show (Report s _) = show s

data Summary = Summary { sfail :: Int
                       , spass :: Int
                       , generated :: Int
                       , codegenfail :: Int
                       , vhdlfail :: Int
                       , simfail :: Int
                       , compfail :: Int
                       , passed :: Int
                       , total :: Int
                       }


instance Show Summary where
    show summary = unlines [tt,sf,sp,rf,gn,vf,cp,si,ps]
        where tt = "Total tests: " ++ show (total summary)
              sf = "Shallow test failures: " ++ show (sfail summary)
              sp = "Shallow tests passed: "
                   ++ case spass summary of
                        0 -> show $ sum [ fn summary
                                        | fn <- [generated, codegenfail, vhdlfail, simfail, compfail, passed]
                                        ]
                        x -> show x
              rf = "VHDL generation failures: " ++ show (codegenfail summary)
              gn = "Simulations generated: "
                   ++ case generated summary of
                        0 -> show $ sum [ fn summary
                                        | fn <- [vhdlfail, simfail, compfail, passed]
                                        ]
                        x -> show x
              vf = "VHDL compilation failures: " ++ show (vhdlfail summary)
              cp = "Simulation failures (non-matching traces): " ++ show (compfail summary)
              si = "Simulation failures (other): " ++ show (simfail summary)
              ps = "Simulation tests passed: " ++ show (passed summary)

main = do
    args <- getArgs
    if length args < 1
        then do pname <- getProgName
                putStrLn "Need path to simulation directory."
                putStrLn $ "USAGE: " ++ pname ++ " path"
                putStrLn $ "Example: " ++ pname ++ " sims"
        else generateReport $ args !! 0

generateReport :: FilePath -> IO ()
generateReport path = do
    postSimulation path
    rs <- buildResults path
    putStrLn $ "rs: " ++ show (length rs)
    let r = buildReport rs

    putStrLn $ show r

    html <- reportToHtml r
    writeFile "report.html" html
    shtml <- reportToSummaryHtml r
    writeFile "summary.html" shtml

-- Traverses all the generated simulation directories and reads the result files.
buildResults :: FilePath -> IO [TestCase]
buildResults spath = go "" spath
    where go :: String -> FilePath -> IO [TestCase]
          go name path = do
            resE <- doesFileExist $ path </> "result"
            res <- if resE
                    then liftM (\r -> [(name,r)]) (read <$> (Strict.readFile $ path </> "result"))
                    else return []

            contents <- getDirectoryContents path
            subdirs <- filterM (\(_,f) -> doesDirectoryExist f)
                               [ (name </> f, path </> f)
                               | f <- contents
                               , f /= "."
                               , f /= ".." ]

            subresults <- concat <$> (mapM (uncurry go) subdirs)

            return $ res ++ subresults

addtoSummary :: Result -> Summary -> Summary
addtoSummary (ShallowFail _ _) s = s { sfail = 1 + (sfail s) }
addtoSummary ShallowPass       s = s { spass = 1 + (spass s) }
addtoSummary SimGenerated      s = s { generated = 1 + (generated s) }
addtoSummary (CodeGenFail _)   s = s { codegenfail = 1 + (codegenfail s) }
addtoSummary (CompileFail _)   s = s { vhdlfail = 1 + (vhdlfail s) }
addtoSummary (SimFail _)       s = s { simfail = 1 + (simfail s) }
addtoSummary (CompareFail _ _ _) s = s { compfail = 1 + (compfail s) }
addtoSummary (Pass _ _ _)        s = s { passed = 1 + (passed s) }

buildReport :: [TestCase] -> Report
buildReport rs = Report summary rs
    where rs' = map snd rs
          summary = foldr addtoSummary (Summary 0 0 0 0 0 0 0 0 (length rs')) rs'

reportToSummaryHtml :: Report -> IO String
reportToSummaryHtml (Report summary _) = do
    header <- Strict.readFile "header.inc"
    mid <- Strict.readFile "mid.inc"
    footer <- Strict.readFile "footer.inc"

    return $ header ++ (summaryToHtml summary) ++ mid ++ footer

summaryToHtml :: Summary -> String
summaryToHtml s = unlines [ "<table>"
                          , "<tr class=\"huge " ++ sclass ++ "\"><td>Shallow Failures:</td><td>" ++ show (sfail s) ++ "</td><td>(" ++ show (total s - sfail s) ++ "/" ++ show (total s) ++ " passed)</td></tr>"
                          , "<tr class=\"huge " ++ dclass ++ "\"><td>Simulation Failures:</td><td>" ++ show (sum [codegenfail s, vhdlfail s, compfail s, simfail s]) ++
                              "</td><td>(" ++ show (passed s) ++ "/" ++ show (total s - sfail s) ++ " passed)</td></tr>"
                          , "</table>"
                          , "<hr width=\"90%\">"
                          , "<table>"
                          , "<tr id=\"cgf\" class=\"kindahuge\"><td>VHDL Generation Failures:</td><td>" ++ show (codegenfail s) ++ "</td></tr>"
                          , "<tr id=\"vcf\" class=\"kindahuge\"><td>VHDL Compilation Failures:</td><td>" ++ show (vhdlfail s) ++ "</td></tr>"
                          , "<tr id=\"cpf\" class=\"kindahuge\"><td>Comparison Failures:</td><td>" ++ show (compfail s) ++ "</td></tr>"
                          , "<tr id=\"osf\" class=\"kindahuge\"><td>Other Simulation Failures:</td><td>" ++ show (simfail s) ++ "</td></tr>"
                          , "</table>"
                          ]
    where chooser x = case x of
                        0 -> "allpass"
                        i | i == total s -> "allfail"
                        _ -> "somepass"
          sclass = chooser $ sfail s
          dclass = chooser $ total s - sfail s - passed s


reportToHtml :: Report -> IO String
reportToHtml (Report summary results) = do
    header <- Strict.readFile "header.inc"
    mid <- Strict.readFile "mid.inc"
    footer <- Strict.readFile "footer.inc"

    let showall = "<a href=\"#\" id=\"showall\">Show All</a>"
        res = unlines [ concat ["<div id=\"", name, "\" class=\"header ", sc, "\">", name
                               ,"<span class=\"status\">", s, "</span></div>\n<div class=\"additional\">"
                               , a, "</div>"]
                      | (name, r) <- results
                      , let (sc, s, a) = case r of
                                           ShallowFail t ts -> ("shallowfail", "Shallow Failed", unDiv [show t, show ts])
                                           ShallowPass -> ("shallowpass", "Shallow Passed", unDiv [""])
                                           SimGenerated -> ("simgenerated", "Simulation Generated", unDiv [""])
                                           CodeGenFail s -> ("codegenfail", "VHDL Generation Failed", unDiv [s])
                                           CompileFail s -> ("compilefail", "VHDL Compilation Failed", unDiv [s])
                                           SimFail s -> ("simfail", "Simulation Failed (other)", unDiv [s])
                                           CompareFail t1 t2 s -> ("comparefail", "Failed", unDiv [show t1, show t2, s])
                                           Pass t1 t2 s -> ("pass", "Passed", unDiv [show t1, show t2, s])
                      ]
    return $ header ++ (summaryToHtml summary) ++ mid ++ showall ++ res ++ footer

unDiv :: [String] -> String
unDiv = foldr (\s tail -> "<div>" ++ sliceString 200 80 s ++ "</div>" ++ tail) ""

sliceString :: Int -> Int -> String -> String
sliceString r c str = unlines $ take r $ chunk str
    where chunk [] = []
          chunk s  = let (c1,r) = splitAt c s in c1 : chunk r
