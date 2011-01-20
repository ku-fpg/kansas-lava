{-# LANGUAGE ScopedTypeVariables #-}
module Report where

import Language.KansasLava.Internals
import Language.KansasLava.Testing.Trace

import Control.Applicative
import qualified Control.Exception as E
import System.Directory
import System.FilePath

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

type TestCase = (String, Result)

data Result = ShallowFail Trace TraceStream  -- Shallow result doesn't match expected
            | ShallowPass                    -- Shallow result matches, we aren't simulating
            | SimGenerated                   -- Shallow passed, testbench generated, not running sim
            | CodeGenFail String             -- Shallow passed, testbench generation failed
            | CompileFail String             -- VHDL compilation failed during simulation
            | SimFail     String             -- Modelsim failed for some other reason
            | CompareFail Trace Trace String -- Deep result didn't match the shallow result
            | Pass        Trace Trace String -- Deep matches shallow which matches expected

addtoSummary :: Result -> Summary -> Summary
addtoSummary (ShallowFail _ _) s = s { sfail = 1 + (sfail s) }
addtoSummary ShallowPass       s = s { spass = 1 + (spass s) }
addtoSummary SimGenerated      s = s { generated = 1 + (generated s) }
addtoSummary (CodeGenFail _)   s = s { codegenfail = 1 + (codegenfail s) }
addtoSummary (CompileFail _)   s = s { vhdlfail = 1 + (vhdlfail s) }
addtoSummary (SimFail _)       s = s { simfail = 1 + (simfail s) }
addtoSummary (CompareFail _ _ _) s = s { compfail = 1 + (compfail s) }
addtoSummary (Pass _ _ _)        s = s { passed = 1 + (passed s) }

generateReport :: [TestCase] -> Report
generateReport rs = Report summary rs
    where rs' = map snd rs
          summary = foldr addtoSummary (Summary 0 0 0 0 0 0 0 0 (length rs')) rs'

reportToSummaryHtml :: Report -> IO String
reportToSummaryHtml (Report summary _) = do
    header <- readFile "header.inc"
    mid <- readFile "mid.inc"
    footer <- readFile "footer.inc"

    return $ header ++ (summaryToHtml summary) ++ mid ++ footer

summaryToHtml :: Summary -> String
summaryToHtml s = unlines [ "<table>"
                          , "<tr><td colspan=\"2\">Total Tests</td><td>" ++ show (total s) ++ "</td></tr>"
                          , "<tr><td>Shallow</td>"
                          , "<td>" ++ show (total s - sfail s) ++ "/" ++ show (total s) ++ "</td>"
                          , "<td>" ++ show (sfail s) ++ " failed</td></tr>"
                          , "<tr><td>Deep</td>"
                          , "<td>" ++ show (passed s) ++ "/" ++ show (total s - sfail s) ++ "</td>"
                          , "<td>" ++ show (total s - sfail s - passed s) ++ " failed</td></tr>"
                          , "<tr><td colspan=\"2\">VHDL Generation Failures</td><td>" ++ show (codegenfail s) ++ "</td></tr>"
                          , "<tr><td colspan=\"2\">VHDL Compilation Failures</td><td>" ++ show (vhdlfail s) ++ "</td></tr>"
                          , "<tr><td colspan=\"2\">Comparison Failures</td><td>" ++ show (compfail s) ++ "</td></tr>"
                          , "<tr><td colspan=\"2\">Other Simulation Failures</td><td>" ++ show (simfail s) ++ "</td></tr>"
                          , "</table>"
                          ]

reportToHtml :: Report -> IO String
reportToHtml (Report summary results) = do
    header <- readFile "header.inc"
    mid <- readFile "mid.inc"
    footer <- readFile "footer.inc"

    let res = unlines [ concat ["<div id=\"", name, "\" class=\"header ", sc, "\">", name
                               ,"<span class=\"status\">", s, "</span></div>\n<div class=\"additional\">"
                               , a, "</div>"]
                      | (name, r) <- results
                      , let (sc, s, a) = case r of
                                                        -- AJG: This take 1000 is a hack,
                                                        -- We need to truncate all these infomational outputs
                                                        -- both in terms of rows and column.
                                                        -- Without the take, ts is an infinite list, so fills your disk with X's.
                                           ShallowFail t ts -> ("shallowfail", "Shallow Failed", unDiv [show t, take 1000 $ show ts])
                                           ShallowPass -> ("shallowpass", "Shallow Passed", unDiv [""])
                                           SimGenerated -> ("simgenerated", "Simulation Generated", unDiv [""])
                                           CodeGenFail s -> ("codegenfail", "VHDL Generation Failed", unDiv [s])
                                           CompileFail s -> ("compilefail", "VHDL Compilation Failed", unDiv [s])
                                           SimFail s -> ("simfail", "Simulation Failed (other)", unDiv [s])
                                           CompareFail t1 t2 s -> ("comparefail", "Failed", unDiv [show t1, show t2, s])
                                           Pass t1 t2 s -> ("pass", "Passed", unDiv [show t1, show t2, s])
                      ]
    return $ header ++ (summaryToHtml summary) ++ mid ++ res ++ footer

unDiv :: [String] -> String
unDiv = foldr (\s tail -> "<div>" ++ s ++ "</div>" ++ tail) ""

