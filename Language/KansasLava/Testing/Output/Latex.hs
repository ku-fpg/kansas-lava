{-# LANGUAGE ParallelListComp  #-}

module Language.KansasLava.Testing.Output.Latex( Options(..), Format(..), defOpts
                                               , mkDiagram, mkDocument, mkFigure, mkTable
                                               , updateMapping, updateDefaultMapping) where

import Language.KansasLava
import Language.KansasLava.Test -- remove this eventually!
import qualified Data.List as List
import qualified Data.Map as Map

data Format  = Boxes | Waves | TruthTable
    deriving (Show)

data Options = Options { elemWidth :: Int
                       , format :: Format
                       , maxResults :: Int
                       , showClock :: Bool
                       , valueMap :: (String -> String, Map.Map String String) }
--    deriving (Show)

mkDiagram opts = case format opts of
                    TruthTable -> printTable opts
                    _          -> printTiming opts

printTable opts nms tt = table $ unlines [ tabular l | l <- showLatex opts tt ]
    where table contents = unlines ["\\begin{tabular}{|" ++ unwords (replicate (length nms) "c |") ++ "}"
                                   ,"\\hline"
                                   ,unwords (List.intersperse "&" nms) ++ "\\\\"
                                   ,"\\hline"
                                   ,"\\hline"
                                   ,contents
                                   ,"\\hline"
                                   ,"\\end{tabular}"]

tabular :: [String] -> String
tabular is = unwords (List.intersperse "&" is) ++ "\\\\"

printTiming opts nms tt = table $ unlines finalTable
    where table contents = "\\begin{tikztimingtable}\n" ++ contents ++ "\\end{tikztimingtable}\n\n"
          waves = List.transpose $ showLatex opts tt
          waveTable = [ i ++ " & " ++ formatted l ++ "\\\\" | l <- waves | i <- nms ++ defNames]
          width = show $ elemWidth opts
          clockSignal = width ++ "c"
          finalTable = case showClock opts of
                        True  -> ("clock & " ++ (concat $ replicate ((length $ head waves)*2) clockSignal) ++ "\\\\")
                                 : waveTable
                        False -> waveTable
          defNames = [ "i" ++ (show i) | i <- [(length nms)..] ]
          formatted = case format opts of
                        Boxes -> toBoxes opts
                        Waves -> toWaves opts

toBoxes :: Options -> [String] -> String
toBoxes opts is = "z" ++ concat [ width ++ "D{" ++ i ++ "}" | i <- is ] ++ "z"
    where width = show $ elemWidth opts

toWaves :: Options -> [String] -> String
toWaves opts is = concat [ width ++ translate opts i | i <- is ]
    where width = show $ elemWidth opts

translate :: Options -> String -> String
translate opts str = Map.findWithDefault (f str) str m
    where (f, m) = valueMap opts

showLatex :: Options -> TT -> [[String]]
showLatex opts (TT lines) = take (maxResults opts) $ concat [ showLine l | l <- lines ]

showLine :: TTL -> [[String]]
showLine (CombValue val tt) = [ val : rs | rss <- map showLine (unTT tt), rs <- rss ]
showLine (SeqValue val ttl) = [ val : rs | rs <- showLine ttl ]
showLine (ResV val)         = [[ val ]]
showLine (TupleTT ttls)     = List.transpose [ x | [x] <- map showLine ttls ]

defMapping = (\str -> "D{" ++ str ++ "}",
              Map.fromList [("F", "L")
                           ,("T" , "H")
                           ,("?"    , "U")
                           ])

defOpts = Options { format = Waves
                  , elemWidth = 4
                  , maxResults = 20
                  , showClock = False
                  , valueMap = defMapping }

updateMapping :: Options -> String -> String -> Options
updateMapping opts k v = opts { valueMap = (f, Map.update (\_ -> Just v) k m) }
    where (f, m) = valueMap opts

updateDefaultMapping :: Options -> (String -> String) -> Options
updateDefaultMapping opts f = opts { valueMap = (f, m) }
    where (_, m) = valueMap opts

writeTimingDiagram title inps tt = writeFile "chunk.tex" $ mkDocument $ mkFigure title $ mkDiagram defOpts inps tt

mkWrapper b c e = unlines b ++ c ++ unlines e

mkDocument content = mkWrapper ["\\documentclass{article}"
                               ,"\\usepackage{tikz}"
                               ,"\\usepackage{tikz-timing}"
                               ,"\\pagestyle{empty}"
                               ,"\\begin{document}"]
                               content
                               ["\\end{document}"]

mkFigure title figure = mkWrapper ["\\begin{figure}"
                                  ,"\\centering"]
                                  figure
                                  ["\\caption{" ++ title ++ "}"
                                  ,"\\end{figure}"]

mkTable title table = mkWrapper ["\\begin{table}"
                                ,"\\centering"]
                                table
                                ["\\caption{" ++ title ++ "}"
                                ,"\\end{table}"]
