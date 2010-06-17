import System
import Data.Sized.Unsigned
import Language.KansasLava
import Language.KansasLava.Test -- remove me eventually!
import Language.KansasLava.Testing.Output.Latex

main = do writeFile "diagrams.tex" (mkDocument $ unlines $
            -- bitNot
            [mkFigure "bitNot" $ mkDiagram (updateMapping defOpts "?" "X") ["i0", "o0"]
                               $ truthTable (bitNot :: Comb Bool -> Comb Bool)] ++
            -- mux2 - waveform
            [mkFigure "mux2 - Waves" $ mkDiagram (defOpts { elemWidth = 2, maxResults = 60 })
                                                 ["s0", "i0", "i1", "o0"] $
                truthTable (mux2 :: Comb Bool -> (Comb Bool, Comb Bool) -> Comb Bool)] ++
            -- mux2 - truth table
            [mkFigure "mux2 - Truth Table" $ mkDiagram (defOpts { format = TruthTable, maxResults = 40 })
                                                       ["s0", "i0", "i1", "o0"] $
                truthTable (mux2 :: Comb Bool -> (Comb Bool, Comb Bool) -> Comb Bool)] ++
            ["\\clearpage"] ++ -- apparently latex has a limit to number of floats it can juggle, this flushes them
            -- and2, or2, and xor2
            [mkFigure (unwords [name,"-",show $ format opts]) (mkDiagram opts ["i0", "i1", "o0"] (truthTable fn))
            | (name,fn) <- all2
            , opts <- [defOpts, defOpts { format = Boxes }]
            ] ++
            -- boolean ops
            [mkFigure (unwords [name,"-",show $ format opts]) (mkDiagram opts ["i0", "i1", "o0"] (truthTable fn))
            | (name,fn) <- allBool
            , opts <- [defOpts, defOpts { format = Boxes }]
            ])

          system "pdflatex diagrams.tex"
    where all2 = [("and2", (and2 :: Comb Bool -> Comb Bool -> Comb Bool))
                 ,("or2", (or2 :: Comb Bool -> Comb Bool -> Comb Bool))
                 ,("xor2", (xor2 :: Comb Bool -> Comb Bool -> Comb Bool))
                 ]
          allBool = [(".==.", ((.==.) :: Comb U1 -> Comb U1 -> Comb Bool))
                    ,("$.>=.$", ((.>=.) :: Comb U1 -> Comb U1 -> Comb Bool)) -- dollar signs so latex
                    ,("$.<=.$", ((.<=.) :: Comb U1 -> Comb U1 -> Comb Bool)) -- doesn't munch the > and
                    ,("$.<.$",  ((.<.)  :: Comb U1 -> Comb U1 -> Comb Bool)) -- < symbols
                    ,("$.>.$",  ((.>.)  :: Comb U1 -> Comb U1 -> Comb Bool))
                    ]
