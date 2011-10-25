-- | Convert a reified Lava 'KLEG' into a graphical Graphviz format.
module Language.KansasLava.DOT (writeDotCircuit) where

import Language.KansasLava.Types

import Data.Reify.Graph(Unique)
import Text.Dot(Dot,NodeId,showDot, attribute, node,edge')
import Data.List(intercalate)
import Data.Maybe(fromMaybe)

-- | The 'writeDotCircuit' function converts a Lava circuit into a graphviz output.
writeDotCircuit :: FilePath  -- ^ Name of output dot file, can be relative or absolute path.
                -> KLEG      -- ^ The reified Lava circuit.
                -> IO ()
writeDotCircuit filename (KLEG nodes circInputs circOutputs) = do

   let showP :: (String,Type) -> String
       showP (v,ty) = "<" ++ v ++ ">" ++ v ++ "::" ++ show ty

       join = intercalate "|"

       mkLabel :: String -> [(String,Type)] -> [(String,Type)] -> String
       mkLabel nm ins outs =
              concatMap addSpecial nm ++ "|{{"
           ++ join (map showP ins) ++ "}|{"
           ++ join (map showP outs) ++ "}}"

       -- TODO: insert types
       -- mkPLabel pname nm ins outs = "{" ++ (concatMap addSpecial $ show nm) ++ "|" ++ join pname ++ "}|{{"
       --     ++ join (map showP ins) ++ "}|{"
       --     ++ join (map showP outs) ++ "}}"

   writeFile filename $ showDot $ do
        attribute ("rankdir","LR")

        input_bar <- node [  ("label","INPUTS|{{" ++ join [ showP (show o,i) | (o,i) <- circInputs] ++ "}}")
                          , ("shape","record")
                          , ("style","filled")
                          ]


        nds <- sequence [ do nd <- node [ ("label",mkLabel (show nm)
                                                    [ (v,ty) |(v,ty,_) <- ins ]
                                                    [ (v,ty) | (v,ty) <- outs] )
                                        , ("shape","record")
                                        , ("style","rounded")
                                        ]
                             return (n,nd)
                          | (n,Entity nm outs ins) <- nodes ]


        output_bar <- node [ ("label","OUTPUTS|{{" ++ join [ showP (show i,ty) | (i,ty,_) <- circOutputs ] ++ "}}")
                                         , ("shape","record")
                                         , ("style","filled")
                                         ]

        let findNd n = fromMaybe (error $ "strange port: " ++ show (n,nds)) (lookup n nds)

        let drawEdge :: Driver Unique -> NodeId -> String -> Dot ()
            drawEdge (Port nm' n') n v = edge' (findNd n') (Just (show nm' ++ ":e")) n (Just (show v ++ ":w")) []
            drawEdge (Pad v') n v
              | v' `elem` map fst circInputs
                = edge' input_bar (Just (show (show v') ++ ":e")) n (Just (show v ++ ":w")) []
              | otherwise = do nd' <- node [ ("label",show v')]
                               edge' nd' Nothing n (Just (show v ++ ":w")) []
            drawEdge (Lit i) n v = do nd' <- node [("label",show i),("shape","none")]
                                      edge' nd' Nothing n (Just (show v ++ ":w")) []
            drawEdge (Generic i) n v = do nd' <- node [("label",show i),("shape","none")]
                                          edge' nd' Nothing n (Just (show v ++ ":w")) []
            drawEdge (Error e) n v = do nd' <- node [("label",show e),("shape","none")]
                                        edge' nd' Nothing n (Just (show v ++ ":w")) []
            drawEdge (ClkDom nm) n v = do nd' <- node [("label",show nm),("shape","none")]
                                          edge' nd' Nothing n (Just (show v ++ ":w")) []
            drawEdge (Lits ls) n v =  do let label = intercalate "," $ map  show ls
                                         nd' <- node [("label",label),("shape","none")]
                                         edge' nd' Nothing n (Just (show v ++ ":w")) []


        -- Draw edges to the outputs
        sequence_ [ drawEdge dr output_bar (show v)
                 | (v,_,dr) <- circOutputs
                 ]

        -- Draw edges between nodes
        sequence_ [ drawEdge dr (findNd n) v
                 | (n,Entity _ _ ins) <- nodes
                 , (v,_,dr) <- ins
                 ]

        return ()

   return () -- for chaining purposes



-- addSpecial '>' = ['\\','>']
addSpecial :: Char -> String
addSpecial '>' = "&gt;";
addSpecial '<' = "&lt;";
addSpecial c = [c]

