-- | The 'Dot' module converts a Lava circuit into a graphical Graphviz format.
module Language.KansasLava.Dot
	( writeDotCircuit
	) where

import Language.KansasLava.Entity
import Language.KansasLava.Reify
import Text.Dot

-- | The 'writeDot' function converts a Lava circuit into a graphviz output. The
--   circuit type must implement the 'Ports' class, so that the circuit
--   inputs/outpus can be represented in the output graph.
writeDotCircuit :: (Ports circuit) =>
                   [ReifyOptions] -- ^ Options for controlling the observable-sharing reification.
                -> String  -- ^ The name of the circuit. The graph will be
                           -- written a file with this name (with a .dot extension).
                -> circuit  -- ^ The Lava circuit.
                -> IO ()
writeDotCircuit opts filename circuit = do
{-
   let (inputs',blob) = output' circuit
   let inputs = map fst inputs'
-}
   (ReifiedCircuit nodes inputs' outputs) <- reifyCircuit opts circuit
   print (nodes,inputs',outputs)
   let inputs = inputs'

   let showP (_,(v,ty)) = "<" ++ show v ++ ">" ++ show v ++ "::" ++ show ty

   let  mkLabel nm ins outs =
	      (concatMap addSpecial $ show nm) ++ "|{{"
 	   ++ join (map showP ins) ++ "}|{"
	   ++ join (map showP outs) ++ "}}"


   print ("INPUTS:" ,inputs)
   print ("NODES:" ,nodes)
   print ("OUTPUTS:" ,outputs)
   -- print ("TYPES:" ,types)

--   print (inputs,inputs',nodes,outputs,types)


   writeFile filename $ showDot $ do
	attribute ("rankdir","LR")

	input_bar <- node [ ("label","INPUTS|{{" ++ join [ showP (Source,i) | i <- inputs] ++ "}}")
	 		                 , ("shape","record")
			       		 , ("style","filled")
			       		 ]


	nds <- sequence [ do nd <- node [ ("label",mkLabel nm [ (n,(v,ty)) |(v,ty,_) <- ins ]
							      [ (n,(v,ty)) | (v,ty) <- outs] )
	 		                 , ("shape","record")
			       		 , ("style","rounded")
			       		 ]
			     return (n,nd)
		        | (n,Entity nm outs ins _) <- nodes ]

	output_bar <- node [ ("label","OUTPUTS|{{" ++ join [ showP (Sink,(i,ty)) | (i,ty,_) <- outputs ] ++ "}}")
	 		                 , ("shape","record")
			       		 , ("style","filled")
			       		 ]

	let findNd (Uq n) = case lookup n nds of
			     Nothing -> error $ "strange port: " ++ show (n,nds)
			     Just nd -> nd
	    findNd Source = input_bar
	    findNd Sink   = output_bar

	let drawEdge dr n v = case dr of
		     Port nm' n' -> let (Just nd) = lookup n' nds
                                    in edge' nd (Just (show nm' ++ ":e")) n (Just (show v ++ ":w")) []
		     Pad v' | v' `elem` (map fst inputs)
					 -> edge' input_bar (Just (show v' ++ ":e")) n (Just (show v ++ ":w")) []
			    | otherwise  -> do nd' <- node [ ("label",show v')
				  	                   ]
				               edge' nd' Nothing n (Just (show v ++ ":w")) []
		     Lit i -> do nd' <- node [("label",show i),("shape","none")]
				 edge' nd' Nothing n (Just (show v ++ ":w")) []
                     p@(PathPad _) -> error $ "Unmatched pattern in drawEdge: " ++ show p

	sequence [ drawEdge dr output_bar v
		 | (v,_,dr) <- outputs
		 ]

	sequence [ drawEdge dr (findNd (Uq n)) v
	       	 | (n,Entity _ _ ins _) <- nodes
		 , (v,_,dr) <- ins
		 ]

	return ()


-- addSpecial '>' = ['\\','>']
addSpecial :: Char -> String
addSpecial '>' = "&gt;";
addSpecial '<' = "&lt;";
addSpecial c = [c]

join :: [String] -> String
join [x] = x
join []  = ""
join (x:xs) = x ++ "|" ++ join xs

