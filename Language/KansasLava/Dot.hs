module Language.KansasLava.Dot
	( writeDotCircuit
	) where

import Language.KansasLava.IO
import Language.KansasLava.Entity
import Language.KansasLava.Reify
import Language.KansasLava.Type
import Text.Dot

writeDotCircuit :: (REIFY circuit) => [ReifyOptions] -> String -> circuit -> IO ()
writeDotCircuit opts filename circuit = do
{-
   let (inputs',blob) = output' circuit
   let inputs = map fst inputs'
-}
   (ReifiedCircuit nodes inputs' outputs types) <- reifyCircuit opts circuit
   print (nodes,inputs',outputs,types)
   let inputs = inputs'

   let findTy :: QVar -> BaseTy
       findTy v = case lookup v types of
		    Nothing -> error $ "can not find type for : " ++ show v
		    Just ty -> ty

   let showP (i,v) = "<" ++ show v ++ ">" ++ show v ++ "::" ++ show (findTy (i,v))

   let  mkLabel nm ins outs = 
	      (concatMap addSpecial $ show nm) ++ "|{{" 
 	   ++ join (map showP ins) ++ "}|{"
	   ++ join (map showP outs) ++ "}}"


   print ("INPUTS:" ,inputs)
   print ("NODES:" ,nodes)
   print ("OUTPUTS:" ,outputs)
   print ("TYPES:" ,types)

--   print (inputs,inputs',nodes,outputs,types)


   writeFile filename $ showDot $ do
	attribute ("rankdir","LR")

	input_bar <- node [ ("label","INPUTS|{{" ++ join [ showP (Source,i) | i <- inputs] ++ "}}") 
	 		                 , ("shape","record")
			       		 , ("style","filled")
			       		 ]


	nds <- sequence [ do nd <- node [ ("label",mkLabel nm [ (Uq n,v) |(v,_) <- ins ] 
							      [ (Uq n,v) | v <- outs] )
	 		                 , ("shape","record")
			       		 , ("style","rounded")
			       		 ]
			     return (n,nd)
		        | (n,Entity nm outs ins _) <- nodes ]

	output_bar <- node [ ("label","OUTPUTS|{{" ++ join [ showP (Sink,i) | (i,_) <- outputs ] ++ "}}") 
	 		                 , ("shape","record")
			       		 , ("style","filled")
			       		 ]

	let findNd (Uq n) = case lookup n nds of
			     Nothing -> error $ "strange port: " ++ show (n,nds)
			     Just nd -> nd
	    findNd Source = input_bar
	    findNd Sink   = output_bar

	let drawEdge dr n v = case dr of
		     Port nm' n' -> edge' (findNd n') (Just (show nm' ++ ":e")) n (Just (show v ++ ":w")) []
		     Pad v' | v' `elem` inputs
					 -> edge' input_bar (Just (show v' ++ ":e")) n (Just (show v ++ ":w")) []
			    | otherwise  -> do nd' <- node [ ("label",show v')
				  	                   ]
				               edge' nd' Nothing n (Just (show v ++ ":w")) []
		     Lit i -> do nd' <- node [("label",show i),("shape","none")]
				 edge' nd' Nothing n (Just (show v ++ ":w")) []

	sequence [ drawEdge dr output_bar v
		 | (v,dr) <- outputs 
		 ]

	sequence [ drawEdge dr (findNd (Uq n)) v
	       	 | (n,Entity nm outs ins _) <- nodes
		 , (v,dr) <- ins 
		 ]

	return ()
        

-- addSpecial '>' = ['\\','>']
addSpecial '>' = "&gt;";
addSpecial '<' = "&lt;";
addSpecial c = [c]

join [x] = x
join []  = ""
join (x:xs) = x ++ "|" ++ join xs

