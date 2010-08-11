{-# LANGUAGE TypeFamilies, FlexibleInstances,ParallelListComp, ScopedTypeVariables #-}
module Language.KansasLava.Reify where

import Data.Reify
import Data.List as L


import Language.KansasLava.Entity
import Language.KansasLava.Entity.Utils
import Language.KansasLava.Wire
import Language.KansasLava.Comb
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Type
import Language.KansasLava.Circuit
import Language.KansasLava.Opt
import Language.KansasLava.Utils

import Data.Sized.Matrix as M
import Debug.Trace

-- | reifyCircuit does reification and type inference.
-- reifyCircuit :: REIFY circuit => [ReifyOptions] -> circuit -> IO ReifiedCircuit
-- ([(Unique,Entity (Ty Var) Unique)],[(Var,Driver Unique)])
reifyCircuit :: (Ports a) => [ReifyOptions] -> a -> IO ReifiedCircuit
reifyCircuit opts circuit = do
        -- GenSym for input/output pad names
	let inputNames = L.zipWith PadVar [0..] $ head $
		[ nms | InputNames nms <- opts ] ++ [[ "i" ++ show i | i <- [0..]]]
	let outputNames =  L.zipWith PadVar [0..] $ head $
		[ nms | OutputNames nms <- opts ] ++ [[ "o" ++ show i | i <- [0..]]]

	let os = ports 0 circuit

	let o = Port (Var "o0")
            	$ E
            	$ Entity (Name "Lava" "top") [(Var "o0",B)]	-- not really a Bit
             	[ (Var $ "i_" ++ show i,tys, dr)
	     	| (i,(tys,dr)) <- zip [0..] os
             	]
             	[]

        -- Get the graph, and associate the output drivers for the graph with
        -- output pad names.
        (gr, outputs) <- case o of
                Port _ o' -> do
                   (Graph gr out) <- reifyGraph o'
		   let gr' = [ (nid,nd) | (nid,nd) <- gr
				        , nid /= out
			     ]
		   case lookup out gr of
                     Just (Entity (Name "Lava" "top")  _ ins _) ->
                       return $ (gr',[(sink,ity, driver)
                                       | (_,ity,driver) <- ins
                                       | sink <- outputNames
				       ])
                     Just (Entity (Name _ _) outs _ _) ->
                       return $ (gr', [(sink,oty, Port ovar out)
                                      | (ovar,oty) <- outs
                                      | sink <- outputNames
				      ])
		     Just (Table (ovar,oty) _ _) ->
		       return $ (gr', [ (sink,oty, Port ovar out)
                                     | sink <- [head outputNames]
				     ])
                     _ -> error $ "reifyCircuit: " ++ show o
		-- TODO: restore this
--                (Lit x) -> return ([],[(Var (head outputNames),ty,Lit x)])
                v -> fail $ "reifyGraph failed in reifyCircuit" ++ show v

        -- Search all of the enities, looking for input ports.
        let inputs = [ (v,vTy) | (_,Entity nm _ ins _) <- gr
			       , (_,vTy,Pad v) <- ins]
		  ++ [ (v,vTy) | (_,Table _ (_,vTy,Pad v) _) <- gr ]
        let rCit = ReifiedCircuit { theCircuit = gr
                                  , theSrcs = nub inputs
                                  , theSinks = outputs
                                  }

--	print rCit

	let rCit' = resolveNames rCit
	let rCit = rCit'

--	print rCit
        if OptimizeReify `elem` opts then optimize opts rCit else return rCit


wireCapture :: forall w . (Wire w) => D w -> [(BaseTy, Driver E)]
wireCapture (D d) = [(wireType (error "wireCapture" :: w), d)]


showReifiedCircuit :: (Ports circuit) => [ReifyOptions] -> circuit -> IO String
showReifiedCircuit opt c = do
	rCir <- reifyCircuit opt c
	return $ show rCir

debugCircuit :: (Ports circuit) => [ReifyOptions] -> circuit -> IO ()
debugCircuit opt c = showReifiedCircuit opt c >>= putStr

-- | The 'Ports' class generates input pads for a function type, so that the
-- function can be Reified. The result of the circuit, as a driver, as well as
-- the result's type, are returned. I _think_ this takes the place of the REIFY
-- typeclass, but I'm not really sure.

class Ports a where
  ports :: Int -> a -> [(BaseTy, Driver E)]

class InPorts a where
    inPorts :: Int -> (a, Int)

    input :: String -> a -> a


instance Wire a => Ports (CSeq c a) where
  ports _ sig = wireCapture (seqDriver sig)

instance Wire a => Ports (Comb a) where
  ports _ sig = wireCapture (combDriver sig)

instance (Ports a, Ports b) => Ports (a,b) where
  ports _ (a,b) = ports bad b ++
		  ports bad a
     where bad = error "bad using of arguments in Reify"

instance (Ports a, Ports b, Ports c) => Ports (a,b,c) where
  ports _ (a,b,c)
 		 = ports bad c ++
		   ports bad b ++
		   ports bad a
     where bad = error "bad using of arguments in Reify"

instance (Ports a,Size x) => Ports (Matrix x a) where
 ports _ m = concatMap (ports (error "bad using of arguments in Reify")) $ M.toList m

instance (InPorts a, Ports b) => Ports (a -> b) where
  ports vs f = ports vs' $ f a
     where (a,vs') = inPorts vs

--class OutPorts a where
--    outPorts :: a ->  [(Var, BaseTy, Driver E)]


{-
input nm = liftS1 $ \ (Comb a d) ->
	let res  = Comb a $ D $ Port (Var "o0") $ E $ entity
	    entity = Entity (Name "Lava" "input")
                    [(Var "o0", bitTypeOf res)]
                    [(Var nm, bitTypeOf res, unD d)]
		    []
	in res

-}

wireGenerate :: Int -> (D w,Int)
wireGenerate v = (D (Pad (PadVar v ("i_" ++ show v))),succ v)

instance Wire a => InPorts (CSeq c a) where
    inPorts vs = (Seq (error "InPorts (Seq a)") d,vs')
      where (d,vs') = wireGenerate vs
    input nm = liftS1 (input nm)

instance Wire a => InPorts (Comb a) where
    inPorts vs = (Comb (error "InPorts (Comb a)") d,vs')
      where (d,vs') = wireGenerate vs

    input nm (Comb a d) =
	let res  = Comb a $ D $ Port (Var "o0") $ E $ entity
	    entity = Entity (Name "Lava" "input")
                    [(Var "o0", bitTypeOf res)]
                    [(Var nm, bitTypeOf res, unD d)]
		    []
	in res
{-

instance InPorts (Env clk) where
  inPorts nms = (Env (
-}

instance InPorts (Clock clk) where
    inPorts vs = (Clock (error "InPorts (Clock clk)") d,vs')
      where (d,vs') = wireGenerate vs

    input nm (Clock f d) =
	let res  = Clock f $ D $ Port (Var "o0") $ E $ entity
	    entity = Entity (Name "Lava" "input")
                    [(Var "o0", ClkTy)]
                    [(Var nm, ClkTy, unD d)]
		    []
	in res

instance InPorts (Env clk) where
    inPorts vs0 = (Env clk rst en,vs3)
	 where ((en,rst,clk),vs3) = inPorts vs0

    input nm (Env clk rst en) = Env (input ("clk" ++ nm) clk)
			            (input ("rst" ++ nm) rst)
			            (input ("sysEnable" ++ nm) en)	-- TODO: better name than sysEnable, its really clk_en

instance (InPorts a, InPorts b) => InPorts (a,b) where
    inPorts vs0 = ((a,b),vs2)
	 where
		(b,vs1) = inPorts vs0
		(a,vs2) = inPorts vs1

    input nm (a,b) = (input (nm ++ "_fst") a,input (nm ++ "_snd") b)

instance (InPorts a, Size x) => InPorts (Matrix x a) where
 inPorts vs0 = (M.matrix bs, vsX)
     where
	sz :: Int
	sz = size (error "sz" :: x)

	loop vs0 0 = ([], vs0)
	loop vs0 n = (b:bs,vs2)
	   where (b, vs1) = inPorts vs0
		 (bs,vs2) = loop vs1 (n-1)

	bs :: [a]
	(bs,vsX) = loop vs0 sz

 input nm m = forEach m $ \ i a -> input (nm ++ "_" ++ show i) a

instance (InPorts a, InPorts b, InPorts c) => InPorts (a,b,c) where
    inPorts vs0 = ((a,b,c),vs3)
	 where
		(c,vs1) = inPorts vs0
		(b,vs2) = inPorts vs1
		(a,vs3) = inPorts vs2

    input nm (a,b,c) = (input (nm ++ "_fst") a,input (nm ++ "_snd") b,input (nm ++ "_thd") c)

---------------------------------------

showOptReifiedCircuit :: (Ports circuit) => [ReifyOptions] -> circuit -> IO String
showOptReifiedCircuit opt c = do
	rCir <- reifyCircuit opt c
	let loop n cs@((nm,Opt c _):_) | and [ n == 0 | (_,Opt c n) <- take 3 cs ] = do
		 putStrLn $ "## Answer " ++ show n ++ " ##############################"
		 print c
		 return c
	    loop n ((nm,Opt c v):cs) = do
		print $ "Round " ++ show n ++ " (" ++ show v ++ " " ++ nm ++ ")"
		print c
		loop (succ n) cs

	let opts = cycle [ ("opt",optimizeReifiedCircuit)
		       	 , ("copy",copyElimReifiedCircuit)
			 , ("dce",dceReifiedCircuit)
			 ]

	rCir' <- loop 0 (("init",Opt rCir 0) : optimizeReifiedCircuits opts rCir)
	return $ show rCir'


-------------------------------------------------------------


output :: (Signal seq, Wire a)  => String -> seq a -> seq a
output nm = liftS1 $ \ (Comb a d) ->
	let res  = Comb a $ D $ Port (Var nm) $ E $ entity
	    entity = Entity (Name "Lava" "output")
                    [(Var nm, bitTypeOf res)]
                    [(Var "i0", bitTypeOf res, unD d)]
		    []
	in res

resolveNames :: ReifiedCircuit -> ReifiedCircuit
resolveNames cir 
	| error1 = error $ "The generated input/output names are non distinct: " ++
			   show (map fst (theSrcs cir))
	| not (null error2) = error $ "A name has been used both labeled and non labeled "
	| error3 = error "The labled input/output names are non distinct"	
	| otherwise = ReifiedCircuit { theCircuit = newCircuit
			 	     , theSrcs = newSrcs
				     , theSinks = newSinks
		           	     }
  where
	error1 = L.length (map fst (theSrcs cir)) /= L.length (nub (map fst (theSrcs cir)))
	error2 =  [ v
			| (_,e) <- newCircuit
			, v <- case e of
			    Entity _ _ ins _ -> [ nm | (_,_,Pad nm) <- ins ]
			    Table _ ins _ -> [ nm | (_,_,Pad nm) <- [ins]]
			, v `elem` oldSrcs
			]
	error3 = L.length (map fst newSrcs) /= L.length (nub (map fst newSrcs))	
	
	newCircuit = 
		[ ( u
		  , case e of
		      Entity (Name "Lava" "input") outs [(Var oNm,oTy,Pad (PadVar i _))] misc
			-> Entity (Name "Lava" "id") outs [(Var oNm,oTy,Pad (PadVar i oNm))] misc
		      Entity (Name "Lava" io) outs ins misc
			| io `elem` ["input","output"]
			-> Entity (Name "Lava" "id") outs ins misc
		      other -> other
		   )
		| (u,e) <- theCircuit cir
		]

	newSrcs :: [(PadVar,BaseTy)]
	newSrcs = [ case lookup nm mapInputs of
		       Nothing -> (nm,ty)
		       Just nm' -> (nm',ty)
	          | (nm,ty) <- theSrcs cir
		  ]

	-- Names that have been replaced.
	oldSrcs = [ nm
		  | (nm,ty) <- theSrcs cir
		  , not (nm `elem` (map fst newSrcs))
		  ]

	newSinks :: [(PadVar,BaseTy,Driver Unique)]
	newSinks = [ case dr of
		      Port (Var nm') u | isOutput u -> (PadVar i nm',ty,dr)
		      _ -> (nm,ty,dr)
		   | (nm@(PadVar i _),ty,dr) <- theSinks cir
		   ]

	isOutput u = case lookup u (theCircuit cir) of
			Just (Entity (Name "Lava" "output") _ _ _) -> True
			_ -> False

	mapInputs :: [(PadVar,PadVar)]
	mapInputs = [ (PadVar i inp,PadVar i nm)
		    | (_,Entity (Name "Lava" "input") _ [(Var nm,_,Pad (PadVar i inp))] _) <- theCircuit cir
		    ]


	isInput u = case lookup u (theCircuit cir) of
			Just (Entity (Name "Lava" "input") _ _ _) -> True
			_ -> False

