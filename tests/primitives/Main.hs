{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts, Rank2Types, ExistentialQuantification, TypeFamilies #-}

import Language.KansasLava

import System.IO.Unsafe

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Dynamic

import Data.Bits
import Data.List as L
import Data.Maybe

import Data.Sized.Arith as A
import Data.Sized.Matrix as M
import Data.Sized.Sampled as S

import Data.Sized.Signed as S
import Data.Sized.Unsigned as U

import Debug.Trace

import Language.KansasLava.Netlist
import Language.KansasLava.Probes
import Language.KansasLava.VHDL.Testbench

import Language.Netlist.GenVHDL

import System.Environment
import System.Directory
import System.Cmd
import System.IO

import qualified System.Posix.Env as Posix

import Text.PrettyPrint(render)

-- CONFIG --
runTests = [] -- empty list builds every test

numberOfCycles :: Int
numberOfCycles = 100

dumpDir = "examine/"
-- END CONFIG --

-- TESTS --
main = run runTests

run enabled = do
    let testCircuit :: (Ports a, Probe a, Ports b) => String -> a -> (a -> b) -> IO ()
        testCircuit = testCircuit' enabled

    Posix.setEnv "LAVA_SIM_PATH" dumpDir True
    createDirectoryIfMissing True dumpDir

    let env = takeThenSeq 7 shallowRst env
        env' = takeThenSeq 40 shallowRst env'

        inp :: Seq U4
        inp  = toSeq $ cycle [0..15]
        inp2 :: Seq U4
        inp2 = toSeq $ cycle $ reverse [0..15]
        inp3 :: Seq U4
        inp3 = toSeq $ step 3 $ cycle $ reverse $ 0 : [0..15]

        sinp :: Seq S5
        sinp  = toSeq $ cycle [0..15]
        sinp2 :: Seq S5
        sinp2 = toSeq $ cycle $ reverse [0..15]
        sinp3 :: Seq S5
        sinp3 = toSeq $ step 3 $ cycle $ reverse $ 0 : [0..15]

        binp :: Seq Bool
        binp  = toSeq $ cycle [True, False]
        binp2 :: Seq Bool
        binp2 = toSeq $ cycle [True, True, False, False]

        step n (x:xs) = x : (step n $ drop (n - 1) xs)

        eInp :: Seq (Enabled U4)
        eInp = toEnabledSeq 
                   $ Prelude.zipWith (\ a b -> if b then Just a else Nothing)
                        (cycle [0..15]) 
                        (cycle [False,True,False,False,True])


	x9 :: Seq X9
	x9 = toSeq $ cycle [0..8]

    testCircuit "regX" 
        (register :: Rst -> Comb U4 -> Seq U4 -> Seq U4)
        (\ reg -> reg env 10 inp)
        
    testCircuit "delayX" 
        (delay :: Seq U4 -> Seq U4)
        (\ f -> f inp)

    testCircuit "muxX" 
        (mux2 :: Seq Bool -> (Seq U4, Seq U4) -> Seq U4)
        (\ f -> f (toSeq (cycle [True,False,True,True,False])) (inp, inp2))  

    testCircuit "signedArithX"
        ((\ a b -> pack (matrix [a + b, a - b{- this overflows! , a * b-}] :: Matrix X2 (Seq S5))) :: Seq S5 -> Seq S5 -> Seq (Matrix X2 S5))
        (\ f -> f sinp sinp3)

    testCircuit "unsignedArithX"
        ((\ a b -> pack (matrix [a + b, a - b, a * b] :: Matrix X3 (Seq U4))) :: Seq U4 -> Seq U4 -> Seq (Matrix X3 U4))
        (\ f -> f inp inp3)

    testCircuit "boolPrimsX"
        ((\ a b -> pack (matrix [a `and2` b, a `or2` b, a `xor2` b, bitNot a] :: Matrix X4 (Seq Bool))) :: Seq Bool -> Seq Bool -> Seq (Matrix X4 Bool))
        (\ f -> f binp binp2)

    testCircuit "boolPrims2Unsigned"
        ((\ a b -> pack (matrix [a .==. b, a .>=. b, a .<=. b, a .>. b, a .<. b] :: Matrix X5 (Seq Bool))) :: Seq U4 -> Seq U4 -> Seq (Matrix X5 Bool))
        (\ f -> f inp inp3)

    testCircuit "boolPrims2Signed"
	((\ a b -> pack (matrix [a .==. b, a .>=. b, a .<=. b, a .>. b, a .<. b] :: Matrix X5 (Seq Bool))) :: Seq S5 -> Seq S5 -> Seq (Matrix X5 Bool))
	(\ f -> f sinp sinp3)

    testCircuit "boolPrims2FLOAT"
	((\ a b -> pack (matrix [a .==. b, a .>=. b, a .<=. b, a .>. b, a .<. b] :: Matrix X5 (Seq Bool))) :: Seq FLOAT -> Seq FLOAT -> Seq (Matrix X5 Bool))
	(\ f -> f (toSeq [-32,-31..31] :: Seq (Sampled X32 X32))
		  (toSeq (reverse ([-32,-31..31] ++ [31])) :: Seq (Sampled X32 X32)))

{-  This doesn't have a deep embedding defined
        testCircuit "testABitX"
        ((\ a i -> testABit a i) :: Seq U8 -> Int -> Seq Bool)
        (\ f -> f (toSeq $ cycle $ [0..255]) 8)
-}

    testCircuit "enabledRegisterX"
        (enabledRegister :: Rst -> Comb U4 -> Seq (Enabled U4) -> Seq U4)
        (\ f -> f env 10 eInp)

    testCircuit "pipeToMemoryX"
        ((\ rst pipe -> memoryToMatrix (pipeToMemory rst pipe)) :: Rst -> Seq (Pipe X8 U4) -> Seq (Matrix X8 U4))
        (\ f -> f env' 
                $ toEnabledSeq (concat
                       [ [ return (x,y), Nothing ]
                       | (x,y) <- cycle $ [(i:: X8,(fromIntegral i * fromIntegral i) :: U4)
                                          | i <- [0..7]
                                          ]
                       ])
        )

    testCircuit "pipeToMemory2X"
        (pipeToMemory :: Rst -> Seq (Pipe X2 U4) -> Seq X2 -> Seq U4)
        (\ f -> f env' 
                  (toEnabledSeq (concat
                       [ [ return (x,y) ]
                       | (x,y) <- cycle $ [(i:: X2, j :: U4)
                                          | (i,j) <- zip (cycle [0,0,1,1,1,0,1,1]) [0..7]
                                          ]
                       ]))
                  (toSeq (cycle [0,1]))
        )

    -- Testing Sampled
    
    testCircuit "arithmeticSampled32X"
        ((\ a b -> pack (matrix [a + b, a - b] :: Matrix X2 (Seq FLOAT))) :: Seq FLOAT -> Seq FLOAT -> Seq (Matrix X2 FLOAT))
        (\ f -> f (toSeq [-32,-31..31] :: Seq (Sampled X32 X32))
                  (toSeq [-3.2,-3.15..10] :: Seq (Sampled X32 X32))
        )

    testCircuit "timesNeg0_75"
        ((\ a -> liftS1 (timesNeg0_75) a) :: Seq FLOAT -> Seq FLOAT)
        (\ f -> f (toSeq [-10,-9.5..10] :: Seq (Sampled X32 X32)))

    testCircuit "metricSampled32X"
        ((\ a b -> liftS2 (metricComb) a b) :: Seq FLOAT -> Seq FLOAT -> Seq FLOAT)
        (\ f -> f (toSeq [-2.4,-2.2..5] :: Seq (Sampled X32 X32))
                  (toSeq [-3.0,-2.5..] :: Seq (Sampled X32 X32))
        )

    testCircuit "matrixOps"
        ((pack . (\ m -> forAll $ \ i -> m ! i)  . unpack) :: Seq (Matrix X4 U4) -> Seq (Matrix X4 U4))
        (\ f -> f (pack (matrix [ inp, inp2, inp3, inp2 ])))
        
    testCircuit "matrixOps2"
        ((pack . (\ m -> (pack $ forAll $ \ i -> m ! i, pack $ forAll $ \ i -> 3))  . unpack) 
                        :: Seq (Matrix X4 U4) -> Seq (Matrix X4 U4,Matrix X3 U3))
        (\ f -> f (pack (matrix [ inp, inp2, inp3, inp2 ])))        
        
    testCircuit "matrixOps3"
        ((pack . (\ m -> (pack $ forAll $ \ i -> m ! i, pack $ forAll $ \ i -> 3, 9))  . unpack) 
                        :: Seq (Matrix X4 U4) -> Seq (Matrix X4 U4,Matrix X3 U3,U4))
        (\ f -> f (pack (matrix [ inp, inp2, inp3, inp2 ])))        

    testCircuit "muxMatrix"
	(muxMatrix :: Seq (Matrix X9 U4) -> Seq X9 -> Seq U4)
        (\ f -> f (pack (matrix [ inp, inp2, inp3, inp2, inp, inp2, inp3, inp2, inp3  ])) x9)    
	
    testCircuit "fullAdder"
        (fullAdder :: Seq Bool -> Seq Bool -> Seq Bool -> (Seq Bool, Seq Bool))
        (\ f -> f binp binp2 $ toSeq $ cycle [False, True, False])

-- HELPERS --
type FLOAT = Sampled X32 X32 

-- ldpc metric function (copied from [git]/hfec/src/KansasLava/Core.hs)
metricComb :: Comb FLOAT -> Comb FLOAT -> Comb FLOAT 
metricComb (Comb ox xe) (Comb oy ye) =
            Comb ((optX :: Maybe FLOAT -> X FLOAT) $
                  (do x <- unX ox :: Maybe FLOAT
                      y <- unX oy :: Maybe FLOAT
                      return $ signum(x) * signum(y) * (min (abs x) (abs y))))
                  (entity2 (Name "LDPC" "metric") xe ye)

timesNeg0_75 :: forall a . (Fractional a, Wire a) => Comb a -> Comb a
timesNeg0_75 (Comb a ea) = Comb (optX $ do a' <- unX a :: Maybe a
                                           return $ a' * (-0.75))
                                (entity1 (Name "LDPC" "timesNeg0_75") ea)
-- END HELPERS --

-- Everything below is to make the tests work. --
halfAdder a b = (sum,carry)
  where sum = xor2 a b
        carry = and2 a b

fullAdder a b cin = (sum,cout)
  where (s1,c1) = probe "ha1" halfAdder a b
        (sum,c2) = probe "ha2" halfAdder cin s1
        cout = xor2 c1 c2

probesTT probes = unlines
    $ take numberOfCycles
    $ mergeWith (\x y -> x ++ " | " ++ y)
    $ map (\(_, (ProbeValue _ xs)) -> valsXStream xs) probes

probesFor name plist = sortBy (\(n,_) (n2,_) -> compare n n2)
                     $ filter (\(_, (ProbeValue nm _)) -> name `isPrefixOf` nm) plist

-- Run through the list of nodes that have probes on them and generate a list of
-- subcircuits that we can build testbenches for. This depends on the behavior of
-- List.intersect (elements come out in same order as they appear in first list).
genRCs probes rc = go bfsOrder []
    where bfsOrder = intersect (bfs (sinkNames rc) rc) probelist
          probelist = map fst probes
          go [] _ = []
          go (n:ns) seen = [ (n, rc { theCircuit = trace ("newGraph: " ++ (unlines $ map show newGraph)) newGraph
                                    , theSrcs = trace ("newPads: " ++ show newPads) newPads
                                    , theSinks = trace ("newSinks: " ++ show newSinks) newSinks })
                           | not $ prefix `elem` seen
                           ] ++ (go ns (prefix:seen))
              where
                  newSinks = case lookup n (theCircuit rc) of
                                Just (Entity _ outs _ _) -> [(Var $ "o" ++ show oname, ty, Port nm n)
                                                            | (oname, (nm, ty)) <- zip [0..] outs ]
                                Nothing -> error $ "mkSink failed on " ++ show n

                  prefix = fst $ splitWith '_' $ last $ fromJust $ lookup n probes

                  subtree = bfs' (\c -> (c == n) || (not (c `elem` probefamily))) [n] rc

                  probefamily = [ node | (node, pnames) <- probes, pname <- pnames, prefix `isPrefixOf` pname ]

                  subGraph = filter ((`elem` subtree) . fst) $ theCircuit rc

                  leafNodes = [ (node, entity) | node <- probefamily, node /= n, Just entity <- [lookup node subGraph] ]

                  newLeaves = [ (ch,Entity (Name "probe" ("i" ++ show i)) outs nins attrs)
                              | (i, (ch, Entity _ outs ins attrs)) <- zip [0..] $ leafNodes
                              , let nins = [ (Var $ "i" ++ show x, ty, Pad $ Var $ "i" ++ show i) | (x, (_,ty)) <- zip [0..] outs ]
                              ]

                  newGraph = newLeaves ++ (filter (not . (`elem` (map fst newLeaves)) . fst) subGraph)

                  newPads = [ (v,ty) | (_, Entity _ _ ins _) <- newLeaves
                                     , (_, ty, Pad v) <- ins ]

-- Seems like this should also exist in the Prelude
splitWith :: Char -> String -> (String,String)
splitWith c s = go s []
    where go [] acc = (reverse acc,[])
          go (i:inp) acc | i == c = (reverse acc,inp)
                         | otherwise = go inp (i:acc)
 
bfs = bfs' (\_ -> True)
bfs' fn nodes rc  = reverse $ go nodes []
    where go [] acc = acc
          go (n:ns) acc = go ns' acc' where acc' = n:acc
                                            ns' = nub
                                                $ filter (not . (`elem` acc'))
                                                $ ns ++ (case fn n of
                                                            True -> (children n rc)
                                                            False -> [])

sinkNames rc = map (\(_,_,Port _ n) -> n) $ theSinks rc
children name rc = catMaybes $ filter isJust
                   $ map (\(_,_,d) -> case d of
                                        Port _ child -> Just child
                                        _ -> Nothing) ins
    where c = theCircuit rc
          Entity _ _ ins _ = fromJust $ lookup name c

probeValues name rc = [ pv | Just (Entity _ _ _ attrs) <- [lookup name $ theCircuit rc]
                          , ("simValue", val) <- attrs
                          , Just pv@(ProbeValue n v) <- [fromDynamic val]]

testReify :: (Ports a) => String -> a -> IO ()
testReify nm fn = do
    putStrLn $ "Testing " ++ nm ++ " reify"
    putStrLn $ "======="
    debugCircuit [OptimizeReify] fn

testCircuit' :: (Ports a, Probe a, Ports b) => [String] -> String -> a -> (a -> b) -> IO ()
testCircuit' enabled name f apply
    | null enabled || name `elem` enabled = do
        let probed = probe name f

        testReify name probed

        plist <- probeCircuit $ apply probed

        let trace = probesFor name plist

--        putStrLn $ "Truth table for " ++ name
--        putStrLn $ "======="
--        putStrLn $ probesTT trace

        mkInputs name dumpDir numberOfCycles trace

        putStrLn $ "Generating Testbench for " ++ name
        putStrLn $ "======="
        let ropts = [OptimizeReify]
        mkTestbench ropts [] name probed   -- inc optimizations?

        rc <- reifyCircuit ropts probed
        applied_rc <- reifyCircuit ropts $ apply probed

        let sinks = sinkNames rc
            pvs = filter (not . null . snd) $ map ((\x -> (x, probeValues x rc)) . fst) $ theCircuit rc
            subRCs = genRCs names rc
            names = [(n,names) | (n,vs) <- pvs, let names = [ nm | ProbeValue nm _ <- vs ]]

        mapM_ (mkTest ropts plist (dumpDir ++ name ++ "/") names) subRCs

        return ()   
    -- Hack to speed up the generation of our tests
    | otherwise = return ()

mkTest ropts plist base names (n,rc) = do
    let name = fst $ splitWith '_' $ last $ fromJust $ lookup n names

    mod <- netlistCircuit' [] name rc

    let vhdl = render $ genVHDL mod

    (inputs,outputs,sequentials) <- ports' ropts rc
    waves <- genProbes' name rc

    let trace = probesFor name plist

    mkInputs name base numberOfCycles trace
    mkTestbench' name base vhdl (inputs,outputs,sequentials) waves

mkInputs name base count probes = do
    let name_n = name ++ "_0"
    let path = base ++ name ++ "/"

    createDirectoryIfMissing True path

    writeFile (path ++ "Makefile")
        $ unlines
        $ ["run : " ++ name ++ ".input " ++ name_n ++ ".info " ++ name ++ ".do"
          ,"\t@echo \"Simulating...\""
          ,"\t@vsim -c -do " ++ name ++ ".do"
          ,"\t@echo \"10 lines from the info file...\""
          ,"\t@tail " ++ name_n ++ ".info"
          ,"\t@echo \"The same 10 lines from the input file...\""
          ,"\t@tail " ++ name ++ ".input"
          ,"\t@echo \"Ditto for the output file...\""
          ,"\t@tail " ++ name ++ ".output"
          ,"\t@./test.sh"
          ]

    writeFile (path ++ "test.sh")
        $ unlines
        $ ["#!/bin/bash"
          ,"THEDIFF=`diff *.input *.output`"
          ,""
          ,"if [[ -z \"$THEDIFF\" ]]; then"
          ,"    echo \"Input/Output Files Are The Same\""
          ,"else"
          ,"    echo \"Warning: Differences Below:\""
          ,"    echo \"$THEDIFF\""
          ,"fi"
          ]
    system $ "chmod +x " ++ path ++ "test.sh"

    let bits = map (\(_, (ProbeValue _ xs)) -> bitsXStream xs) probes

    writeFile (path ++ name ++ ".input")
        $ unlines
        $ take count
        $ mergeWith (++)
        $ bits

    -- kind of messy I know
    writeFile (path ++ name_n ++ ".info")
        $ unlines
        $ L.zipWith (\n l -> "(" ++ show n ++ ") " ++ l) [0..]
        $ mergeWith (\x y -> x ++ " -> " ++ y)
        $ L.zipWith (\bs vs -> L.zipWith (\v b -> v ++ "/" ++ b) (take count vs) (take count bs)) bits
        $ map (\(_, (ProbeValue _ xs)) -> valsXStream xs) probes

-- surely this exists in the prelude?
mergeWith :: (a -> a -> a) -> [[a]] -> [a]
mergeWith fn probes = go probes []
    where go (bs:ps) []  = go ps bs
          go (bs:ps) acc = go ps $ L.zipWith fn acc bs
          go []      acc = acc


-- Lots of tests for primitives
testSomeTruth:: (Testable a) => Int -> String -> a -> IO ()
testSomeTruth n nm fn = do
    putStrLn $ "Testing " ++ nm ++ " function"
    putStrLn $ "======="
    putStrLn $ showSomeTT n $ truthTable fn 
    
testSome
  :: (Ports a, Testable a1, Examine a) 
  => String -> a -> (Example a -> a1) -> IO ()
testSome nm tst f
  | nm `elem` runTests = do   
    testReify nm tst        
    testSomeTruth numberOfCycles nm $ f (example (examine nm tst))
    dumpBitTrace (dumpDir ++ nm ++ "/") numberOfCycles
    mkTestbench [OptimizeReify] [] nm tst   -- inc optimizations?
  -- Hack to speed up the generation of our tests
  | otherwise = putStrLn $ "Ignoring " ++ show nm

		
