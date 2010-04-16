{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts, Rank2Types, ExistentialQuantification, TypeFamilies #-}

import Language.KansasLava

import System.IO.Unsafe

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Dynamic

import Data.Bits
import Data.List as L

import Data.Sized.Arith as A
import Data.Sized.Matrix as M
import Data.Sized.Sampled as S

import Data.Sized.Signed as S
import Data.Sized.Unsigned as U

import Language.KansasLava.Probes
import Language.KansasLava.VHDL.Testbench

import System.Environment
import System.Directory
import System.Cmd
import System.IO

import qualified System.Posix.Env as Posix

-- CONFIG --
runTests = [] -- 'muxX"] -- empty list builds every test

numberOfCycles :: Int
numberOfCycles = 100

dumpDir = "examine/"
-- END CONFIG --

-- TESTS --
main = do
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

    testCircuit "boolPrims2X"
        ((\ a b -> pack (matrix [a .==. b, a .>=. b, a .<=. b, a .>. b, a .<. b] :: Matrix X5 (Seq Bool))) :: Seq U4 -> Seq U4 -> Seq (Matrix X5 Bool))
        (\ f -> f inp inp3)

    testSome "boolPrims2Signed"
	((\ a b -> pack (matrix [a .==. b, a .>=. b, a .<=. b, a .>. b, a .<. b] :: Matrix X5 (Seq Bool))) :: Seq S5 -> Seq S5 -> Seq (Matrix X5 Bool))
	(\ f -> f .*. sinp .*. sinp3)

    testSome "boolPrims2FLOAT"
	((\ a b -> pack (matrix [a .==. b, a .>=. b, a .<=. b, a .>. b, a .<. b] :: Matrix X5 (Seq Bool))) :: Seq FLOAT -> Seq FLOAT -> Seq (Matrix X5 Bool))
	(\ f -> f .*. (toSeq [-32,-31..31] :: Seq (Sampled X32 X32))
		  .*. (toSeq (reverse ([-32,-31..31] ++ [31])) :: Seq (Sampled X32 X32)))


{-  This doesn't have a deep embedding defined, and takes an Int, which requires an instance of Examine (Int -> Seq Bool)
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

testCircuit :: (Ports a, Probe a, Ports b) => String -> a -> (a -> b) -> IO ()
testCircuit nm tst f
    | null runTests || nm `elem` runTests = do
        plist <- probeCircuit $ f $ probe nm tst
        mkTest nm 0 numberOfCycles
		$ sortBy (\(n,_) (n2,_) -> compare n n2)
                $ filter (\(_, (ProbeValue name _)) -> nm `isPrefixOf` name) plist
        mkTestbench [OptimizeReify] [] nm tst   -- inc optimizations?

        return ()   
    -- Hack to speed up the generation of our tests
    | otherwise = putStrLn $ "Ignoring " ++ show nm

-- bitsXStream creates a list of binary representations of the values in the stream.
bitsXStream :: forall a. RepWire a => XStream a -> [String]
bitsXStream (XStream strm) = showSeqBits ((shallowSeq strm) :: Seq a)

-- valsXStream creates a list of string representations of the values in the stream.
valsXStream :: forall a. RepWire a => XStream a -> [String]
valsXStream (XStream strm) = showSeqVals ((shallowSeq strm) :: Seq a)

mkTest nm n count probes = do
    let nm_n = nm ++ "_" ++ show n
    let path = dumpDir ++ nm ++ "/"

    createDirectoryIfMissing True path

    writeFile (path ++ "Makefile")
        $ unlines
        $ ["run : " ++ nm ++ ".input " ++ nm_n ++ ".info " ++ nm ++ ".do"
          ,"\t@echo \"Simulating...\""
          ,"\t@vsim -c -do " ++ nm ++ ".do"
          ,"\t@echo \"10 lines from the info file...\""
          ,"\t@tail " ++ nm_n ++ ".info"
          ,"\t@echo \"The same 10 lines from the input file...\""
          ,"\t@tail " ++ nm ++ ".input"
          ,"\t@echo \"Ditto for the output file...\""
          ,"\t@tail " ++ nm ++ ".output"
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

    writeFile (path ++ nm ++ ".input")
        $ unlines
        $ take count
        $ mergeWith (++)
        $ bits

    -- kind of messy I know
    writeFile (path ++ nm_n ++ ".info")
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
    
testReify :: (Ports a) => String -> a -> IO ()
testReify nm fn = do
    putStrLn $ "Testing " ++ nm ++ " reify"
    putStrLn $ "======="
    debugCircuit [OptimizeReify] fn

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

		
