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

import Language.KansasLava.Testing.Probes
import Language.KansasLava.Testing.Codegen

import System.Directory
import System.Environment
import System.IO

import qualified System.Posix.Env as Posix

-- CONFIG --
runTests = [] -- empty list builds every test
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
