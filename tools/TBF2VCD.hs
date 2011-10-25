module Main where

import Language.KansasLava.Trace
import Language.KansasLava.VCD
import Control.Applicative
import Data.Char as C
import System.Environment

main :: IO ()
main = do
	cmds <- getArgs
	main2 cmds

main2 :: [String] -> IO ()
main2 ["--diff",sig,inbits,outbits] = main4 sig inbits outbits
main2 ["--clock",clk,sig,bits'] | all C.isDigit clk
	= main3 True (read clk) sig bits'
main2 [clk,sig,bits'] | all C.isDigit clk
	= main3 False (read clk) sig bits'
main2 _ = error $ "usage:\n   tbf2vcd [--clock] (clockrate-in-ns) <.sig-file> <.bits-file>\n" ++
                          "   tbf2vcd --diff <.sig-file> <.left-bits-file> <.right-bits-file>"

main3 :: Bool -> Integer -> FilePath -> FilePath -> IO ()
main3 ifClk clkRate sigName bitsName = do
	sig <- read <$> readFile sigName
	str <- lines <$> readFile bitsName
	putStrLn $ toVCD ifClk clkRate $ readTBF str sig

main4 :: FilePath -> FilePath -> FilePath -> IO ()
main4 sigfile leftfile rightfile = do
    left  <- lines <$> readFile leftfile
    right <- lines <$> readFile rightfile
    sig   <- read  <$> readFile sigfile

    let t1 = readTBF left  sig
        t2 = readTBF right sig

    writeFile "diff.vcd" $ vcdDiff t1 t2

vcdDiff :: VCD -> VCD -> String
vcdDiff (VCD i1 o1 p1) (VCD i2 o2 p2) = toVCD False 10 t
    where t = VCD (mergeMaps i1 i2) (mergeMaps o1 o2) (mergeMaps p1 p2)
          mergeMaps m1 m2 = [ ("trace1_" ++ k,v) | (k,v) <- m1 ]
                            ++
                            [ ("trace2_" ++ k,v) | (k,v) <- m2 ]

