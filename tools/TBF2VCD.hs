module Main where

import Language.KansasLava.VCD
import Control.Applicative
import Data.Char as C
import System.Environment

main :: IO ()
main = do
	cmds <- getArgs
	main2 cmds

main2 :: [String] -> IO ()
main2 ["--diff",sig,inbits,outbits,vcd_out] = main4 sig inbits outbits vcd_out
main2 ["--clock",clk,sig,bits',vcd_out] | all C.isDigit clk
	= main3 True (read clk) sig bits' vcd_out
main2 [clk,sig,bits',vcd_out] | all C.isDigit clk
	= main3 False (read clk) sig bits' vcd_out
main2 _ = error $ "usage:\n   tbf2vcd [--clock] (clockrate-in-ns) <.sig-file> <.bits-file> <vcd-file>\n" ++
                          "   tbf2vcd --diff <.sig-file> <.left-bits-file> <.right-bits-file> <vcd-file>"

main3 :: Bool -> Integer -> FilePath -> FilePath ->  FilePath -> IO ()
main3 ifClk clkRate sigName bitsName vcdFile = do
	sig <- read <$> readFile sigName
	str <- lines <$> readFile bitsName
	writeVCDFile ifClk clkRate vcdFile $ readTBF str sig

main4 :: FilePath -> FilePath -> FilePath ->  FilePath -> IO ()
main4 sigfile leftfile rightfile vcdFile = do
    left  <- lines <$> readFile leftfile
    right <- lines <$> readFile rightfile
    sig   <- read  <$> readFile sigfile

    let t1 = readTBF left  sig
        t2 = readTBF right sig

    vcdDiff t1 t2 vcdFile

vcdDiff :: VCD -> VCD -> FilePath -> IO ()
vcdDiff (VCD m1) (VCD m2) filePath = writeVCDFile False 10 filePath t
    where t = VCD $ [ ("trace1_" ++ k,v) | (k,v) <- m1 ]
                    ++
                    [ ("trace2_" ++ k,v) | (k,v) <- m2 ]

