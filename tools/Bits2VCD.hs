module Main where

import Language.KansasLava
import Data.List as L
import Data.Char as C
import System.Environment

-- parse a line of values, into a 
parseline :: [Type] -> String -> [[Char]]
parseline []      [] = []
parseline (B:ns)   (x:xs) = [ toVCDBit x ] : parseline ns xs
parseline (S n:ns) xs     = ( "b" ++ map toVCDBit (take n xs) ++ " ") : parseline ns (drop n xs)
parseline (U n:ns) xs     = ( "b" ++ map toVCDBit (take n xs) ++ " ") : parseline ns (drop n xs)
parseline (V n:ns) xs     = ( "b" ++ map toVCDBit (take n xs) ++ " ") : parseline ns (drop n xs)
parseline (TupleTy ts:ns) 
		   xs     = parseline (reverse ts ++ ns) xs
parseline (MatrixTy {}:_) _ = error "Matrix not yet supported"
parseline _ _ = error "parseline"
	
toVCDBit :: Char -> Char
toVCDBit '0' = '0'
toVCDBit '1' = '1'
toVCDBit 'X' = 'x'
toVCDBit 'U' = 'x'	-- undefined is unknown in VCD
toVCDBit c = error $ "strange bit in bits : " ++ show c

parsesig :: [(OVar, Type)] -> [t] -> [(Int, t, String)]
parsesig ((nm,B):tys)   (x:xs) = (1,x,show nm) : parsesig tys xs
parsesig ((nm,S n):tys) (x:xs) = (n,x,show nm) : parsesig tys xs
parsesig ((nm,U n):tys) (x:xs) = (n,x,show nm) : parsesig tys xs
parsesig ((nm,V n):tys) (x:xs) = (n,x,show nm) : parsesig tys xs
parsesig ((OVar i nm,TupleTy ts):tys) xs = 
	parsesig ([ (OVar i (nm ++ "_" ++ show (x::Int)),t)
		  | (t,x) <- zip (reverse ts) [0..]
		  ] ++ tys) xs
parsesig ((nm,ty):_) _ = error $ show ("parsesig",nm,ty)
parsesig [] _ = []

bitFileToVCD :: Bool -> Integer -> Signature -> String -> String
bitFileToVCD ifClk clk sig file = header ++ content ++ "$end\n"
  where
	header =
		"$version\n   Kansas Lava\n$end\n" ++
		"$timescale 1ns $end\n" ++
		unlines [ "$var wire " ++ show w ++ " " ++ c ++ " " ++ show nm ++ " $end"
			| (w,c,nm) <- (1,clk_code,"clk")
			 	    : parsesig (sigInputs sig ++ sigOutputs sig) (L.tail identCodes)
			] ++
		"$enddefinitions $end\n" ++
		"$dumpvars\n"

	content =
		concat $
		map groupEvent $ 
		groupBy eq $ 
		sortBy cmp $ 
		concat $ 
		front withTimes

	groupEvent (xs@((tm,_,_):_)) = unlines $
		("#" ++ show tm) : 
		[ b ++ c | (_,b,c) <- xs ]	
	groupEvent [] = error "groupEvent []"

	eq (c0,_,_) (c1,_,_) = c0 == c1
	cmp (c0,_,_) (c1,_,_) = c0 `compare` c1

	-- TODO: remove the -1
	clk1     :: [Integer]
	clk1 	 = -1 : iterate (+ clk) 0

	inWidth = sum (map typeWidth (map snd (sigInputs sig)))
	delta   = (+ ((clk * 2) `div` 3))

	inWires = L.transpose $ map (parseline (map snd (sigInputs sig))) $ map (take inWidth) $ take 2000 $ lines file
	outWires = L.transpose $ map (parseline (map snd (sigOutputs sig))) $ map (drop inWidth) $ take 2000 $ lines file

	clk_code = L.head identCodes

	withTimes :: [[(Integer,String)]]
	withTimes = 
		  [ concat
		     [ [ (n,"1"),(n + clk `div` 2, "0") ]
		     | n <- take (length (L.head inWires)) $ map (*clk) [0..]
		     , ifClk
		     ] 
		  ] ++
		map (findTimes clk1)             (start inWires) ++
		map (findTimes (map delta clk1)) (start outWires)
		
	start xs = map ("?" :) xs

	findTimes :: [Integer] -> [String] -> [(Integer,String)]
	findTimes (_:c1:cs) (x0:x1:xs) 
		| x0 /= x1   = (c1,x1) : findTimes (c1:cs) (x1:xs)
		| otherwise  =           findTimes (c1:cs) (x1:xs)
	findTimes _ _ = []

	front xss = [ [ (i,s,n) | (i,s) <- xs ] | (xs,n) <- zip xss identCodes ]

-- From vcd package, by Tom Hawkins <tomahawkins@gmail.com>
identCodes :: [String]
identCodes = map code [0..]
  where
  code :: Int -> String
  code i | i < 94 =           [chr (33 + mod i 94)] 
  code i = code (div i 94) ++ [chr (33 + mod i 94)] 


main :: IO ()
main = do
	cmds <- getArgs
	main2 cmds
	
main2 :: [String] -> IO ()
main2 ["--clock",clk,sig,bits] | all C.isDigit clk
	= main3 True (read clk) sig bits
main2 [clk,sig,bits] | all C.isDigit clk
	= main3 False (read clk) sig bits
main2 _ = error $ "usage bits2vcd: [--clock] (clockrate-in-ns) <.sig-file> <.bits-file>"
	
main3 :: Bool -> Integer -> String -> String -> IO ()
main3 ifClk clkRate sigName bitsName = do
	sig <- readFile sigName
	str <- readFile bitsName
	putStrLn $ bitFileToVCD ifClk clkRate (read sig) str	

