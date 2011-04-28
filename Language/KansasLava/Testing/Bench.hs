{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is used to generate a VHDL testbench for a Lava circuit.
module Language.KansasLava.Testing.Bench (mkTestbench, fromASCII) where

import Language.KansasLava
import Language.KansasLava.Netlist(preprocessNetlistCircuit)
import Language.KansasLava.Testing.Trace

import Data.List(mapAccumL,sort)

import System.Directory
import System.FilePath.Posix

-- | Make a VHDL testbench from a 'Fabric' and its inputs.
mkTestbench :: FilePath                 -- ^ Directory where we should place testbench files. Will be created if it doesn't exist.
            -> Int                      -- ^ Generate inputs for this many cycles.
            -> (Circuit -> IO Circuit)  -- ^ any operations on the circuit before VHDL generation
            -> Fabric ()                -- ^ The Fabric for which we are building a testbench.
            -> [(String,Pad)]           -- ^ Inputs to the Fabric
            -> IO Trace
mkTestbench path cycles circuitMod fabric input = do
    let name = last $ splitPath path

    createDirectoryIfMissing True path

    (trace, rc) <- mkTraceCM (return cycles) fabric input circuitMod

    writeFile (path </> name <.> "shallow") $ toASCII trace
    writeFile (path </> name <.> "info") $ toInfo trace
    writeFile (path </> name <.> "sig") $ show $ toSignature trace

    writeTestbench name path rc

    return trace

-- | Convert the inputs and outputs of a Trace to the textual format expected
-- by a testbench.
toASCII :: Trace -> String
toASCII = unlines . mergeWith (++) . asciiStrings

-- | Inverse of toASCII, needs a signature for the shape of the desired Trace.
-- Creates a Trace from testbench signal files.
fromASCII :: [String] -> Signature -> Trace
fromASCII ilines sig = et { inputs = ins, outputs = outs }
    where et = setCycles (length ilines) $ fromSignature sig
          widths = [ typeWidth ty
                   | (_,TraceStream ty _) <- inputs et ++ outputs et
                   ]
          (inSigs, outSigs) = splitAt (length $ inputs et) $ splitLists ilines widths
          addToMap sigs m = [ (k,TraceStream ty (map (read . reverse) strm))
                            | (strm,(k,TraceStream ty _)) <- zip sigs m
                            ]
          (ins, outs) = (addToMap inSigs $ inputs et, addToMap outSigs $ outputs et)

-- | Generate a human readable format for a trace.
toInfo :: Trace -> String
toInfo t = unlines [ "(" ++ show i ++ ") " ++ l
                   | (i::Int,l) <- zip [1..] lines' ]
    where lines' = mergeWith (\ x y -> x ++ " -> " ++ y) $ asciiStrings t

-- | Convert a Trace into a list of lists of Strings, each String is a value,
-- each list of Strings is a signal.
asciiStrings :: Trace -> [[String]]
asciiStrings (Trace c ins outs _) = [ map (reverse . show) $ takeMaybe c s
                                    | (_,TraceStream _ s) <- ins ++ outs ]
-- Note the reverse here is crucial due to way vhdl indexes stuff

-- surely this exists in the prelude?
mergeWith :: (a -> a -> a) -> [[a]] -> [a]
mergeWith _ [] = []
mergeWith f ls = foldr1 (Prelude.zipWith f) ls

splitLists :: [[a]] -> [Int] -> [[[a]]]
splitLists xs (i:is) = map (take i) xs : splitLists (map (drop i) xs) is
splitLists _  []     = [[]]

writeTestbench :: String -> FilePath -> Circuit -> IO ()
writeTestbench name path circuit = do
    createDirectoryIfMissing True path
    writeVhdlCircuit name (path </> name <.> "vhd") circuit

    writeFile (path </> name ++ "_tb.vhd")
            $ entity name ++ architecture name (preprocessNetlistCircuit circuit)

    writeFile (path </> name <.> "do") $ doscript name

entity :: String -> String
entity name = unlines
  ["library ieee;",
   "use ieee.std_logic_1164.all;",
   "use ieee.std_logic_textio.all;",
   "library std;",
   "use std.textio.all;",
   "library work;",
   "entity " ++ name ++ "_tb is",
   "begin",
   "end entity " ++ name ++ "_tb;"
  ]

architecture :: String -> Circuit -> String
architecture name circuit = unlines $
        ["architecture sim of " ++ name ++ "_tb is"
        ,"signal clk, rst : std_logic;"
        ,"constant input_size : integer := 16;"
        ,"constant output_size : integer := 16;"
        ,"signal input : " ++ portType (ins ++ outs) ++ ":= (others => '0');"
        ,"signal output : " ++ portType (ins ++ outs) ++ ";"
        ,"begin"
        ,stimulus name ins outs
        ,dut name ins outs sequentials
        ,"end architecture sim;"]
    where (ins, outs, sequentials) = ports circuit

dut :: String -> [(OVar, Type)] -> [(OVar, Type)] -> [(OVar, Type)] -> String
dut name ins outs sequentials = unlines $ [
    "dut: entity work." ++ name,
    "port map ("] ++
    ["\t" ++ c ++ " => " ++ case c of
				"clk_en" -> "'1',"
				"clk"    -> "clk,"
				"rst"    -> "rst,"
                                n -> n
	 	| (OVar _ c,_) <- sequentials] ++
    (let xs = portAssigns ins outs in (init xs) ++ [init (last xs)]) ++
    [");"]

-- TODO: add clock speed argument
stimulus :: String -> [(a, Type)] -> [(a, Type)] -> String
stimulus name ins outs = unlines $ [
  "runtest: process  is",
  "\tFILE " ++ inputfile ++  " : TEXT open read_mode IS \"" ++ name ++ ".shallow\";",
  "\tFILE " ++ outputfile ++ " : TEXT open write_mode IS \"" ++ name ++ ".deep\";",
  "\tVARIABLE line_in,line_out  : LINE;",
  "\tvariable input_var : " ++ portType (ins ++ outs) ++ ";",
  "\tvariable output_var : " ++ portType (ins ++ outs) ++ ";",
  "\tvariable needs_rst : boolean := true;",

  "begin",

  "\tclk <= '0';",
  pause 5,
  "\tclk <= '1';",
  pause 5,
  "\tclk <= '0';",
  pause 5,	-- 5 cycles
  "wait for 20 ns;",
  "\twhile not endfile (" ++ inputfile ++ ") loop",
  "\t\tREADLINE(" ++ inputfile ++ ", line_in);",
  "\t\tREAD(line_in,input_var);",
	-- clock start
  "\t\tclk <= '1';",
  pause 1,
  "\t\tinput <= input_var;",
  "\t\tif needs_rst then",
  "\t\t\trst <= '1';",
  "\t\tend if;",
  "\t\toutput(" ++ outputRange ++ ") <= input_var(" ++ outputRange ++ ");",
  pause 4,
  "\t\tclk <= '0';",
  pause 4,
  "\t\tif needs_rst then",
  "\t\t\trst <= '0';",
  "\t\t\tneeds_rst := false;",
  "\t\tend if;",
  "\t\toutput_var := output;",
  "\t\tWRITE(line_out, output_var);",
  "\t\tWRITELINE(" ++ outputfile ++ ", line_out);",
  pause 1,
  "\tend loop;",
  "\twait;",
  "end process;"
                ]
  where inputfile = name ++ "_input"
        outputfile = name ++ "_output"
	clockSpeed = 50 -- ns
	pause n    = "\t\twait for " ++ (show (n * clockSpeed `div` (10 ::Int))) ++ " ns;"
	outputRange = show (portLen (ins ++ outs) - 1) ++ " downto " ++ show (portLen outs)

-- Manipulating ports
ports :: Circuit -> ([(OVar, Type)],[(OVar, Type)],[(OVar, Type)])
ports reified = (sort ins, sort outs, sort clocks)
    where ins  = [(nm,ty) | (nm@(OVar _ m),ty) <- theSrcs reified, m `notElem` ["clk","rst","clk_en"]]
          outs = [(nm,ty) | (nm,ty,_) <- theSinks reified]
          clocks  = [(nm,ty) | (nm@(OVar _ m),ty) <- theSrcs reified, m `elem` ["clk","rst","clk_en"]]
--      resets = [(nm,RstTy) | (nm,RstTy) <- theSrcs reified]

portType :: [(a, Type)] -> [Char]
portType pts = "std_logic_vector(" ++ show (portLen pts - 1) ++ " downto 0)"

portLen :: [(a, Type)] -> Int
portLen pts = sum (map (typeWidth .snd) pts)

portAssigns :: [(OVar, Type)]-> [(OVar, Type)] -> [String]
portAssigns ins outs = imap ++ omap
  where assign sig idx (B,n,1) =
          (idx + 1, "\t" ++ n ++ " => " ++ sig ++ "(" ++ show idx ++ "),")
        assign sig idx (_,n,k) =
          (idx + k, "\t" ++ n ++ " => " ++ sig ++ "(" ++ show (idx + k - 1) ++" downto " ++ show idx ++ "),")
        (_,imap) = mapAccumL (assign "input") (portLen outs) $ reverse [(ty,n,typeWidth ty) | (OVar _ n,ty) <- ins]
        (_,omap) = mapAccumL (assign "output") 0 $ reverse [(ty,n,typeWidth ty) | (OVar _ n,ty) <- outs]

-- Modelsim 'do' script
doscript :: String -> String
doscript name = unlines $
        ["vlib " ++ workDir
	,"vcom -work mywork Lava.vhd"
        ,"if [catch {vcom -work " ++ workDir ++ " " ++ name ++ ".vhd} einfo] {"
        ,"    puts $einfo"
        ," } else {"
        ,"    vcom -work " ++ workDir ++ " " ++ name ++ "_tb.vhd"
        ,"    vsim -lib "  ++ workDir ++ " " ++ name ++ "_tb"
        ,"    add wave -r /*"
        ,"    run -all"
        ," }"
        ,"quit"
        ]
    where workDir = "mywork"
--          waves = genProbes name circuit

{-
As of modelsim 6.6, you can't label waves anymore.
The add wave -r /* above puts waves on all signals.

-- Generating probes
genProbes :: String -> Circuit -> [String]
genProbes top c = concatMap getProbe graph
    where graph = theCircuit c
          getProbe (ident, (Entity (TraceVal nms _) [( v, _)] _)) =
            ["add wave -label " ++ show nm ++ " " ++ sig
            | nm <- nms
            , let sig = "/" ++ top ++ "_tb/dut/sig_" ++ show ident ++ "_" ++ v ]
          getProbe _ = []
-}

