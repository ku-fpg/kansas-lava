{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is used to generate a VHDL testbench for a Lava circuit.
module Language.KansasLava.Testing.Bench (mkTestbench) where

import Language.KansasLava hiding (ports)
import Language.KansasLava.Netlist.Utils
import Language.KansasLava.Testing.Probes
import Data.List(mapAccumL,sortBy, elemIndex,find,sort)
import Data.Bits

import System.Random
import System.Environment(getEnvironment)
import System.Directory
import System.FilePath.Posix
import Control.Monad(liftM)

mkTestbench :: String -> FilePath -> Circuit -> IO ()
mkTestbench name path circuit = do
    createDirectoryIfMissing True path
    writeVhdlCircuit ["work.all"] name (path </> name <.> "vhd") circuit

    writeFile (path </> name ++ "_tb.vhd")
            $ entity name ++ architecture name circuit

    writeFile (path </> name <.> "do") $ doscript name circuit

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
        ,"signal input : " ++ portType (inputs ++ outputs) ++ ":= (others => '0');"
        ,"signal output : " ++ portType (inputs ++ outputs) ++ ";"
        ,"begin"
        ,stimulus name inputs outputs
        ,dut name inputs outputs sequentials
        ,"end architecture sim;"]
    where (inputs, outputs, sequentials) = ports circuit

dut :: String -> [(OVar, Type)] -> [(OVar, Type)] -> [(OVar, Type)] -> String
dut name inputs outputs sequentials = unlines $ [
    "dut: entity work." ++ name,
    "port map ("] ++
    ["\t" ++ c ++ " => clk," | (OVar _ c,_) <- sequentials] ++
    (let xs = portAssigns inputs outputs in (init xs) ++ [init (last xs)]) ++
    [");"]

-- TODO: add clock speed argument
stimulus :: String -> [(a, Type)] -> [(a, Type)] -> String
stimulus name inputs outputs = unlines $ [
  "runtest: process  is",
  "\tFILE " ++ inputfile ++  " : TEXT open read_mode IS \"" ++ name ++ ".shallow\";",
  "\tFILE " ++ outputfile ++ " : TEXT open write_mode IS \"" ++ name ++ ".deep\";",
  "\tVARIABLE line_in,line_out  : LINE;",
  "\tvariable input_var : " ++ portType (inputs ++ outputs) ++ ";",
  "\tvariable output_var : " ++ portType (inputs ++ outputs) ++ ";",

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
  "\t\toutput(" ++ outputRange ++ ") <= input_var(" ++ outputRange ++ ");",
  pause 4,
  "\t\tclk <= '0';",
  pause 4,
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
	pause n    = "\t\twait for " ++ (show (n * clockSpeed `div` 10)) ++ " ns;"
	outputRange = show (portLen (inputs ++ outputs) - 1) ++ " downto " ++ show (portLen outputs)

-- Manipulating ports
ports :: Circuit -> ([(OVar, Type)],[(OVar, Type)],[(OVar, Type)])
ports reified = (sort inputs, sort outputs, sort clocks)
    where inputs  = [(nm,ty) | (nm,ty) <- theSrcs reified, not (ty `elem` [ClkTy])]
          outputs = [(nm,ty) | (nm,ty,_) <- theSinks reified]
          clocks  = [(nm,ClkTy) | (nm,ClkTy) <- theSrcs reified]
--      resets = [(nm,RstTy) | (nm,RstTy) <- theSrcs reified]

portType :: [(a, Type)] -> [Char]
portType pts = "std_logic_vector(" ++ show (portLen pts - 1) ++ " downto 0)"

portLen :: [(a, Type)] -> Int
portLen pts = sum (map (typeWidth .snd) pts)

portAssigns :: [(OVar, Type)]-> [(OVar, Type)] -> [String]
portAssigns inputs outputs = imap ++ omap
  where assign sig idx (B,n,1) =
          (idx + 1, "\t" ++ n ++ " => " ++ sig ++ "(" ++ show idx ++ "),")
        assign sig idx (ty,n,k) =
          (idx + k, "\t" ++ n ++ " => " ++ sig ++ "(" ++ show (idx + k - 1) ++" downto " ++ show idx ++ "),")
        (_,imap) = mapAccumL (assign "input") (portLen outputs) $ reverse [(ty,n,typeWidth ty) | (OVar _ n,ty) <- inputs]
        (_,omap) = mapAccumL (assign "output") 0 $ reverse [(ty,n,typeWidth ty) | (OVar _ n,ty) <- outputs]

-- Modelsim 'do' script
doscript :: String -> Circuit -> String
doscript name circuit = unlines $
        ["vlib " ++ workDir
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
