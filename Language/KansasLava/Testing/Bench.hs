{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is used to generate a VHDL testbench for a Lava circuit.
module Language.KansasLava.Testing.Bench where
--  (mkTestbench,mkTestbench',testbenchBaseDir, genProbes',ports') where

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

{-
-- | The 'mkTestbench' function will generate a VHDL testbench for a Lava
--   circuit. Given a circuit (with a given name), this will generate a series
--   of support files. The files will place in a directory with the name of the
--   circuit. The directory will be created in the location pointed to in the
--   @LAVA_SIM_PATH@ environment variable, or else in @/tmp@ if that is not
--   defined.
--   'mkTestbench' will create the following files, assuming a circuit @name@ :
--
--   [@name.vhd@] The VHDL entity/architecture pair for the circuit.
--
--   [@name_tb.vhd@] A testbench that instantiates the @name@ entity as the
--   device under test and supplies test stimuli.
--
--   [@name.input@] A list of input stimuli used by the testbench. These are
--   randomly generated stimuli, and may not be appropriate for many circuits.
--
--   [@name.do@] A @modelsim@ script that will compile the vhd and execute the testbench.
mkTestbench :: Ports fun =>
               [CircuitOptions] -- Options for controlling the observable-sharing reification, of dut
            -> [NetlistOption] -- Options for controlling the netlist generation
            -> String -- ^ The name of the function
            -> FilePath -- ^ Base directory
            -> fun    -- ^ The Lava circuit
            -> IO ()
mkTestbench ropts nlopts name base fun = do
  vhdl <- vhdlCircuit {- ropts -} nlopts name ["work.all"] fun
  (inputs,outputs,sequentials) <- ports ropts fun
  waves <- genProbes name fun
    -- TODO: Fix waves!
  mkTestbench' name base vhdl (inputs,outputs,sequentials) [] -- waves

testbenchBaseDir = do
  env <- getEnvironment
  return $ case lookup "LAVA_SIM_PATH" env of
               Nothing -> "/tmp"
               Just dir -> dir

mkTestbench' :: String -- Name of circuit
	         -> FilePath -- Base directory
             -> String -- Generated VHDL
             -> ([(OVar, Type)], [(OVar, Type)], [(OVar, Type)]) -- result of call to ports
             -> [String] -- result of call to genProbes
             -> IO ()
mkTestbench' name dir vhdl (inputs,outputs,sequentials) waves = do
  putStrLn $ "Base directory is " ++ dir
  createDirectoryIfMissing True dir
  writeFile (dir </> name <.> "vhd") vhdl
  writeFile (dir </> name ++ "_tb.vhd") $
            entity name ++ architecture name inputs outputs sequentials
--  stim <- vectors inputs
--  writeFile (dir ++ name ++ ".input") stim
  -- Get the probes
  writeFile (dir </> name <.> "do") (doscript name waves)

entity :: String -> String
entity coreName = unlines
  ["library ieee;",
   "use ieee.std_logic_1164.all;",
   "use ieee.std_logic_textio.all;",
   "library std;",
   "use std.textio.all;",
   "library work;",
   "entity " ++ coreName ++ "_tb is",
   "begin",
   "end entity " ++ coreName ++ "_tb;"
  ]

architecture :: String  -> [(OVar, Type)] -> [(OVar, Type)] -> [(OVar, Type)] -> String
architecture coreName inputs outputs sequentials = unlines $
  ["architecture sim of " ++ coreName ++ "_tb is"] ++
   signals ++
  ["begin",
   stimulus coreName inputs outputs,
   dut coreName inputs outputs sequentials,
   "end architecture sim;"]
  where signals = [
         "signal clk, rst : std_logic;",
         "constant input_size : integer := 16;",
         "constant output_size : integer := 16;",
         "signal input : " ++ portType (inputs ++ outputs) ++ ":= (others => '0');",
         "signal output : " ++ portType (inputs ++ outputs) ++ ";"
          ]

dut :: String -> [(OVar, Type)] -> [(OVar, Type)] -> [(OVar, Type)] -> String
dut coreName inputs outputs sequentials = unlines $ [
 "dut: entity work." ++ coreName,
 "port map ("] ++
 ["\t" ++ c ++ " => clk," | (OVar _ c,_) <- sequentials] ++
 (let xs = portAssigns inputs outputs in (init xs) ++ [init (last xs)]) ++
 [");"]

-- TODO: add clock speed argument
stimulus :: String -> [(a, Type)] -> [(a, Type)] -> String
stimulus coreName inputs outputs = unlines $ [
  "runtest: process  is",
  "\tFILE " ++ inputfile ++  " : TEXT open read_mode IS \"" ++ coreName ++ ".input\";",
  "\tFILE " ++ outputfile ++ " : TEXT open write_mode IS \"" ++ coreName ++ ".output\";",
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
  where inputfile = coreName ++ "_input"
        outputfile = coreName ++ "_output"
	clockSpeed = 50 -- ns
	pause n    = "\t\twait for " ++ (show (n * clockSpeed `div` 10)) ++ " ns;"
	outputRange = show (portLen (inputs ++ outputs) - 1) ++ " downto " ++ show (portLen outputs)

-- Manipulating ports
ports :: (Ports a) =>
         [CircuitOptions] -> a -> IO ([(OVar, Type)],[(OVar, Type)],[(OVar, Type)])
ports ropts fun = do
  reified <- reifyCircuit ropts fun
  ports' ropts reified

ports' :: [CircuitOptions] -> Circuit -> IO ([(OVar, Type)],[(OVar, Type)],[(OVar, Type)])
ports' ropts reified = do
  let inputs = [(nm,ty) | (nm,ty) <- theSrcs reified, not (ty `elem` [ClkTy])]
      outputs = [(nm,ty) | (nm,ty,_) <- theSinks reified]
      clocks = [(nm,ClkTy) | (nm,ClkTy) <- theSrcs reified]
--      resets = [(nm,RstTy) | (nm,RstTy) <- theSrcs reified]
  return (sort inputs,{-sortPorts (findOutputs ropts)-} sort outputs,sort clocks) -- zip clocks resets)

-- sortInputs
sortPorts names ports = sortBy comp ports
  where comp ( a, aTy) ( b, bTy) =
            case (elemIndex a names, elemIndex b names) of
              (Just x, Just y) -> compare x y
              (Just x, Nothing) -> LT
              (Nothing, Just y) -> GT
              (Nothing,Nothing) -> case (a,b) of
                                     ('i':as, 'i':bs) -> compare (read as :: Int) (read bs)
                                     ('o':as, 'o':bs) -> compare (read as :: Int) (read bs)	-- HACK?
                                     _ -> error $ "sortInputs:" ++ show (a,b,names,ports)

{-
findInputs opts = maybe [] names (find isInp opts)
  where isInp (InputNames names) = True
        isInp _ = False
        names (InputNames names) = names
findOutputs opts = maybe [] names (find isOut opts)
  where isOut (OutputNames names) = True
        isOut _ = False
        names (OutputNames names) = names
-}

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
doscript :: String -> [String] -> String
doscript coreName waves =
    let workDir = "mywork"
    in
      unlines $ ["vlib " ++ workDir,
                 "vcom -work " ++ workDir ++ " " ++ coreName ++ ".vhd",
                 "vcom -work " ++ workDir ++ " " ++ coreName ++ "_tb.vhd",
                 "vsim -lib "  ++ workDir ++ " " ++ coreName ++ "_tb",
                 "add wave -r /*" ]
                  ++ waves
                  ++ ["run -all",
                      "quit"]

-- Generating the test vectors.
toBits :: Bits a => Int -> a -> String
toBits size val = map (\i -> if testBit val i then '1' else '0') downto
  where downto = reverse upto
        upto = [0..size-1]

vectors ::  [(a, Type)] -> IO String
vectors inputs = do
    setStdGen (mkStdGen 0)
    let vector = liftM concat $ mapM stim types
    vals <- sequence (replicate 100 $ vector)
    return $ unlines vals

  where stim B = do (v :: Int) <- randomRIO (0,1)
                    return $ toBits 1 v
        stim (S x) = do let bound = 2^(x-1)
                        (v :: Int) <- randomRIO (-bound,bound - 1)
                        return $ toBits x v
        stim (U x) = do (v :: Int) <- randomRIO (0,2^x- 1)
                        return $ toBits x v
        stim ClkTy = return $ toBits 1 (0 :: Int)
	stim (TupleTy tys) = do
		ss <- mapM stim tys
		return $ concat ss
	stim other = error $"other, unknown type : " ++ show other

        types = map snd inputs

-- Generating probes
genProbes :: Ports a => String -> a -> IO [String]
genProbes top fun = do
    c <- reifyCircuit [] fun
    genProbes' top c

genProbes' :: String -> Circuit -> IO [String]
genProbes' top c = do
    let graph = theCircuit c
    return (concatMap getProbe graph)
  where getProbe (ident, (Entity _ [( v, _)] _ attrs)) =
            ["add wave -label " ++ show name ++ " " ++ sig
            | ProbeValue name _ <- attrs
            , let sig = "/" ++ top ++ "_tb/dut/sig_" ++ show ident ++ "_" ++ v ]
        getProbe _ = []
-}
