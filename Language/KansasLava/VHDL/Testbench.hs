{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is used to generate a VHDL testbench for a Lava circuit.
module Language.KansasLava.VHDL.Testbench
  (mkTestbench) where

import Language.KansasLava hiding (ports)
import Language.KansasLava.VHDL.VCD(ProbeValue(..))
import Language.KansasLava.Netlist(NetlistOption(..))
import Data.List(mapAccumL,sortBy, elemIndex,find)
import Data.Bits

import System.Random
import System.Environment(getEnvironment)
import System.Directory
import Control.Monad(liftM)
import Data.Dynamic(fromDynamic)


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
               [ReifyOptions] -- Options for controlling the observable-sharing reification, of dut
            -> [NetlistOption] -- Options for controlling the netlist generation
            -> String -- ^ The name of the function
            -> fun    -- ^ The Lava circuit
            -> IO ()
mkTestbench ropts nlopts coreName fun = do
  env <- getEnvironment
  let base = case lookup "LAVA_SIM_PATH" env of
               Nothing -> "/tmp/" ++ coreName ++ "/"
               Just dir -> dir ++"/"++coreName ++ "/"
  putStrLn $ "Base directory is " ++ base
  createDirectoryIfMissing True base
  vhdl <- vhdlCircuit ropts nlopts coreName fun
  writeFile (base ++ coreName ++ ".vhd") vhdl
  (inputs,outputs,sequentials) <- ports ropts fun
  writeFile (base ++ coreName ++ "_tb.vhd") $
            entity coreName ++ architecture coreName inputs outputs sequentials
  stim <- vectors inputs
  writeFile (base ++ coreName ++ ".input") stim
  -- Get the probes
  waves <- genProbes coreName fun
  writeFile (base ++ coreName ++ ".do") (doscript coreName waves)



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

architecture :: String  -> [(Var, BaseTy)] -> [(Var, BaseTy)] -> [((Var, t), (Var, t1))] -> String
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
         "signal input : " ++ portType inputs ++ ":= (others => '0');",
         "signal output : " ++ portType outputs ++ ";"
          ]

dut :: String -> [(Var, BaseTy)] -> [(Var, BaseTy)] -> [((Var, t), (Var, t1))] -> String
dut coreName inputs outputs sequentials = unlines $ [
 "dut: entity work." ++ coreName,
 "port map ("] ++
 portAssigns inputs outputs ++
 ["\t" ++ c ++ " => clk, " ++ r ++ "=> rst" | ((Var c,_),(Var r, _)) <- sequentials] ++
 [");"]

stimulus :: String -> [(a, BaseTy)] -> [(a1, BaseTy)] -> String
stimulus coreName inputs outputs = unlines $ [
  "runtest: process  is",
  "\tFILE " ++ inputfile ++  " : TEXT open read_mode IS \"" ++ coreName ++ ".input\";",
  "\tFILE " ++ outputfile ++ " : TEXT open write_mode IS \"" ++ coreName ++ ".output\";",
  "\tVARIABLE line_in,line_out  : LINE;",
  "\tvariable input_var : " ++ portType inputs ++ ";",
  "\tvariable output_var : " ++ portType outputs ++ ";",

  "begin",
  "\tclk <= '0';",
  "wait for 10 ns;",
  "\trst <= '1', '0' after 10 ns;",
  "\tclk <= '1', '0' after 10 ns;",
  "wait for 20 ns;",
  "\twhile not endfile (" ++ inputfile ++ ") loop",
  "\t\tREADLINE(" ++ inputfile ++ ", line_in);",
  "\t\tREAD(line_in,input_var);",
  "\t\tinput <= input_var;",
  "\t\twait for 5 ns;",
  "\t\tclk <= '1';",
  "\t\twait for 10 ns;",
  "\t\tclk <= '0';",
  "\t\toutput_var := output;",
  "\t\tWRITE(line_out, output_var);",
  "\t\tWRITELINE(" ++ outputfile ++ ", line_out);",
  "\t\twait for 5 ns;",
  "\tend loop;",
  "\twait;",
  "end process;"
                ]
  where inputfile = coreName ++ "_input"
        outputfile = coreName ++ "_output"

-- Manipulating ports
ports :: (Ports a) =>
         [ReifyOptions] -> a -> IO ([(Var, BaseTy)],[(Var, BaseTy)],[((Var, BaseTy), (Var, BaseTy))])

ports ropts fun = do
  reified <- reifyCircuit ropts fun
  let inputs = [(nm,ty) | (nm,ty) <- theSrcs reified, not (ty `elem` [ClkTy,RstTy])]
      outputs = [(nm,ty) | (nm,ty,_) <- theSinks reified]
      clocks = [(nm,ClkTy) | (nm,ClkTy) <- theSrcs reified]
      resets = [(nm,RstTy) | (nm,RstTy) <- theSrcs reified]
  return (sortPorts (findInputs ropts) inputs,sortPorts (findOutputs ropts) outputs,zip clocks resets)

-- sortInputs
sortPorts names ports = sortBy comp ports
  where comp (Var a, aTy) (Var b, bTy) =
            case (elemIndex a names, elemIndex b names) of
              (Just x, Just y) -> compare x y
              (Just x, Nothing) -> LT
              (Nothing, Just y) -> GT
              (Nothing,Nothing) -> case (a,b) of
                                     ('i':as, 'i':bs) -> compare (read as :: Int) (read bs)
                                     _ -> error $ "sortInputs" ++ show a ++ show b

findInputs opts = maybe [] names (find isInp opts)
  where isInp (InputNames names) = True
        isInp _ = False
        names (InputNames names) = names
findOutputs opts = maybe [] names (find isOut opts)
  where isOut (OutputNames names) = True
        isOut _ = False
        names (OutputNames names) = names


portType :: [(a, BaseTy)] -> [Char]
portType pts = "std_logic_vector(" ++ show (portLen pts - 1) ++ " downto 0)"
portLen :: [(a, BaseTy)] -> Int
portLen pts = sum (map (baseTypeLength .snd) pts)

portAssigns :: [(Var, BaseTy)]-> [(Var, BaseTy)] -> [String]
portAssigns inputs outputs = imap ++ omap
  where assign sig idx (n,1) =
          (idx + 1, "\t" ++ n ++ " => " ++ sig ++ "(" ++ show idx ++ "),")
        assign sig idx (n,k) =
          (idx + k, "\t" ++ n ++ " => " ++ sig ++ "(" ++ show (idx + k - 1) ++" downto " ++ show idx ++ "),")
        (_,imap) = mapAccumL (assign "input") 0 $ reverse [(n,baseTypeLength ty) | (Var n,ty) <- inputs]
        (_,omap) = mapAccumL (assign "output") 0 $ reverse [(n,baseTypeLength ty) | (Var n,ty) <- outputs]



-- Modelsim 'do' script
doscript :: String -> [String] -> String
doscript coreName waves = unlines $ [
  "vlib work",
  "vcom " ++ coreName ++ ".vhd",
  "vcom " ++ coreName ++ "_tb.vhd",
  "vsim " ++ coreName ++ "_tb"] ++
  waves ++
  ["run -all",
  "quit"]



-- Generating the test vectors.
toBits :: Bits a => Int -> a -> String
toBits size val = map (\i -> if testBit val i then '1' else '0') downto
  where downto = reverse upto
        upto = [0..size-1]

vectors ::  [(a, BaseTy)] -> IO String
vectors inputs = do
    setStdGen (mkStdGen 0)
    let vector = liftM concat $ mapM stim types
    vals <- sequence (replicate 100 $ vector)
    return $ unlines vals

  where stim B = do (v :: Int) <- randomRIO (0,1)
                    return $ toBits 1 v
        stim CB = do (v :: Int) <- randomRIO (0,1)
                     return $ toBits 1 v
        stim (S x) = do let bound = 2^(x-1)
                        (v :: Int) <- randomRIO (-bound,bound - 1)
                        return $ toBits x v
        stim (U x) = do (v :: Int) <- randomRIO (0,2^x- 1)
                        return $ toBits x v
        stim ClkTy = return $ toBits 1 (0 :: Int)
        stim RstTy = return $ toBits 1 (0 :: Int)
        stim T     = error "vectors.stim T not supported"


        types = map snd inputs

-- Generating probes
genProbes :: Ports a => String -> a -> IO [String]
genProbes top fun = do
    c <- reifyCircuit [] fun
    let graph = theCircuit c
    return (concatMap getProbe graph)
  where getProbe (ident, (Entity _ [(Var v, _)] _ attrs) )=
          case lookup "simValue" attrs of
            Just dyn -> case fromDynamic dyn of
                          Just (ProbeValue name _) ->
                            let sig = "/" ++ top ++ "_tb/dut/sig_" ++ show ident ++ "_" ++ v
                            in ["add wave -label " ++ show name ++ " " ++ sig]
            Nothing -> []
        getProbe _ = []

