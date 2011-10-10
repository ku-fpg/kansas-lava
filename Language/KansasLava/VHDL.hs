{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}

-- | This module converts a Lava circuit to a synthesizable VHDL netlist.
module Language.KansasLava.VHDL(writeVhdlCircuit, mkTestbench, toASCII, fromASCII) where

import Data.List(mapAccumL)

import Language.KansasLava.Fabric
import Language.KansasLava.Trace
import Language.KansasLava.Types
import Language.KansasLava.Netlist.Utils(toStdLogicExpr,toStdLogicTy, isMatrixStdLogicTy, sizedRange)
import Language.KansasLava.Netlist.Decl
import Language.KansasLava.Netlist.Inst

import Language.Netlist.AST

import Language.Netlist.GenVHDL

import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import Data.Char
import Data.Reify(Unique)


-- | The 'vhdlCircuit' function converts a Lava KLEG into a VHDL entity/architecture pair.
writeVhdlCircuit :: String -> FilePath -> KLEG -> IO ()
writeVhdlCircuit nm file cir = do
	nlMod <- netlistCircuit nm cir
	writeFile file (genVHDL nlMod mods)
    where
        -- we always use the following 'use' statements.
        mods = ["work.lava.all","work.all"]

-- | Make a VHDL testbench from a 'Fabric' and its inputs.
mkTestbench :: FilePath                 -- ^ Directory where we should place testbench files. Will be created if it doesn't exist.
            -> Int                      -- ^ Generate inputs for this many cycles.
            -> (KLEG -> IO KLEG)  -- ^ any operations on the circuit before VHDL generation
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
    writeFile (path </> name <.> "kleg") $ show rc

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
          addToMap sigs m = [ (k,TraceStream ty $ map unASCII strm)
                            | (strm,(k,TraceStream ty _)) <- zip sigs m
                            ]
          (ins, outs) = (addToMap inSigs $ inputs et, addToMap outSigs $ outputs et)
          -- this needs to do the inverse of what asciiStrings does below
          unASCII :: String -> RepValue
          unASCII vals = RepValue [ case v of
                                        'X' -> Nothing
                                        '1' -> Just True
                                        '0' -> Just False
                                        _   -> error "fromASCII: bad character!"
                                  | v <- reverse vals ]

-- | Generate a human readable format for a trace.
toInfo :: Trace -> String
toInfo t = unwords names ++ "\n"
         ++ unlines [ "(" ++ show i ++ ") " ++ l
                   | (i::Int,l) <- zip [1..] lines' ]
    where lines' = mergeWith (\ x y -> x ++ " " ++ y) $ asciiStrings t
          names  = case t of (Trace _ ins outs _) -> map show (map fst (ins ++ outs))

-- | Convert a Trace into a list of lists of Strings, each String is a value,
-- each list of Strings is a signal.
asciiStrings :: Trace -> [[String]]
asciiStrings (Trace c ins outs _) = [ map showRep $ takeMaybe c s
                                    | (_,TraceStream _ s) <- ins ++ outs ]
  where showRep (RepValue vals) = [ case v of
                                      Nothing   -> 'X'
                                      Just True  -> '1'
                                      Just False -> '0'
                                    | v <- reverse vals
                                  ]
-- Note the reverse here is crucial due to way vhdl indexes stuff

-- surely this exists in the prelude?
mergeWith :: (a -> a -> a) -> [[a]] -> [a]
mergeWith _ [] = []
mergeWith f ls = foldr1 (Prelude.zipWith f) ls

splitLists :: [[a]] -> [Int] -> [[[a]]]
splitLists xs (i:is) = map (take i) xs : splitLists (map (drop i) xs) is
splitLists _  []     = [[]]

writeTestbench :: String -> FilePath -> KLEG -> IO ()
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

architecture :: String -> KLEG -> String
architecture name circuit = unlines $
        ["architecture sim of " ++ name ++ "_tb is"
        ,"signal clk : std_logic := '1';"
        ,"signal rst : std_logic := '0';"
        ,"constant input_size : integer := 16;"
        ,"constant output_size : integer := 16;"
        ,"signal input : " ++ portType (ins ++ outs) ++ ":= (others => '0');"
        ,"signal output : " ++ portType (ins ++ outs) ++ ";"
        ,"begin"
        ,stimulus name ins outs
        ,dut name ins outs sequentials
        ,"end architecture sim;"]
    where (ins, outs, sequentials) = ports circuit

dut :: String -> [(String, Type)] -> [(String, Type)] -> [(String, Type)] -> String
dut name ins outs sequentials = unlines $ [
    "dut: entity work." ++ name,
    "port map ("] ++
    ["\t" ++ c ++ " => " ++ case c of
				"clk_en" -> "'1',"
				"clk"    -> "clk,"
				"rst"    -> "rst,"
                                n -> n
	 	| (c,_) <- sequentials] ++
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
  "\tvariable needs_rst : boolean := false;",

  "begin",

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
ports :: KLEG -> ([(String, Type)],[(String, Type)],[(String, Type)])
ports reified = (ins, outs, clocks)
    where ins  = [(nm,ty) | (nm,ty) <- theSrcs reified, nm `notElem` ["clk","rst","clk_en"]]
          outs = [(nm,ty) | (nm,ty,_) <- theSinks reified]
          clocks  = [(nm,ty) | (nm,ty) <- theSrcs reified, nm `elem` ["clk","rst","clk_en"]]
--      resets = [(nm,RstTy) | (nm,RstTy) <- theSrcs reified]

portType :: [(a, Type)] -> [Char]
portType pts = "std_logic_vector(" ++ show (portLen pts - 1) ++ " downto 0)"

portLen :: [(a, Type)] -> Int
portLen pts = sum (map (typeWidth .snd) pts)

portAssigns :: [(String, Type)]-> [(String, Type)] -> [String]
portAssigns ins outs = imap ++ omap
  where assign sig idx (B,n,1) =
          (idx + 1, "\t" ++ n ++ " => " ++ sig ++ "(" ++ show idx ++ "),")
        assign sig idx (_,n,k) =
          (idx + k, "\t" ++ n ++ " => " ++ sig ++ "(" ++ show (idx + k - 1) ++" downto " ++ show idx ++ "),")
        (_,imap) = mapAccumL (assign "input") (portLen outs) $ reverse [(ty,n,typeWidth ty) | (n,ty) <- ins]
        (_,omap) = mapAccumL (assign "output") 0 $ reverse [(ty,n,typeWidth ty) | (n,ty) <- outs]

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


----------------------------------------------------------------------------------

-- | The 'netlistCircuit' function converts a Lava circuit into a Netlist AST
--   The circuit type must implement the 'Ports' class.  If the circuit type is
--   a function, the function arguments will be exposed as input ports, and the
--   result will be exposed as an output port (or ports, if it is a compound
--   type).
netlistCircuit :: String         -- ^ The name of the generated entity.
               -> KLEG 	 -- ^ The Lava circuit.
               -> IO Module
netlistCircuit name circ = do
  let (KLEG nodes srcs sinks) = preprocessNetlistCircuit circ

  let inports = checkPortType srcs
  let outports = checkPortType (map outputNameAndType sinks)

  -- Finals are the assignments from the output signals for entities to the output ports
  let finals = [ NetAssign n (toStdLogicExpr ty x) | (n,ty,x) <- sinks
                                                   , case toStdLogicTy ty of
                                                        MatrixTy {} -> error "can not have a matrix as an out argument"
                                                        _ -> True
               ]

  return $ Module name inports outports []
	   (concatMap genDecl nodes ++
	    concatMap (uncurry (genInst' (M.fromList nodes))) nodes ++
	    finals)


  where checkPortType ports' =  [ (nm,sizedRange ty) | (nm, ty) <- ports'
                               , not (isMatrixStdLogicTy ty) || error "can not have a matrix as a port"
                               ]
        outputNameAndType (n,ty,_) = (n,ty)



-- | This gets a circuit ready for Netlist generation.
-- Specifically, it normalizes all the arguments
-- because arguments that are of type MatrixTy are now supported.
-- 'netlistCircuit' calls 'preprocessNetlistCircuit' before generating 'Module'.
preprocessNetlistCircuit :: KLEG -> KLEG
preprocessNetlistCircuit cir = res
    where
        KLEG nodes srcs sinks = cir
        res = KLEG nodes' srcs' sinks'

        vars = allocEntities cir

        (sinkVars,srcVars) = splitAt (length sinks) vars

        nodes'  = map fixUp nodes ++ nodesIn ++ nodesOut

        -- figure out the list of srcs
        srcs'   =  [ (nm ++ extra1, ty2)
                   | (nm, ty) <- srcs
                         , (extra1,ty2)
                                <- case toStdLogicTy ty of
                                     B    -> [("",ty)]
                                     V _  -> [("",ty)]
                                     MatrixTy n (V _)
                                          -> let (MatrixTy _ inner) = ty
                                             in reverse [("_x" ++ show j,inner) | j <- [0..(n-1)]]
                                     other -> error $ show ("srcs",other)
--                   | k <- [0..] -- This gives them better sorting numbers
                   ]


        extras0 :: [(String,Entity Unique)]
        extras0  = [ (nm, Entity (Prim "concat")
                              [("o0",ty)]
                              [ ( 'i':show j
                                , case ty of
                                   MatrixTy _ inner -> inner
                                   _ -> error $ "preprocessVhdlCircuit: not a matrix type " ++ show ty
                                , case [ nm'
                                         | (nm',_) <- srcs'
                                         , nm' == (nm ++ "_x" ++ show j)
                                         ] of
                                      [] -> error ("could not find " ++ show nm)
                                      [x] -> Pad x
                                      _ -> error ("too many of " ++ show nm)
                                )
                              | j <- [0..(getMatrixNumColumns ty - 1)]]
                     )
                  | (nm, ty) <- srcs
                  , isMatrixStdLogicTy ty
                  ]

        getMatrixNumColumns (MatrixTy c _) = c
        getMatrixNumColumns _ = error "Can't get number of columns for non-matrix type"

        extras1 :: [(Unique, (String, Entity Unique))]
        extras1 = zip srcVars extras0

        nodesIn :: [(Unique, Entity Unique)]
        nodesIn = [ (u,e) | (u,(_,e)) <- extras1 ]

        --------------------------------------------------------------------------------------------

        sinks'  = [ (nm ++ extra1, ty2, dr2)
                  | (u,(nm, ty, dr)) <- zip sinkVars (sinks)
                         , (extra1,ty2,dr2)
                                <- case toStdLogicTy ty of
                                     B    -> [("",ty,dr)]
                                     V _  -> [("",ty,dr)]
                                     MatrixTy n (V _)
                                          -> let (MatrixTy _ inner) = ty
                                             in reverse [ ("_x" ++ show j,inner,Port ('o':show j) u) | j <- [0..(n-1)]]
                                     other -> error $ show ("sinks",other)
--                  | k <- [0..] -- This gives them better sorting numbers
                  ]


        nodesOut :: [(Unique, Entity Unique)]
        nodesOut = [  (u,Entity (Prim "unconcat")
                                [('o':show j,innerTy) | j <- [0..(n-1)]]
                                [("i0",ty,dr)])
                   | (u,(_, ty, dr)) <- zip sinkVars (sinks)
                   , (innerTy,n )
                        <- case toStdLogicTy ty of
                             B    -> []
                             V _  -> []
                             MatrixTy n (V _)
                                  -> let (MatrixTy _ inner) = ty
                                     in [ (inner,n) ]
                             other -> error $ show ("nodesOut",other)
                   ]

        --------------------------------------------------------------------------------------------

{- ACF: Here is original, before removing OVars, because I'm not sure I'm doing this right.
        fixUp :: (Unique,Entity Unique) -> (Unique, Entity Unique)
        fixUp (i,Entity e ins outs) = (i,
                Entity e ins
                         [ (o,t,case d of
                                 Pad o2@(OVar _ nm)
                                     -> case [ u | (u,(o3,_)) <- extras1, o2 == o3 ] of
                                             [u] -> Port "o0" u
                                             []  -> case [ j | (OVar j nm',_) <- srcs', nm == nm' ] of
                                                      [k] -> Pad (OVar k nm)
                                                      _ -> error "fixUp find"
                                             _ -> error "fixUp"
                                 other -> other
                                 ) | (o,t,d) <- outs ])
-}

        fixUp :: (Unique,Entity Unique) -> (Unique, Entity Unique)
        fixUp (i,Entity e ins outs) = (i,
                Entity e ins
                         [ (o,t,case d of
                                 Pad nm
                                     -> case [ u | (u,(o3,_)) <- extras1, nm == o3 ] of
                                             [u] -> Port "o0" u
                                             []  -> case [ nm' | (nm',_) <- srcs', nm == dropWhile isDigit nm' ] of
                                                      [nm'] -> Pad nm'
                                                      _ -> error "fixUp find"
                                             _ -> error "fixUp"
                                 other -> other
                                 ) | (o,t,d) <- outs ])

takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe = maybe id take
