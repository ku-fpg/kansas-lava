{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts, DeriveDataTypeable, DataKinds #-}
module Test where

import Language.KansasLava.Fabric
import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.VCD
import Language.KansasLava.VHDL

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Exception

-- found in dist/build/autogen
import Paths_kansas_lava

import GHC.TypeLits

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Data.List as List
import Data.Maybe as Maybe
import Data.Default
--import Data.Sized.Unsigned

import System.Cmd
import System.Console.CmdArgs hiding (Default,def,name,summary,opt)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath as FP
import qualified System.IO.Strict as Strict
import qualified System.Random as R

import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Universal

-------------------------------------------------------------------------------------

data SingleTest = SingleTest String Int (Fabric ()) (Fabric (Int -> Maybe String))

data Tests a = Tests a ([SingleTest] -> [SingleTest])

instance Monad Tests where
        return a = Tests a id
        Tests a xs >>= k = let Tests b ys = k a
                           in Tests b (xs . ys)


test :: String -> Int  -> Fabric () -> (Fabric (Int -> Maybe String)) -> Tests ()
test str i fab tb_fab = Tests () (SingleTest  str i fab tb_fab :)


instance Show SingleTest where
        show (SingleTest str i _ _) = str ++ "(" ++ show i ++ ")"


runShallowTest :: SingleTest -> IO ()
runShallowTest st@(SingleTest name count f_dut f_expected) = do
        createDirectoryIfMissing True ("sims" </> name)
{-
        let inp :: [(String,Pad CLK)]
            Pure (expected_fn,inp) = runFabric f_expected shallow

            shallow :: [(String,Pad CLK)]
            Pure (_,shallow) = runFabric f_dut inp

-}
        let Pure (((),vcd),expected_fn) = runFabricWithDriver (recordVCDFabric count f_dut) f_expected

            expected = expected_fn count

        case expected of
          Just msg -> do
                  putStrLn $ name ++ " failed shallow build"
                  putStrLn $ msg
                  -- do not write the file
          Nothing -> do
                  writeSIG ("sims" </> name </> "dut.sig") vcd
                  writeVCD ("sims" </> name </> "dut.in.vcd") 10 vcd    -- 100MHz
                  writeTBF ("sims" </> name </> "dut.in.tbf") vcd       -- also writes <...>.sig file

runVHDLGeneratorTest :: SingleTest -> IO ()
runVHDLGeneratorTest st@(SingleTest name count f_dut _) = do
        createDirectoryIfMissing True ("sims" </> name)

        rc <- reifyFabric f_dut

        mkTestbench "dut" ("sims" </> name) rc
        copyLavaPrelude ("sims" </> name)

        writeFile ("sims" </> name </> "dut.kleg") $ show rc

        -- Finally, write the VHDL file.
        writeVhdlCircuit "dut" ("sims" </> name </> "dut.vhd") rc
        return ()

compareLines :: Int -> [String] -> [String] -> String
compareLines 1 [] []         = "fail: both shallow and deep are empty"
compareLines n [] []         = "success: " ++ show (n - 1) ++ " cycles match"
compareLines n (xs:xss) (ys:yss)
        | length xs /= length ys = "fail: line " ++ show n ++ " are different widths"
        | all okay (zip xs ys)   = compareLines (succ n) xss yss
        | otherwise              = unlines
                [ "fail: at line " ++ show n
                , "  " ++ xs
                , "  " ++ ys
                , "  " ++ [ case (x,y) of
                              ('0','0') -> ' '
                              ('1','1') -> ' '
                              ('X',_)   -> '-'
                              (_,_)     -> '^'
                          | (x,y) <- zip xs ys
                          ]
                ]
 where
         okay (x,y) | x == y    = True             -- good
                    | x == 'X'  = True             -- fine (model has no expectations)
                    | otherwise = False

compareLines n []     (_:_)  = "fail: shallow finished early"
compareLines n (_:_) []      = "fail: deep finished early (perhaps failed to compile?)"


-- | Convert the inputs and outputs of a VCD to the textual format expected
-- by a testbench. Also write a .sig file, which summarizes the shape of the data.
writeTBF :: String -> VCD -> IO ()
writeTBF filename vcd = do
        writeFile filename
                $ unlines
                $ (\ xs -> if null xs then [] else foldr1 (Prelude.zipWith (++)) xs)
                $ tbfVCD vcd



preludeFile :: String
preludeFile = "Lava.vhd"

copyLavaPrelude :: FilePath -> IO ()
copyLavaPrelude dest = do
  file <- readPreludeFile ("Prelude/VHDL/" </> preludeFile)
  writeFile (dest </> preludeFile) file

-------------------------------------------------------------------------------------

data Gen a = Gen Integer (Integer -> Maybe a)

arbitrary :: forall w . (Rep w) => Gen w
arbitrary = Gen sz integer2rep
  where
        sz = 2 ^ (fromIntegral (repWidth (Witness :: Witness w)) :: Int)
        integer2rep :: Integer -> Maybe w
        integer2rep v = unX
                $ fromRep
                $ RepValue
                $ take (repWidth (Witness :: Witness w))
                $ map Just
                $ map odd
                $ iterate (`div` 2)
                $ (fromIntegral v :: Int)

------------------------------------------------------------------------------------
-- The new testing system.

-- | 'allCases' returns all values of type w, in a non-random order.
allCases :: (Rep w) => [w]
allCases = Maybe.catMaybes $ fmap f [0..(n-1)]
   where (Gen n f) = arbitrary

-- | 'finiteCases' returns finite values, perhaps many times, in a random order.
finiteCases :: (Rep w) => Int ->[w]
finiteCases i = take i $ Maybe.catMaybes $ fmap f $ R.randomRs (0,n-1) (R.mkStdGen 0)
  where (Gen n f) = arbitrary

-------------------------------------------------------------------

-- | Get a file from the prelude. First, check the KANSAS_LAVA_ROOT system
-- environment variable. If it exists, use that. If not, try to get it from the
-- installed cabal package.
readPreludeFile :: String -> IO String
readPreludeFile fname = do
   ks <- getEnv "KANSAS_LAVA_ROOT"
   Strict.readFile (ks </> fname)
 `E.catch` \ (_ :: IOException) -> do
    path <- getDataFileName fname
    Strict.readFile path
 `E.catch` \ (_ :: IOException) -> do
   putStrLn "Set the KANSAS_LAVA_ROOT environment variable"
   putStrLn "to point to the root of the KsLava source directory."
   exitFailure

matchExpected :: (Rep a, SingI (W a), Show a) => String -> Seq a -> Fabric (Int -> Maybe String)
matchExpected out_name ref = do
        o0 <- inStdLogicVector out_name
        let sq = o0 `refinesFrom` ref
        return $ \ count ->
                case [ (i::Int,o,r)
                     | (i,v,o,r) <- take (fromIntegral count)
                                $ zip4 [0..]
                                  (fromS sq)
                                  (S.toList (fmap (show . unRepValue . toRep) (shallowXS o0)))
                                  (S.toList (fmap (show . unRepValue . toRep) (shallowXS ref)))

                     , v /= Just True
                     ] of
                     [] -> Nothing
                     ns -> Just $ "failed on cycles " ++ show (take 20 $ ns)


