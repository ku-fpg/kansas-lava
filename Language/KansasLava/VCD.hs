{-# LANGUAGE RankNTypes, ScopedTypeVariables, DataKinds #-}
-- | This module contains functions for generating VCD debug traces.
-- It also provides functionality for (de)serializing Traces.
module Language.KansasLava.VCD
    ( -- * The main data structure
      VCD
      -- * Writing VCD to file
    , openVCD
    , writeVCD

     -- * Test bench format
    , cmpTBF
    , writeTBF

    -- * Capture a VCD from a fabric
    , recordVCDFabric

    -- * set up probes
    , probesToVCD
    ) where

import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Internal
import Language.KansasLava.Universal

import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Probes

import Control.Monad.Fix
import Data.Char
import Data.List
import qualified Data.Map as M
import Control.Concurrent.MVar
import System.IO.Unsafe
import System.IO
import System.FilePath as FP

-- for testing
-- import Data.Sized.Unsigned
-- import Data.Sized.Fin


scope :: String -> VCD -> [(String,VC)]
scope s (VCD _ m) = [ (nm,ts) | (nm,ts) <- M.assocs m, s `isPrefixOf` nm ]

inputs :: VCD -> [(String,VC)]
inputs = scope "inputs/"

outputs :: VCD -> [(String,VC)]
outputs = scope "outputs/"

type VCDID = String
-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
vcdIds :: [VCDID]
vcdIds = map code [0..]
    where code :: Int -> VCDID
          code i | i < 0 = ""
          code i         = chr (33 + mod i 94) : code (div i 94 - 1)


dumpVars :: [(VCDID, RepValue)] -> String
dumpVars vals = "$dumpvars\n" ++ unlines (map (uncurry vcdVal) vals) ++ "$end\n"


vcdVal :: VCDID -> RepValue -> String
vcdVal i r@(RepValue bs) | length bs == 1 = rep2tbw r ++ i
                         | otherwise      = "b" ++ rep2tbw r ++ " " ++ i

----------------------------------------------------------------------------------------

-- | Convert the inputs and outputs of a VCD to the textual format expected
-- by a testbench. Also write a .sig file, which summarizes the shape of the data.
writeTBF :: String -> VCD -> IO ()
writeTBF filename vcd = do
        writeFile filename $ unlines $ mergeWith (++) $ asciiStrings vcd
        writeFile (filename <.> "sig") $ unlines
                [ k ++ " :: " ++ show ty ++ " [" ++ show (length cs) ++ " event(s)]"
                | (k,VC ty _ cs _) <- inputs vcd ++ outputs vcd
                ]

cmpTBF :: [String] -> [String] -> Maybe Int
cmpTBF master slave = head $
        [ Just i
        | (i,m,s) <- zip3 [0..] master' slave'
        , not (cmpRepValue m s)
        ] ++ [Nothing]
  where
          master' = map tbw2rep master
          slave'  = map tbw2rep slave

-- | Convert a VCD into a list of lists of Strings, each String is a value,
-- each list of Strings is a signal.
asciiStrings :: VCD -> [[String]]
asciiStrings vcd@(VCD 0 m) =
                             [ map rep2tbw (vcSplice vc 0 j)
                             | (_,vc) <- inputs vcd ++ outputs vcd
                             ]
  where
         -- last value to print
         j = maximum [ e | VC _ _ _ e <- M.elems m ]



-- [ E.toList $ fmap rep2tbw s | VC _ s <- M.assoc insOuts ]
--    where insOuts = [ ts | (_,ts) <- inputs vcd ++ outputs vcd ]

-- | Convert string representation used in testbench files to a RepValue
-- Note the reverse here is crucial due to way vhdl indexes stuff
tbw2rep :: String -> RepValue
tbw2rep vals = RepValue [ case v of
                            'X' -> Nothing
                            '1' -> Just True
                            '0' -> Just False
                            'U' -> Nothing
                            other -> error $ "tbw2rep: bad character! " ++ [other]
                        | v <- reverse vals ]

-- | Convert a RepValue to the string representation used in testbench files
rep2tbw :: RepValue -> String
rep2tbw (RepValue vals) = [ case v of
                              Nothing   -> 'X'
                              Just True  -> '1'
                              Just False -> '0'
                          | v <- reverse vals ]

----------------------------------------------------------------------------

{-# NOINLINE probesToVCD #-}
probesToVCD :: Int -> Integer -> String -> IO () -> IO ()
probesToVCD _size speed filename todo = do
        v <- newMVar emptyVCD
        setShallowProbes $ \ nm clkNo x -> unsafePerformIO $ do
                modifyMVar_ v $ \ vcd -> return (snocVCD nm (fromIntegral clkNo) x vcd)
                return x
        todo
        vcd <- takeMVar v
        h <- openVCD speed filename vcd
        writeVCD h vcd
        hClose h
        putStrLn $ "[Written probe values to " ++ show filename ++ "]"
        return ()


data VCD = VCD Int (M.Map String VC)
--        deriving Show


data VC = VC
         { vcType     :: !Type
         , vcInit     :: !RepValue
         , vcChanges  :: ![(Int,RepValue)] -- reversed list of events
         , vcEnd      :: !Int
         }
        deriving Show


vcSplice :: VC -> Int -> Int -> [RepValue]
vcSplice vc from to = loop (vcInit vc) (reverse [ (i,v) | (i,v) <- vcChanges vc]) [from .. to]
  where
          loop _ _                []     = []
          loop (RepValue xs) rs (k:ks)
                | k > vcEnd vc = RepValue (map (const Nothing) xs) : loop (RepValue xs) rs ks
          loop _ ((clk,val):rs1) (k:ks)
                | k > clk      = loop val rs1 (k:ks)
          loop _ ((clk,val):rs1) (k:ks)
                | k == clk     = val : loop val rs1 ks
          loop i rs (_k:ks)
                | otherwise    = i   : loop i   rs  ks

showVCColumn :: VC -> Int -> Int -> [String]
showVCColumn vc from to =
        [ if vcEnd vc < i then ""
          else if i `elem` (map fst (vcChanges vc)) then '~' : txt
          else ' ':  txt
        | (i,txt) <- [from .. to] `zip` map show (vcSplice vc from to)
        ]

emptyVCD :: VCD
emptyVCD = VCD 0 M.empty

-- snoc on an element onto a VC node,
-- the clock numbers *must* be acsending,
-- and you can not add a fresh named node after a split.

snocVCD :: (Rep a) => String -> Int -> X a -> VCD -> VCD
snocVCD nm i val (VCD start m) = VCD start $
        M.alter (\ opt_vc -> case opt_vc of
                Nothing | start == 0
                        -> return (snocVC i (toRep val) $ emptyVC (typeX val))
                        | otherwise
                        -> error "can not add a new VC to a split VCD"
                Just vc -> return (snocVC i (toRep val) vc)
             ) nm m
--        case M.lookup nm m of
--          Just vc ->

emptyVC :: Type -> VC
emptyVC ty = VC ty (RepValue $ take (typeWidth ty) (repeat Nothing)) [] (-1)

snocVC :: Int -> RepValue -> VC -> VC
snocVC i val vc | i <= vcEnd vc = error "snocVC: error in order of calls"
                 | lastVC vc == val = vc { vcEnd = i }
                 | otherwise         = vc { vcEnd = i, vcChanges = (i,val) : vcChanges vc }

lastVC :: VC -> RepValue
lastVC (VC _ v [] _)        = v
lastVC (VC _ _ ((_,v):_) _) = v


-- | Convert a Pad to a Tracestream
padToVC :: (Clock c) => Int -> Pad c -> VC
padToVC i (StdLogic s)       = convertVC i s
padToVC i (StdLogicVector s) = convertVC i s
padToVC _ other = error $ "fix padToVC for " ++ show other

-- | Convert a Stream to a VC. Note this can force evaluation.
convertVC :: forall clk w . (Clock clk, Rep w) => Int -> Signal clk w -> VC
convertVC len xs = foldl (\ vc (i,v) -> snocVC i v vc)
                                (emptyVC (typeOfS xs))
                                [ (i,toRep v)
                                | (i,v) <- take len [0..] `zip` S.toList (shallowXS xs)
                                ]

showColumns :: [[String]] -> [String]
showColumns xss = take height
          [ unwords (zipWith item widths xs)
          | xs <- transpose xss
          ]
  where
          item :: Int -> String -> String
          item i xs = replicate (i - length xs) ' ' ++ xs

          widths :: [Int]
          widths = [ maximum (map length xs) | xs <- xss ]

          height :: Int
          height = maximum (map length xss)


instance Show VCD where
    show (VCD i m)
        | M.null m = "empty VCD, clock = " ++ show i
        | otherwise = unlines (showColumns (clock : columns))
      where
         -- last value to print
         j = maximum [ e | VC _ _ _ e <- M.elems m ]

         names = sort (M.keys m)

         clock :: [String]
         clock = "" : "" : [ show k | k <- [i .. j]]

         columns :: [[String]]
         columns = [ case M.lookup k m of
                       Nothing -> error "bad lookup name"
                       Just vc -> k : "" : showVCColumn vc i j
                   | k <- names
                   ]

-- | Convert a 'VCD' to a VCD file.
openVCD :: Integer    -- ^ Timescale in nanoseconds
        -> FilePath   -- ^ name of VCD file
        -> VCD
        -> IO Handle
openVCD ts fileName (VCD start m)
        | start /= 0 = error "can not write VCD header for VCD's that do not start at zero"
        | otherwise  = do
                h <- openFile fileName WriteMode
                hPutStr h header
                hFlush h
                return h
  where
        header :: String
        header = unlines
                [ "$version\n   Kansas Lava\n$end"
                , "$timescale " ++ show ts ++ "ns $end"
                , "$scope module top $end"
                ]
                ++ unlines [ unwords ["$var wire", show $ typeWidth ty, ident, show k, "$end"]
                | (ident,(k,VC ty _ _ _)) <- vcdIds `zip` M.assocs m ]
                ++ "$enddefinitions $end\n"
                ++ dumpVars [ (ident,i)
                            | (ident,(_,VC _ i _ _)) <- vcdIds `zip` M.assocs m
                            ]

mkComment :: String -> String
mkComment comm = unlines ["$comment",comm,"$end"]

writeVCD :: Handle -> VCD -> IO ()
writeVCD h (VCD i m) = do
        hPutStr h $ if M.null m
                    then mkComment $ "clock = " ++ show i
                    else comment ++ unlines changes
        hFlush h
      where
         -- last value to print
         j = maximum [ e | VC _ _ _ e <- M.elems m ]

         changes = concat
                 $ map (\ xs -> ("#" ++ show (fst (head xs)))
                             : [ vcdVal tag val
                               | (_,(tag,val)) <- xs
                               ])
                 $ groupBy (\ a b -> fst a == fst b)
                 $ sort
                 [ (tm,(tag,val))
                 | (tag,(_,VC _ _ cs _)) <- vcs
                 , (tm,val) <- cs
                 ]

         comment = mkComment $ "clock = " ++ show i ++ "-" ++ show j

         vcs = vcdIds `zip` M.assocs m

recordVCDFabric :: (MonadFix m, Clock c) => Int -> SuperFabric c m a -> SuperFabric c m (a,VCD)
recordVCDFabric i fab = do
        (a,ins,vars,outs) <- recordFabric fab
        return (a,VCD 0 $ foldr (\ (nm,val) -> M.insert nm (padToVC i val))
                                M.empty
                         $ [ ("inputs/" ++ nm,val) | (nm,val) <- ins ] ++
                           [ ("vars/v" ++ show n,val) | (n,val) <- vars ] ++
                           [ ("outputs/" ++ nm,val) | (nm,val) <- outs ])
{-

testVC1 :: VC
testVC1 = VC (V 4)
              (toRep (unknownX :: X U4))
              [(i,toRep (pureX (fromIntegral i) :: (X (Fin 4)))) | i <- reverse [0..10], i < 4 || i > 7]
              10

testVC2 :: VC
testVC2 = VC B
              (toRep (unknownX :: X U1))
              [(i,toRep (pureX (fromIntegral i) :: X (Unsigned 1))) | i <- reverse [0..12]]
              12

testVCD1 :: VCD
testVCD1 = VCD 0 (M.fromList [("inputs/x",testVC1),("outputs/y",testVC2)])

main = do
        h <- openVCD 100 "x.vcd" testVCD1
        let (vc1,vc2) = splitVCD testVCD1 5
        writeVCD h vc1
        writeVCD h vc2
        hClose h

example = probesToVCD 0 100 "x.vcd" (print (takeS 100 (probeS "x" (iterateS (\ x -> 1 + x*23) (0 :: Int) :: Seq Int))))

testF :: Fabric ()
testF = do
        x :: Seq U8 <- inStdLogicVector "x"
        y :: Seq U8 <- inStdLogicVector "y"
        outStdLogicVector "z" (x + y)

testF_TB :: Fabric [Bool]
testF_TB = do
        outStdLogicVector "x" (toS [1..100] :: Seq U8)
        outStdLogicVector "y" (toS [100..200] :: Seq U8)
        z :: Seq U8 <- inStdLogicVector "z"
        return [ maybe False (\ z' -> z' == x' + y') ss | (x',y',ss) <- zip3 [1..100] [100..200] (fromS z)]

-}
