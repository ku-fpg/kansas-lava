{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
-- | This module contains functions for generating VCD debug traces.
-- It also provides functionality for (de)serializing Traces.
module Language.KansasLava.VCD {-
    ( VCD(..)
    , writeVCDFile
    , readVCDFile
    , addEvent
    -- * Generate a Signature from a VCD trace
    , toSignature
    , fromSignature
    -- * Compare two VCDs
    , cmpVCD
    , ioOnly
    -- * Make a VCD trace from a Fabric and input Pads
    , mkVCD
    , mkVCDCM
    -- * Reading and Writing the Test Bench Format (.tbf)
    , readTBF
    , writeTBF
    -- * Convert Rep to Test Bench Word
    , tbw2rep
    , rep2tbw
    -- * probes
    , probesToVCD
    , snapProbesAsVCD
    ) -} where

import Language.KansasLava.Fabric
import Language.KansasLava.Rep
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Internal
import Language.KansasLava.Universal

import qualified Language.KansasLava.VCD.EventList as E

import qualified Language.KansasLava.Stream as S
import Language.KansasLava.Probes

import Control.Monad

import Data.Char
import qualified Data.Foldable as F
import Data.Function
import Data.List
import qualified Data.Map as M
import Control.Concurrent.MVar
import System.IO.Unsafe
import Data.IORef
import Control.DeepSeq
import System.IO
import qualified Data.Map as M

-- testing
import Data.Sized.Unsigned
import Data.Sized.Ix
import Language.KansasLava.Utils

----------------------------------------------------------------------------------------
-- | The VC (value change) is used for capturing traces of shallow-embedded
-- streams. It combines the bitwise representation of a stream along with the
-- type of the stream.
data VC = VC Type (E.EventList RepValue)
    deriving (Eq, Ord, Read, Show)

addVC :: VC -> Int -> RepValue -> VC
addVC (VC ty el) i v = VC ty $ E.insert (i,v) el

newVC :: forall w . (Rep w) => Witness w -> VC
newVC _ = VC (repType (Witness :: Witness w)) $ E.fromList []

-- | Convert a Pad to a Tracestream
padToVC :: Int -> Pad -> VC
padToVC c (StdLogic s) = convertVC $ take c $ S.toList $ shallowS s
padToVC c (StdLogicVector s) = convertVC $ take c $ S.toList $ shallowS s
padToVC _ other = error $ "fix padToVC for " ++ show other

-- | Convert a Stream to a VC. Note this can force evaluation.
convertVC :: forall w . (Rep w) => [X w] -> VC
convertVC l = VC ty $ E.fromList $ map toRep l
    where (VC ty _) = newVC (Witness :: Witness w)

----------------------------------------------------------------------------------------
-- | 'VCD' is a primary bit-wise record of an interactive session with some circuit
-- Map from module/name to stream.
newtype VCD = VCD [(String,VC)]
    deriving (Eq)

instance Show VCD where
    show (VCD m) = headers ++ "\n" ++ E.foldrWithTime (\(clk,str) r -> pr (show clk) clkwidth str ++ "\n" ++ r) "" rows
        where wMaxLens :: [E.EventList (String,Int)]
              wMaxLens = [ let maxlen = max $ length h
                           in fmap (\v -> let str = showRepValue ty v in (str, maxlen $ length str)) el
                         | (h, VC ty el) <- m ]

              rows = fmap fst
                   $ E.mergeWith (\(s1,l1) (s2,l2) -> (pr s1 l1 s2, l1 + l2))
                                 wMaxLens

              clkwidth = max 3 $ length $ show $ E.length rows

              widths = map (snd . E.head) wMaxLens
              headers = foldr (\(h,l) r -> pr h l r) "" $ zip ("clk" : map fst m) (clkwidth : widths)

              pr s1 l1 s2 = s1 ++ replicate (1 + l1 - length s1) ' ' ++ s2

addEvent :: forall w . (Rep w) => String -> Int -> (X w) -> VCD -> VCD
addEvent nm i v (VCD m) | nm `elem` map fst m = VCD [ (n,if n == nm then addVC vc i (toRep v) else vc) | (n,vc) <- m ]
                        | otherwise           = VCD $ (nm, addVC (newVC (Witness :: Witness w)) i (toRep v)) : m

-- | Generate a signature from a trace.
-- TODO: support generics in both these functions?
-- only used right after writeTBF.
toSignature :: VCD -> Signature
toSignature vcd = Signature (convert $ inputs vcd) (convert $ outputs vcd) []
    where convert m = [ (dropModName nm,ty) | (nm,VC ty _) <- m ]
          dropModName = reverse . takeWhile (/= '/') . reverse

-- | Creates an (empty) trace from a signature
--  only used inside function
fromSignature :: Signature -> VCD
fromSignature (Signature inps outps _) = VCD $ convert "inputs" inps ++ convert "outputs" outps
    where convert mnm l = [ (mnm ++ "/" ++ nm, VC ty $ E.fromList [])  | (nm, ty) <- l ]

scope :: String -> VCD -> [(String,VC)]
scope s = scopes [s]

scopes :: [String] -> VCD -> [(String,VC)]
scopes s (VCD m) = [ (nm,ts) | (nm,ts) <- m
                             , s' <- s
                             , s' `isPrefixOf` nm ]

inputs :: VCD -> [(String,VC)]
inputs = scope "inputs"

outputs :: VCD -> [(String,VC)]
outputs = scope "outputs"

----------------------------------------------------------------------------------------

-- | Convert a VCD file to a VCD object.
readVCDFile :: FilePath -> Signature -> IO VCD
readVCDFile fileName sig = do
   vcd <- readFile fileName

   let (signames, ls) = defs2map $ dropWhile (not . isPrefixOf "$var") $ lines $ trimWhile isSpace vcd
       vals = uncurry changes . dumpvars $ ls
       streams = [ (nm, vs) | (i, nm) <- signames, (i',vs) <- vals, i == i' ]

   return $ VCD $ [ ("inputs/" ++ nm, VC ty s)
                            | (nm,ty) <- sigInputs sig, (snm,s) <- streams, nm == snm ]
                         ++ [ ("outputs/" ++ nm, VC ty s)
                            | (nm,ty) <- sigOutputs sig, (snm, s) <- streams, nm == snm ]

-- | Parse definitions section, getting map of VCDIDs to signal names.
defs2map :: [String] -> ([(VCDID,String)],[String])
defs2map = go []
    where go m (l:ls) | head ws == "$enddefinitions" = (m,ls)
                      | head ws == "$var" = go ((ws !! 3, trimWhile (== '"') $ ws !! 4):m) ls
                      | otherwise = error "defs2map: parse error!"
            where ws = words l
          go _ _ = error "defs2map: parse error, no lines!"

trimWhile :: (Char -> Bool) -> String -> String
trimWhile p = f . f
    where f = reverse . dropWhile p

-- | Parse $dumpvars section, getting initial values for each signal.
dumpvars :: [String] -- ^ remaining lines of the vcd file
         -> ([(VCDID,RepValue)],[String]) -- ^ map of vcdIds to initial values
dumpvars ("$dumpvars":ls) = go ls []
    where go ("$end":rest) m = (m,rest)
          go (line:rest)   m = let (vcdId,val) = parseVal line
                                   (m',rest')  = go rest m
                               in ((vcdId,val):m',rest')
          go [] _ = error $ "dumpvars: no $end!"
dumpvars other = error $ "dumpvars: bad parse! " ++ show other

-- | Parse list of changes into an EventList
changes :: [(VCDID,RepValue)] -> [String] -> [(String, E.EventList RepValue)]
changes initVals ls = M.toList $ unMerge elist
    where (_,elist) = foldl go (0,E.fromList []) ls

          go :: (Int,E.EventList (String, RepValue)) -> String -> (Int,E.EventList (String, RepValue))
          go (_,el) ('#':time) = (read time, el)
          go (t,el) line       = (t, E.insert (t, parseVal line) el)

          unMerge :: (E.EventList (String,RepValue)) -> M.Map String (E.EventList RepValue)
          unMerge = E.foldrWithTime f $ M.fromList [ (i,E.fromList [v]) | (i,v) <- initVals ]
            where f (i,(nm,v)) m | M.member nm m = M.adjust (E.insert (i,v)) nm m
                                 | otherwise     = M.insert nm (E.singleton (i,v)) m

parseVal :: String -> (String, RepValue)
parseVal = go . words
    where go [bitVal] | length bitVal > 1   = (tail bitVal, tbw2rep $ take 1 bitVal)
          go [t:vals,ident] | t `elem` "bB" = (ident      , tbw2rep vals           )
          go other                          = error $ "parseVal: can't parse! " ++ unwords other

----------------------------------------------------------------------------------------

-- | Convert a 'VCD' to a VCD file.
writeVCDFile :: Bool    -- ^ Whether to include the clock signal in the list of signals
          -> Integer    -- ^ Timescale in nanoseconds
          -> FilePath   -- ^ name of VCD file
          -> VCD
          -> IO ()
writeVCDFile _incClk ts fileName (VCD m) = writeFile fileName $ unlines
    [ "$version\n   Kansas Lava\n$end"
    , "$timescale " ++ show ts ++ "ns $end"
    , "$scope module top $end"
    ]
    ++ unlines [ unwords ["$var wire", show $ typeWidth ty, ident, show k, "$end"]
               | (ident,(k,VC ty _)) <- signals ]
    ++ "$enddefinitions $end\n"
    ++ values [ (i',strm) | (i',(_,VC _ strm)) <- signals ]

    where signals = zip vcdIds m

type VCDID = String
-- VCD uses a compressed identifier naming scheme. This CAF generates the identifiers.
vcdIds :: [VCDID]
vcdIds = map code [0..]
    where code :: Int -> VCDID
          code i | i < 0 = ""
          code i         = chr (33 + mod i 94) : code (div i 94 - 1)

values :: [(VCDID, E.EventList RepValue)] -> String
values sigs = dumpVars initials ++ eventList rest
    where (initials,rest) = unzip [ ((i, E.head el), (i, el)) | (i, el) <- sigs ]

dumpVars :: [(VCDID, RepValue)] -> String
dumpVars vals = "$dumpvars\n" ++ unlines (map (uncurry vcdVal) vals) ++ "$end\n"

eventList :: [(VCDID, E.EventList RepValue)] -> String
eventList strms = E.foldrWithTime (\(t,ls) r -> "#" ++ show t ++ "\n" ++ ls ++ "\n" ++ r) "" elist
    where elist = E.mergeWith (\s1 s2 -> s1 ++ ('\n':s2))
                              [ fmap (vcdVal ident) elist' | (ident,elist') <- strms ]

vcdVal :: VCDID -> RepValue -> String
vcdVal i r@(RepValue bs) | length bs == 1 = rep2tbw r ++ i
                         | otherwise      = "b" ++ rep2tbw r ++ " " ++ i

----------------------------------------------------------------------------------------

-- | Compare two trace objects. First argument is the golden value. See notes for cmpRepValue
cmpVCD :: VCD -> VCD -> Bool
cmpVCD (VCD m1) (VCD m2) =
    and [ k1 == k2 && cmpVC (tslen s1) s1 s2
        | ((k1,s1),(k2,s2)) <- zip (sorted m1) (sorted m2)
        ]
    where tslen (VC _ el) = E.length el
          sorted = sortBy ((compare) `on` fst)

ioOnly :: VCD -> VCD
ioOnly = VCD . scopes ["inputs","outputs"]

-- | 'cmpVC' compares two traces to determine equivalence. Note this
-- uses 'cmpRepValue' under the hood, so the first argument is considered the
-- golden trace.
cmpVC :: Int -> VC -> VC -> Bool
cmpVC count (VC t1 s1) (VC t2 s2) = t1 == t2 && countLTs1 && s1LTs2 && eql
    where countLTs1 = count <= E.length s1
          s1LTs2 = E.length s1 <= E.length s2
          eql = F.foldr (&&) True $ E.zipWith cmpRepValue (E.take count s1) (E.take count s2)

-- | Make a 'VCD' from a 'Fabric' and its input.
mkVCD :: Int            -- ^ number of cycles to capture
      -> Fabric ()      -- ^ The Fabric we are tracing
      -> [(String,Pad)] -- ^ Inputs to the Fabric
      -> IO VCD
mkVCD c fabric input = do
    (trace, _) <- mkVCDCM c fabric input (return)
    return trace

-- | Version of 'mkVCD' that accepts arbitrary circuit mods.
mkVCDCM :: Int               -- ^ number of cycles to capture
        -> Fabric ()         -- ^ Fabric we are tracing
        -> [(String, Pad)]   -- ^ Inputs to the Fabric
        -> (KLEG -> IO KLEG) -- ^ KLEG Mod
        -> IO (VCD, KLEG)
mkVCDCM c fabric input circuitMod = do
    rc <- (reifyFabric >=> circuitMod) fabric

    let Pure (_,output) = runFabric fabric input
        tr = VCD $ [ ("inputs/" ++ nm, padToVC c p)
                   | (nm,_) <- theSrcs rc
                   , (nm',p) <- input
                   , nm == nm' ]
                 ++ [ ("outputs/" ++ nm, padToVC c p)
                    | (nm,_,_) <- theSinks rc
                    , (nm',p) <- output
                    , nm == nm' ]

    return (tr, rc)

-- Wraps up a Fabric with a VCD capture of inputs and outputs.
vcdFabric :: Fabric a -> Fabric (a,VCD)
vcdFabric fab = do
        (a,ins0,outs0) <- traceFabric fab
        return $ (a,VCD [])


----------------------------------------------------------------------------------------

-- | Convert the inputs and outputs of a VCD to the textual format expected
-- by a testbench.
writeTBF :: String -> VCD -> IO ()
writeTBF filename = writeFile filename . unlines . mergeWith (++) . asciiStrings

-- | Inverse of showTBF, needs a signature for the shape of the desired VCD.
-- Creates a VCD from testbench signal files.
readTBF :: [String] -> Signature -> VCD
readTBF ilines sig = VCD $ ins ++ outs
    where et = fromSignature sig
          widths = [ typeWidth ty
                   | (_,VC ty _) <- inputs et ++ outputs et
                   ]
          (inSigs, outSigs) = splitAt (length $ inputs et) $ splitLists ilines widths
          addToMap sigs m = [ (k,VC ty $ E.fromList $ map tbw2rep strm)
                            | (strm,(k,VC ty _)) <- zip sigs m
                            ]
          (ins, outs) = (addToMap inSigs $ inputs et, addToMap outSigs $ outputs et)

-- | Convert a VCD into a list of lists of Strings, each String is a value,
-- each list of Strings is a signal.
asciiStrings :: VCD -> [[String]]
asciiStrings vcd = [ E.toList $ fmap rep2tbw s | VC _ s <- insOuts ]
    where insOuts = [ ts | (_,ts) <- inputs vcd ++ outputs vcd ]

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

-- We keep this thread-safe, just in case.
{-# NOINLINE vcdOfProbes #-}
vcdOfProbes :: MVar VCD_
vcdOfProbes = unsafePerformIO $ newEmptyMVar

{-# NOINLINE probesToVCD #-}
probesToVCD :: Integer -> Integer -> String -> IO () -> IO ()
probesToVCD size speed filename todo = do
        v <- newMVar emptyVCD_
        setShallowProbes $ \ nm clkNo x -> unsafePerformIO $ do
                modifyMVar_ v $ \ vcd -> return (snocVCD_ nm clkNo x vcd)
                return x
        todo
        vcd <- takeMVar v
        h <- openVCD speed filename vcd
        writeVCD h vcd
        hClose h
        putStrLn $ "[Written probe values to " ++ show filename ++ "]"
        return ()

{-
{-# NOINLINE snapProbesAsVCD #-}
snapProbesAsVCD :: IO VCD_
snapProbesAsVCD = readMVar vcdOfProbes
-}

----------------------------------------------------------------------------
{-
data VCD' = VCD' Handle                                            -- When we send the fine
                 (MVar Integer)                                    -- How much we've flushed
                 (MVar (M.Map String (M.Map Integer [RepValue])))  -- What we've got in pipeline


openVCD' :: String -> IO VCD'
openVCD' nm = do
        h <- openFile nm ReadMode       -- AppendMode?
        vI <- newMVar 0
        vM <- newMVar M.empty
        return $ VCD' h vI vM

-- This simply records a single event.
writeVCD' :: String -> Integer -> X a -> VCD' -> IO ()
writeVCD' nm i x (VCD h vI vM) =
        v <- takeMVar vM
        -- need to de-rep the x
        putMVar vM $ Map.alter nm (maybe (Map.insert i x) M.empy)

flushVCD' :: VCD' -> (VCD -> VCD) -> IO ()
flushVCD' (VCD' v i) =
-}

data VCD_ = VCD_ Integer (M.Map String VC_)
--        deriving Show


data VC_ = VC_
         { vcType     :: Type
         , vcInit     :: RepValue
         , vcChanges  :: [(Integer,RepValue)] -- reversed list of events
         , vcEnd      :: Integer
         }
        deriving Show


-- inefficent, but works
showVCColumn :: VC_ -> Integer -> Integer -> [String]
showVCColumn vc from to = loop (show (vcInit vc)) (reverse [ (i,show v) | (i,v) <- vcChanges vc]) [from .. to]
  where
          loop _ _                []     = []
          loop i rs (k:ks)
                | k > vcEnd vc = ""  : loop i   rs ks
          loop i rs@((clk,val):rs1) (k:ks)
                | k == clk     = val : loop val rs1 ks
          loop i rs (k:ks)
                | otherwise    = i   : loop i   rs  ks

-- modifyMVar_
--alterVCD :: MVar VCD_ -> (VCD_ -> IO VCD_) -> IO ()
--alterVCD = do
--        v <-

emptyVCD_ :: VCD_
emptyVCD_ = VCD_ 0 M.empty

-- snoc on an element onto a VC node,
-- the clock numbers *must* be acsending,
-- and you can not add a fresh named node after a split.

snocVCD_ :: (Rep a) => String -> Integer -> X a -> VCD_ -> VCD_
snocVCD_ nm i val (VCD_ start m) = VCD_ start $
        M.alter (\ opt_vc -> case opt_vc of
                Nothing | start == 0
                        -> return (snocVC_ i (toRep val) $ emptyVC_ (typeX val))
                        | otherwise
                        -> error "can not add a new VC to a split VCD"
                Just vc -> return (snocVC_ i (toRep val) vc)
             ) nm m
--        case M.lookup nm m of
--          Just vc ->

emptyVC_ :: Type -> VC_
emptyVC_ ty = VC_ ty (RepValue $ take (typeWidth ty) (repeat Nothing)) [] (-1)

snocVC_ :: Integer -> RepValue -> VC_ -> VC_
snocVC_ i val vc | i <= vcEnd vc = error "snocVC_: error in order of calls"
                 | lastVC_ vc == val = vc { vcEnd = i }
                 | otherwise         = vc { vcEnd = i, vcChanges = (i,val) : vcChanges vc }

lastVC_ :: VC_ -> RepValue
lastVC_ (VC_ _ v [] _)        = v
lastVC_ (VC_ _ _ ((_,v):_) _) = v

-- TODO: if you are past the end, you are undefined
valueAt :: VC_ -> Integer -> RepValue
valueAt (VC_ _ i c e) now | now > e   = error "valueAt passed end of time"
                          | otherwise = find c
  where
          find []               = i
          find ((clk,val):rest) = if clk <= now then val
                                                else find rest

-- TODO: be careful about leaks. None of this should be lazy.
splitVCD :: VCD_ -> Integer -> (VCD_,VCD_)
splitVCD (VCD_ i m) j = (VCD_ i before,VCD_ j after)
    where
          before = fmap (\ vc@(VC_ t i cs e) -> VC_ t
                                                   i
                                                   (filter (\ (x,_) -> x < j) cs)
                                                   (j - 1)
                        ) m
          after  = fmap (\ vc@(VC_ t i cs e) -> VC_ t
                                                   (valueAt vc (j - 1))
                                                   (filter (\ (x,_) -> x >= j) cs)
                                                   e
                        ) m

testVC1 = VC_ (V 4)
              (toRep (unknownX :: X U4))
              [(i,toRep (pureX (fromIntegral i) :: X (Unsigned X4))) | i <- reverse [0..10], i < 4 || i > 7]
              10

testVC2 = VC_ B
              (toRep (unknownX :: X U1))
              [(i,toRep (pureX (fromIntegral i) :: X (Unsigned X1))) | i <- reverse [0..12]]
              12

testVCD1 = VCD_ 0 (M.fromList [("x",testVC1),("y",testVC2)])


--         comment = "$comment\nclock = " ++ show i ++ "\n$end"

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

          xss' = map ((++) (repeat "")) xss


instance Show VCD_ where
    show (VCD_ i m)
        | M.null m = "empty VCD, clock = " ++ show i
        | otherwise = unlines (showColumns (clock : columns))
      where
         -- last value to print
         j = maximum [ e | VC_ _ _ _ e <- M.elems m ]

         comment = "-- clock = " ++ show i ++ "-" ++ show j

         names = sort (M.keys m)

         clock :: [String]
         clock = "" : "" : [ show k | k <- [i .. j]]

         columns :: [[String]]
         columns = [ case M.lookup k m of
                       Nothing -> error "bad lookup name"
                       Just vc -> k : "" : showVCColumn vc i j
                   | k <- names
                   ]
--         maxValue = maximum [ keys
{-
--         wMaxLens :: [E.EventList (String,Int)]
{-
              wMaxLens = [ let maxlen = max $ length h
                           in fmap (\v -> let str = showRepValue ty v in (str, maxlen $ length str)) el
                         | (h, VC ty el) <- m ]


--            headers -- ++ "\n" ++ E.foldrWithTime (\(clk,str) r -> pr (show clk) clkwidth str ++ "\n" ++ r) "" rows

        where wMaxLens :: [E.EventList (String,Int)]
              wMaxLens = [ let maxlen = max $ length h
                           in fmap (\v -> let str = showRepValue ty v in (str, maxlen $ length str)) el
                         | (h, VC ty el) <- m ]

              rows = fmap fst
                   $ E.mergeWith (\(s1,l1) (s2,l2) -> (pr s1 l1 s2, l1 + l2))
                                 wMaxLens

              clkwidth = max 3 $ length $ show $ E.length rows

              widths = map (snd . E.head) wMaxLens

              headers = foldr (\(h,l) r -> pr h l r) "" $ zip ("clk" : map fst m) (clkwidth : widths)

              pr s1 l1 s2 = s1 ++ replicate (1 + l1 - length s1) ' ' ++ s2
-}
-}

-- | Convert a 'VCD' to a VCD file.
openVCD :: Integer    -- ^ Timescale in nanoseconds
        -> FilePath   -- ^ name of VCD file
        -> VCD_
        -> IO Handle
openVCD ts fileName (VCD_ start m)
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
                | (ident,(k,VC_ ty _ _ _)) <- vcdIds `zip` M.assocs m ]
                ++ "$enddefinitions $end\n"
                ++ dumpVars [ (ident,i)
                            | (ident,(k,VC_ _ i _ _)) <- vcdIds `zip` M.assocs m
                            ]

mkComment comm = unlines ["$comment",comm,"$end"]

writeVCD :: Handle -> VCD_ -> IO ()
writeVCD h (VCD_ i m) = do
        hPutStr h $ if M.null m
                    then mkComment $ "clock = " ++ show i
                    else comment ++ unlines changes
        hFlush h
      where
         -- last value to print
         j = maximum [ e | VC_ _ _ _ e <- M.elems m ]

         changes = concat
                 $ map (\ xs -> ("#" ++ show (fst (head xs)))
                             : [ vcdVal tag val
                               | (_,(tag,val)) <- xs
                               ])
                 $ groupBy (\ a b -> fst a == fst b)
                 $ sort
                 [ (tm,(tag,val))
                 | (tag,(_,VC_ _ _ cs _)) <- vcs
                 , (tm,val) <- cs
                 ]

         comment = mkComment $ "clock = " ++ show i ++ "-" ++ show j

         vcs = vcdIds `zip` M.assocs m

main = do
        h <- openVCD 100 "x.vcd" testVCD1
        let (vc1,vc2) = splitVCD testVCD1 5
        writeVCD h vc1
        writeVCD h vc2
        hClose h

example = probesToVCD 0 100 "x.vcd" (print (takeS 100 (probeS "x" (iterateS (\ x -> 1 + x*23) (0 :: Int) :: Seq Int))))
