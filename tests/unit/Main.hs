{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}
module Main where

import Language.KansasLava
import Language.KansasLava.Stream as S
import Language.KansasLava.Testing.Thunk

import Data.Bits
import Data.Default
import Data.List ( sortBy, sort )
import Data.Ord ( comparing )
import Data.Maybe as Maybe
import Data.Sized.Arith
import Data.Sized.Ix
import qualified Data.Sized.Matrix as M
import Data.Sized.Sampled
import Data.Sized.Signed
import Data.Sized.Unsigned
import Debug.Trace

import Control.Applicative
import Control.Concurrent.MVar
import System.Cmd
import Trace.Hpc.Reflect
import Trace.Hpc.Tix

import Report
import Utils

main = do
        let opt = def { verboseOpt = 9  -- 4 == show cases that failed
                      , genSim = True
--                      , runSim = True
                      , simMods = [("default_opts", (optimizeCircuit def))]
                      , testOnly = return ["fifo"]
                      , testNever = ["max","min","abs","signum"] -- for now
                      }

        -- This should be built using the cabal system,
        -- can called from inside the ./dist directory.
        system "ghc -i../.. -o tracediff --make Diff.hs"

        putStrLn "Running with the following options:"
        putStrLn $ show opt

        prepareSimDirectory opt

        results <- newMVar [] :: IO (MVar [(String,Result)])

        let reporter (name, result) = do
                rs <- takeMVar results
                putMVar results $ (name,result) : rs

        let test :: TestSeq
            test = TestSeq (testSeq opt reporter)
                        (case testData opt of
                           Nothing -> genToList
                           Just i -> take i . genToRandom)

        tests test

        rs <- takeMVar results

        let r = generateReport $ reverse rs

        putStrLn $ show r

        html <- reportToHtml r
        writeFile "report.html" html
        shtml <- reportToSummaryHtml r
        writeFile "summary.html" shtml

        Tix tix <- examineTix
        let counts = concat [ xs | TixModule _ _ _ xs <- tix ]
        let len = length counts
        let txs = length $ filter (> 0) counts
        putStrLn $ "Raw coverage: " ++ show (floor (100 * fromIntegral txs / fromIntegral len)) ++ "%"

tests test = do
        -- Just the Eq Stuff
        let t :: (Eq a, Show a, Rep a) =>
                 String -> Gen a -> IO ()
            t str arb = testOpsEq test str arb

        t "StdLogicVector/X1" (arbitrary :: Gen (StdLogicVector X1))
        t "StdLogicVector/X2" (arbitrary :: Gen (StdLogicVector X2))
        t "StdLogicVector/X3" (arbitrary :: Gen (StdLogicVector X3))
        t "StdLogicVector/X4" (arbitrary :: Gen (StdLogicVector X4))
        t "StdLogicVector/X8" (arbitrary :: Gen (StdLogicVector X8))
        t "StdLogicVector/X32" (arbitrary :: Gen (StdLogicVector X32))

        -- Just the Ord Stuff
        let t :: (Show a, Ord a, Rep a) => String -> Gen a -> IO ()
            t str arb = testOpsOrd test str arb

        t "Bool" (arbitrary :: Gen Bool)

        -- Just the Num Stuff
        let t :: (Num a, Ord a, Rep a) => String -> Gen a -> IO ()
            t str arb = testOpsNum test str arb

        -- With Sampled, use
        --  * powers of two scale, larger than 1
        --  * make sure there are enough bits to represent
        --     both the fractional and non-fractional part.

        t "Sampled/X8xX8" (arbitrary :: Gen (Sampled X8 X8))
-- These do not represent every integer in their range, so fail
--        t "Sampled/X4xX2" (arbitrary :: Gen (Sampled X4 X2))
--        t "Sampled/X2xX2" (arbitrary :: Gen (Sampled X2 X2))
--        t "Sampled/X2xX1" (arbitrary :: Gen (Sampled X2 X1))
-- This have a round error; looks like a base case
--        t "Sampled/X1xX2" (arbitrary :: Gen (Sampled X1 X2))
        t "Sampled/X1xX4" (arbitrary :: Gen (Sampled X1 X4))
        t "Sampled/X8xX10"(arbitrary :: Gen (Sampled X8 X10))
        t "Sampled/X128xX16"(arbitrary :: Gen (Sampled X128 X16))

        -- Just the Bits Stuff
        let t :: (Ord a, Bits a, Rep a) => String -> Gen a -> IO ()
            t str arb = testOpsBits test str arb

        t "U1" (arbitrary :: Gen U1)
        t "U2" (arbitrary :: Gen U2)
        t "U3" (arbitrary :: Gen U3)
        t "U4" (arbitrary :: Gen U4)
        t "U5" (arbitrary :: Gen U5)
        t "U6" (arbitrary :: Gen U6)
        t "U7" (arbitrary :: Gen U7)
        t "U8" (arbitrary :: Gen U8)
        t "U32" (arbitrary :: Gen U32)
{- ghci keeps getting killed during these, Out Of Memory maybe?
        t "U64" (arbitrary :: Gen (Unsigned X64))
-}

        -- no S1
        t "S2" (arbitrary :: Gen S2)
        t "S3" (arbitrary :: Gen S3)
        t "S4" (arbitrary :: Gen S4)
        t "S5" (arbitrary :: Gen S5)
        t "S6" (arbitrary :: Gen S6)
        t "S7" (arbitrary :: Gen S7)
        t "S8" (arbitrary :: Gen S8)
        t "S32" (arbitrary :: Gen S32)
{- ghci keeps getting killed during these, Out Of Memory maybe?
        t "S64" (arbitrary :: Gen (Signed X64))
-}
        -- Just the Eq Stuff

        -- None

        --  Now registers
        let t :: (Eq a, Show a, Rep a) => String -> Gen a -> IO ()
            t str arb = testRegister test str arb

        t "U1" (loop 10 (arbitrary :: Gen U1))
        t "U2" (loop 10 (arbitrary :: Gen U2))
        t "U3" (loop 10 (arbitrary :: Gen U3))
        t "Int" (loop 10 (arbitrary :: Gen Int))
        t "Bool" (loop 10 (arbitrary :: Gen Bool))

        let t :: (Eq a, Show a, Rep a) => String -> Gen a -> IO ()
            t str arb = testDelay test str arb

        t "U1" (loop 10 (arbitrary :: Gen U1))
        t "U2" (loop 10 (arbitrary :: Gen U2))
        t "U3" (loop 10 (arbitrary :: Gen U3))
        t "Int" (loop 10 (arbitrary :: Gen Int))
        t "Bool" (loop 10 (arbitrary :: Gen Bool))

        --  Memories
        let t :: (Eq b, Integral a, Show b,
                 Size (Column a), Size (Row a),
                 Size a, Rep a, Rep b) =>
                 String -> Gen (Maybe (a,b)) -> IO ()
            t str arb = testConstMemory test str arb

        t "X1xBool" (loop 10 $ dubSeq (arbitrary :: Gen (Maybe (X1,Bool))))
        t "X1xU4" (dubSeq (arbitrary :: Gen (Maybe (X1,U4))))
        t "X2xU4" (dubSeq (arbitrary :: Gen (Maybe (X2,U4))))
        t "X4xU4" (dubSeq (arbitrary :: Gen (Maybe (X4,U4))))
        t "X16xS10" (dubSeq (arbitrary :: Gen (Maybe (X256,S10))))

        let t :: (Eq a, Integral a, Show b,
                  Size (Column a), Size (Row a),
                  Size a, Rep a, Rep b, Eq b
                 ) =>
                  String -> Gen (Maybe (a,b),a) -> IO ()
            t str arb = testSyncMemory test str arb
        t "X1xBool" (loop 10 $ dubSeq (arbitrary :: Gen (Maybe (X1,Bool),X1)))
        t "X2xU4" (dubSeq (arbitrary :: Gen (Maybe (X2,U4),X2)))
        t "X4xU5" (dubSeq (arbitrary :: Gen (Maybe (X4,U5),X4)))

        let t :: (Eq a, Integral a, Show b,
                  Size (Column a), Size (Row a),
                  Size a, Rep a, Rep b, Eq b
                 ) =>
                  String -> Gen (Maybe (a,b),a) -> IO ()
            t str arb = testAsyncMemory test str arb
        t "X1xBool" (loop 10 $ dubSeq (arbitrary :: Gen (Maybe (X1,Bool),X1)))
        t "X2xU4" (dubSeq (arbitrary :: Gen (Maybe (X2,U4),X2)))
        t "X4xU5" (dubSeq (arbitrary :: Gen (Maybe (X4,U5),X4)))

        -- testing FIFOs

        let t str arb = testFIFO test str arb
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5))

{- ghci keeps getting killed during these, Out Of Memory maybe?
        t "X16xS10" (dubSeq (arbitrary :: Gen (Maybe (X16,S10),X16)))
-}

main_testLabel :: IO ()
main_testLabel = do
        let g :: Seq U4 -> Seq U4 -> Seq U4
            g a b = output "out" ((+) (input "a" a) (input "b" b))

        c <- reifyCircuit g
        print c
        let sig = circuitSignature c
        if "[(a$0,4U),(b$1,4U)]" == show (sort (sigInputs sig))
          then print "label inputs passed"
          else do print ("labels failed: ",show sig,show (sort (sigInputs sig)))
                  print c
        print ()

allValues :: forall w . (Rep w) => [w]
allValues = xs
    where
        xs :: [w]
        xs = Maybe.catMaybes
           $ fmap (unX :: X w -> Maybe w)
           $ fmap (fromRep :: RepValue -> X w)
           $ (allReps (Witness :: Witness w))

allBounded :: (Enum w, Bounded w) => [w]
allBounded = [minBound..maxBound]
-------------------------------------------------------------------------------------------------
testMux :: forall a .
        (Size (ADD (WIDTH a) (WIDTH a)),
         Enum (ADD (WIDTH a) (WIDTH a)),
         Eq a, Show a, Rep a) => TestSeq -> String -> Gen (Bool,a,a) -> IO ()
testMux (TestSeq test toList) nm gen = do
        let (gate,us0,us1) = unzip3 $ toList gen
        let thu = Thunk (mux2 :: Seq Bool -> (Seq a, Seq a) -> Seq a)
                        (\ f -> f (toSeq gate)
                                  (toSeq us0, toSeq us1)
                        )
            res = toSeq (Prelude.zipWith3 (\ c x y -> if c then x else y)
                                  gate
                                  us0
                                  us1
                        )
        test nm (length gate) thu res

-------------------------------------------------------------------------------------------------
-- This only tests at the *value* level, and ignores testing unknowns.

testUniOp :: (Rep a, Show a, Eq a, Rep b, Show b, Eq b) => TestSeq -> String -> (a -> b) -> (Comb a -> Comb b) -> Gen a -> IO ()
testUniOp (TestSeq test toList) nm op lavaOp gen = do
        let us0 = toList gen
        let thu = Thunk (liftS1 lavaOp)
                        (\ f -> f (toSeq us0)
                        )
            res = toSeq (fmap op
                              us0
                        )
        test nm (length us0) thu res

testBinOp :: (Rep a, Show a, Eq c, Rep b, Show b, Eq b, Rep c, Show c) => TestSeq -> String -> (a -> b -> c) -> (Comb a -> Comb b -> Comb c) -> Gen (a,b) -> IO ()
testBinOp (TestSeq test toList) nm op lavaOp gen = do
        let (us0,us1) = unzip $ toList gen
        let thu = Thunk (liftS2 lavaOp)
                        (\ f -> f (toSeq us0) (toSeq us1)
                        )
            res = toSeq (Prelude.zipWith op
                                  us0
                                  us1
                        )
        test nm (length (zip us0 us1)) thu res

testTriOp :: (Rep a, Show a, Eq c, Rep b, Show b, Eq b, Rep c, Show c, Rep d, Show d, Eq d) => TestSeq -> String -> (a -> b -> c -> d) -> (Comb a -> Comb b -> Comb c -> Comb d) -> Gen (a,b,c) -> IO ()
testTriOp (TestSeq test toList) nm op lavaOp gen = do
        let (us0,us1,us2) = unzip3 $ toList gen
        let thu = Thunk (liftS3 lavaOp)
                        (\ f -> f (toSeq us0) (toSeq us1) (toSeq us2)
                        )
            res = toSeq (Prelude.zipWith3 op
                                  us0
                                  us1
                                  us2
                        )
        test nm (length (zip us0 us1)) thu res

------------------------------------------------------------------------------------------------

testOpsEq :: (Rep w, Eq w, Show w) => TestSeq -> String -> Gen w -> IO ()
testOpsEq test tyName ws = do
        let ws2 = pair ws

        sequence_
          [ testTriOp test (name ++ "/" ++ tyName) op lavaOp
                        (pure (\ c (a,b) -> (c,a,b))
                                          <*> arbitrary
                                          <*> ws2)
          | (name,op,lavaOp) <-
                [ ("mux",\ c a b -> if c then a else b,\ c a b -> mux2 c (a,b))
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <-
                [ ("double-equal",(==),(.==.))
--              , ("not-equal",(/=),(./=.))
                ]
          ]


------------------------------------------------------------------------------------------------


testOpsOrd :: (Rep w, Ord w, Show w) => TestSeq -> String -> Gen w -> IO ()
testOpsOrd test tyName ws = do
        let ws2 = pair ws

        testOpsEq test tyName ws

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <-
                [ ("greater-than",(>),(.>.))
                , ("less-than",(<),(.<.))
                , ("gt-equal",(>=),(.>=.))
                , ("lt-equal",(<=),(.<=.))
                ]
          ]


------------------------------------------------------------------------------------------------


testOpsNum :: forall w .
        (Ord w, Rep w, Num w) => TestSeq -> String -> Gen w -> IO ()
testOpsNum test tyName ws = do
        testOpsOrd test tyName ws

        let ws2 = pair ws

        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) op lavaOp ws
          | (name,op,lavaOp) <-
                [ ("negate",negate,negate)
                , ("abs",abs,abs)
                , ("signum",signum,signum)
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName)  op lavaOp ws2
          | (name,op,lavaOp) <-
                [ ("add",(+),(+))
                , ("sub",(-),(-))
                , ("mul",(*),(*))
                , ("max",max,max)
                , ("min",min,min)
                ]
          ]

----------------------------------------------------------------------------------------

testOpsBits :: forall w .
        (Ord w, Rep w, Bits w) => TestSeq -> String -> Gen w -> IO ()
testOpsBits test tyName ws = do
        testOpsNum test tyName ws

        let ws2 = pair ws

        sequence_
          [ testUniOp test (name ++ "/" ++ tyName) op lavaOp ws
          | (name,op,lavaOp) <-
                [ ("complement",complement,complement)
                ]
          ]

        sequence_
          [ testBinOp test (name ++ "/" ++ tyName) op lavaOp ws2
          | (name,op,lavaOp) <-
                [ ("bitwise-and",(.&.),(.&.))
                , ("bitwise-or",(.|.),(.|.))
                , ("xor",(xor),(xor))
                ]
          ]

pair :: (Applicative f) => f a -> f (a, a)
pair ws = pure (,) <*> ws <*> ws

triple :: (Applicative f) => f a -> f (a, a, a)
triple ws = pure (,,) <*> ws <*> ws <*> ws

--------------------------------------------------------------------------------------
-- Testing register and memory
testRegister :: forall w . (Show w, Eq w, Rep w) => TestSeq -> String -> Gen w -> IO ()
testRegister  (TestSeq test toList) tyName ws = do
        let (u0:us0) = toList ws
        let reg = register :: w -> Seq w -> Seq w
        let thu = Thunk (reg u0)
                        (\ f -> f (toSeq us0)
                        )
            res = toSeq (u0 : us0)
        test ("register/" ++ tyName) (length us0) thu res
        return ()

testDelay :: forall w . (Show w, Eq w, Rep w) => TestSeq -> String -> Gen w -> IO ()
testDelay  (TestSeq test toList) tyName ws = do
        let us0 = toList ws
        let reg = delay :: Seq w -> Seq w
        let thu = Thunk reg
                        (\ f -> f (toSeq us0)
                        )
            res = shallowSeq (unknownX :~ S.fromList (map pureX us0))
        test ("delay/" ++ tyName) (length us0) thu res
        return ()

testAsyncMemory :: forall w1 w2 . (Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (Maybe (w1,w2),w1) -> IO ()
testAsyncMemory (TestSeq test toList) tyName ws = do
        let (writes,reads) = unzip $ toList ws
        let mem = asyncRead . writeMemory :: Seq (Maybe (w1,w2)) -> Seq w1 -> Seq w2
        let thu = Thunk mem
                        (\ f -> f (toSeq writes) (toSeq reads)
                        )
            res :: Seq w2
            res = toSeq' $
                    [ last $
                     [Nothing] ++
                     [ Just b
                     | Just (a,b) <- take (i - 1) writes
                     , a == fromIntegral r
                     ]
                    | (i,r) <- zip [1..(length writes-1)] reads

                    ]
        test ("async-memory/" ++ tyName) (length writes) thu res

testSyncMemory :: forall w1 w2 . (Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (Maybe (w1,w2),w1) -> IO ()
testSyncMemory (TestSeq test toList) tyName ws = do
        let (writes,reads) = unzip $ toList ws
        let mem = syncRead . writeMemory :: Seq (Maybe (w1,w2)) -> Seq w1 -> Seq w2
        let thu = Thunk mem
                        (\ f -> f (toSeq writes) (toSeq reads)
                        )
            res :: Seq w2
            res = toSeq' $
                    [Nothing] ++
                    [ last $
                     [Nothing] ++
                     [ Just b
                     | Just (a,b) <- take (i - 1) writes
                     , a == fromIntegral r
                     ]
                    | (i,r) <- zip [1..(length writes-1)] reads
                    ]
        test ("sync-memory/" ++ tyName) (length writes) thu res


testConstMemory :: forall w1 w2 . (Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (Maybe (w1,w2)) -> IO ()
testConstMemory (TestSeq test toList) tyName ws = do
        let writes = toList ws
        let mem = memoryToMatrix . writeMemory :: Seq (Maybe (w1,w2)) -> Seq (M.Matrix w1 w2)
        let thu = Thunk mem
                        (\ f -> f (toSeq writes)
                        )
            res :: M.Matrix w1 (Seq w2)
            res = M.matrix
                $ [ toSeq' $
                    [ last $
                     [Nothing] ++
                     [ Just b
                     | Just (a,b) <- take (i - 1) writes
                     , a == fromIntegral x
                     ]
                    | i <- [1..(length writes-1)]
                    ]
                  | x <- [0..(size (error "witness" :: w1) - 1 )]
                  ]
        test ("memory/const/" ++ tyName) (length writes) thu (pack res)
        return ()


-- stutters up to 16 cycles.
testFIFO :: forall w . (Eq w, Rep w, Show w) => TestSeq -> String -> Gen (Bool,Maybe w) -> IO ()
testFIFO (TestSeq test toList) tyName ws = do
        let outBools :: [Bool]
            vals    :: [Maybe w]
            (outBools,vals) = unzip $ toList ws

        print $ take 20 $ outBools
        print $ take 20 $ vals

        let cir = (unHandShaken . fifo (Witness :: Witness X1) low . HandShaken)
                                                   :: (Seq Bool -> Seq (Enabled w))
                                                   -> (Seq Bool -> Seq (Enabled w))
        let thu :: Thunk (CSeq () (Bool,Enabled w))
            thu = Thunk cir
                        (\ f -> pack ( undefinedS :: CSeq () Bool
                                     , f (unHandShaken (toHandShaken' (repeat 0) vals)) (toSeq (cycle outBools))
                                     )
                        )

        let fifoSize :: Int
            fifoSize = size (error "witness" :: X1)

        let -- fifoSpec b c d | trace (show ("fifoSpec",take 10 b, take 10 c,d)) False = undefined
            fifoSpec (val:vals) outs state
                        | length [ () | Just _ <- state ] < fifoSize
                        = fifoSpec2 vals  outs (val:state)
                          -- FIFO is full, so do not accept
                        | otherwise
                        = fifoSpec2  (val:vals) outs (Nothing:state)

            fifoSpec2 vals (ready:outs) state = 
                    case [ x | Just x <- reverse $ drop 3 state ] of
                        [] -> Nothing   : fifoSpec vals outs state
                        (x:_) -> Just x : fifoSpec  vals outs (nextState state ready)
                        
            nextState state False = state
            nextState state True  = take 3 state ++ init [ Just x | Just x <- drop 3 state ]

        let res :: Seq (Bool,Enabled w)
            res = pack (undefinedS,toSeq $ fifoSpec vals (cycle outBools) [])
        test ("fifo/" ++ tyName) (length vals) thu res
        return ()
