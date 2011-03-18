{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Memory where

import Language.KansasLava

import Utils
import Data.Sized.Arith
import Data.Sized.Matrix as M hiding (length)
import Data.Sized.Signed
import Data.Sized.Unsigned

import Data.List as List

tests :: TestSeq -> IO ()
tests test = do
        --  Memories
        let t :: (Eq b, Integral a, Show b,
                 Size (Column a), Size (Row a), Size (W a),
                 Size (W (Maybe (a,b))), Size (MUL a (W b)),
                 Size a, Rep a, Rep b) =>
                 String -> Gen (Maybe (a,b)) -> IO ()
            t str arb = testMatrixMemory test str arb

        t "X1xBool" (loop 10 $ dubSeq (arbitrary :: Gen (Maybe (X1,Bool))))
        t "X1xU4" (dubSeq (arbitrary :: Gen (Maybe (X1,U4))))
        t "X2xU4" (dubSeq (arbitrary :: Gen (Maybe (X2,U4))))
        t "X4xU4" (dubSeq (arbitrary :: Gen (Maybe (X4,U4))))
        t "X16xS10" (dubSeq (arbitrary :: Gen (Maybe (X256,S10))))

        let t :: (Eq a, Integral a, Show b,
                  Size (Column a), Size (Row a),
                  Size (W a), Size (W b), Size (MUL a (W b)), Size (W (Maybe (a,b))),
                  Size a, Rep a, Rep b, Eq b
                 ) =>
                  String -> Gen (Maybe (a,b),a) -> IO ()
            t str arb = testSyncMemory test str arb
        t "X1xBool" (loop 10 $ dubSeq (arbitrary :: Gen (Maybe (X1,Bool),X1)))
        t "X2xU4" (dubSeq (arbitrary :: Gen (Maybe (X2,U4),X2)))
        t "X4xU5" (dubSeq (arbitrary :: Gen (Maybe (X4,U5),X4)))

        let t :: (Eq a, Integral a, Show b,
                  Size (Column a), Size (Row a),
                  Size (W a), Size (W b), Size (MUL a (W b)), Size (W (Maybe (a,b))),
                  Size a, Rep a, Rep b, Eq b
                 ) =>
                  String -> Gen (Maybe (a,b),a) -> IO ()
            t str arb = testAsyncMemory test str arb
        t "X1xBool" (loop 10 $ dubSeq (arbitrary :: Gen (Maybe (X1,Bool),X1)))
        t "X2xU4" (dubSeq (arbitrary :: Gen (Maybe (X2,U4),X2)))
        t "X4xU5" (dubSeq (arbitrary :: Gen (Maybe (X4,U5),X4)))


        -- test ROM
        let t :: (Integral a, Size a, Eq a, Rep a,
                 Eq b, Show b, Rep b,
                 Size (Column a), Size (Row a),
                 Size (W a), Size (W b)) =>
                 String -> Gen (a,b) -> IO ()
            t str arb = testRomMemory test str arb

        t "X4xU5" (dubSeq (arbitrary :: Gen (X4,U5)))
        t "X4xU8" (dubSeq (arbitrary :: Gen (X4,U8)))
        t "X8xB"  (dubSeq (arbitrary :: Gen (X8,Bool)))

testAsyncMemory :: forall w1 w2 .
                  ( Integral w1, Size w1, Eq w1, Rep w1
                  , Eq w2, Show w2, Rep w2
                  , Size (Column w1), Size (Row w1), Size (W w1), Size (W w2)
                  , Size (W (Maybe (w1,w2))))
                => TestSeq -> String -> Gen (Maybe (w1,w2),w1) -> IO ()
testAsyncMemory (TestSeq test toList) tyName ws = do
    let (writes,reads) = unzip $ toList ws
        mem = asyncRead . writeMemory :: Seq (Maybe (w1,w2)) -> Seq w1 -> Seq w2

        driver = do
                outStdLogicVector "writes" (coerce (toSeq writes) :: Seq (Unsigned (W (Maybe (w1,w2)))))
                outStdLogicVector "reads" (coerce (toSeq reads) :: Seq (Unsigned (W w1)))
        dut = do
                ws <- inStdLogicVector "writes"
                rs <- inStdLogicVector "reads"
                let o0 = mem (coerce ws) (coerce rs)
                outStdLogicVector "o0" (coerce o0)
        res = do
                outStdLogicVector "o0" (coerce shallow)

        -- we look backwards in the writes, starting with what was written
        -- in the previous cycle. If we find a write for this address, we
        -- return that as the value. If we find a Nothing, we return Nothing.
        -- If we find a write to another address, we keep looking.
        -- In this way, writing an unknown value invalidates the whole memory.
        shallow :: Seq w2
        shallow = toSeq' $
                    [ List.head
                     [ val
                     | maybe_ab <- reverse $ take i (Nothing:writes) -- note this gets i-1 writes (up to previous cycle)
                     , let (filtr, val) = case maybe_ab of
                                            Nothing -> (True, Nothing)
                                            Just (a,b) -> (a == fromIntegral r,Just b)
                     , filtr
                     ]
                    | (i,r) <- zip [1..(length writes-1)] reads
                    ]

    test ("memory/async/" ++ tyName) (length writes) driver dut res

testSyncMemory :: forall w1 w2 .
                  ( Integral w1, Size w1, Eq w1, Rep w1
                  , Eq w2, Show w2, Rep w2
                  , Size (Column w1), Size (Row w1), Size (W w1), Size (W w2)
                  , Size (W (Maybe (w1,w2))))
               => TestSeq -> String -> Gen (Maybe (w1,w2),w1) -> IO ()
testSyncMemory (TestSeq test toList) tyName ws = do
    let (writes,reads) = unzip $ toList ws
        mem = syncRead . writeMemory :: Seq (Maybe (w1,w2)) -> Seq w1 -> Seq w2

        driver = do
                outStdLogicVector "writes" (coerce (toSeq writes) :: Seq (Unsigned (W (Maybe (w1,w2)))))
                outStdLogicVector "reads" (coerce (toSeq reads) :: Seq (Unsigned (W w1)))
        dut = do
                ws <- inStdLogicVector "writes"
                rs <- inStdLogicVector "reads"
                let o0 = mem (coerce ws) (coerce rs)
                outStdLogicVector "o0" (coerce o0)
        res = do
                outStdLogicVector "o0" (coerce shallow)

        -- see note in testAsyncMemory for semantics of generating expected output
        shallow :: Seq w2
        shallow = toSeq' $
                    [ Nothing ] ++
                    [ List.head
                     [ val
                     | maybe_ab <- reverse $ take i (Nothing:writes) -- note this gets i-1 writes (up to previous cycle)
                     , let (filtr, val) = case maybe_ab of
                                            Nothing -> (True, Nothing)
                                            Just (a,b) -> (a == fromIntegral r,Just b)
                     , filtr
                     ]
                    | (i,r) <- zip [1..(length writes-1)] reads
                    ]

    test ("memory/sync/" ++ tyName) (length writes) driver dut res


testMatrixMemory :: forall w1 w2 .
                    ( Integral w1, Size w1, Eq w1, Rep w1
                    , Eq w2, Show w2, Rep w2
                    , Size (Column w1), Size (Row w1), Size (W w1)
                    , Size (W (Maybe (w1,w2))), Size (MUL w1 (W w2)))
                 => TestSeq -> String -> Gen (Maybe (w1,w2)) -> IO ()
testMatrixMemory (TestSeq test toList) tyName ws = do
    let writes = toList ws
        mem = memoryToMatrix . writeMemory :: Seq (Maybe (w1,w2)) -> Seq (M.Matrix w1 w2)

        driver = do
                outStdLogicVector "i0" (coerce (toSeq writes) :: Seq (Unsigned (W (Maybe (w1,w2)))))
        dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = mem (coerce i0)
                outStdLogicVector "o0" (coerce o0)
        res = do
                outStdLogicVector "o0" (coerce shallow)

        -- see note in testAsyncMemory for semantics of generating expected output
        shallow :: Seq (M.Matrix w1 w2)
        shallow = pack
                $ M.matrix
                $ [ toSeq' $
                    [ List.head
                     [ val
                     | maybe_ab <- reverse $ take i (Nothing:writes) -- note this gets i-1 writes (up to previous cycle)
                     , let (filtr, val) = case maybe_ab of
                                            Nothing -> (True, Nothing)
                                            Just (a,b) -> (a == fromIntegral x,Just b)
                     , filtr
                     ]
                    | i <- [1..(length writes-1)]
                    ]
                  | x <- [0..(size (error "witness" :: w1) - 1 )]
                  ]

    test ("memory/matrix/" ++ tyName) (length writes) driver dut res
    return ()

testRomMemory :: forall w1 w2 .
                 ( Integral w1, Size w1, Eq w1, Rep w1
                 , Eq w2, Show w2, Rep w2
                 , Size (Column w1), Size (Row w1), Size (W w1), Size (W w2))
              => TestSeq
              -> String
              -> Gen (w1,w2)
              -> IO ()
testRomMemory (TestSeq test toList) tyName ws = do
    let (addr,vals) = unzip $ toList ws

        m :: Matrix w1 w2
        m = matrix $ take (size (error "" :: w1)) (cycle $ List.nub vals)
        driver = do
                outStdLogicVector "i0" (coerce (toSeq addr) :: Seq (Unsigned (W w1)))
        dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = mem (coerce i0)
                outStdLogicVector "o0" (coerce o0)
        res = do
                outStdLogicVector "o0" (coerce (toSeq [ m M.! a | a <- addr ] :: Seq w2))


        mem = funMap (\ a -> return (m M.! a)) :: Seq w1 -> Seq w2

    test ("memory/async/rom/" ++ tyName) (length addr) driver dut res
    return ()

mytest :: (Eq b, Integral a, Show b,
           Size (Column a), Size (Row a), Size (W a),
           Size (W (Maybe (a,b))), Size (MUL a (W b)),
           Size a, Rep a, Rep b)
       => String -> Gen (Maybe (a,b)) -> IO ()
mytest str arb = testMatrixMemory undefined str arb
