{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Memory where

import Language.KansasLava
import Language.KansasLava.Test

import Data.Sized.Arith
import Data.Sized.Matrix as M hiding (length)
import Data.Sized.Signed
import Data.Sized.Unsigned

import Data.List as List

type List a = [a]

tests :: TestSeq -> IO ()
tests test = do
        --  Memories
        let t1 :: (Eq b, Integral a, Show b,
                 Size (W a),
                 Size (W (Maybe (a,b))), Size (MUL a (W b)),
                 Size a, Rep a, Rep b) =>
                 String -> List (Maybe (a,b)) -> IO ()
            t1 str arb = testMatrixMemory test str arb

        t1 "X1xBool" ((finiteCases 1000 :: List (Maybe (X1,Bool))))
        t1 "X1xU4" ((finiteCases 1000 :: List (Maybe (X1,U4))))
        t1 "X2xU4" ((finiteCases 1000 :: List (Maybe (X2,U4))))
        t1 "X4xU4" ((finiteCases 1000 :: List (Maybe (X4,U4))))
        t1 "X16xS10" ((finiteCases 1000 :: List (Maybe (X256,S10))))

        let t2 :: (Eq a, Integral a, Show b,
                  Size (W a), Size (W b), Size (MUL a (W b)), Size (W (Maybe (a,b))),
                  Size a, Rep a, Rep b, Eq b
                 ) =>
                  String -> List (Maybe (a,b),a) -> IO ()
            t2 str arb = testSyncMemory test str arb
        t2 "X1xBool" (finiteCases 1000 :: List (Maybe (X1,Bool),X1))
        t2 "X2xU4" ((finiteCases 1000 :: List (Maybe (X2,U4),X2)))
        t2 "X4xU5" ((finiteCases 1000 :: List (Maybe (X4,U5),X4)))

        let t3 :: (Eq a, Integral a, Show b,
                  Size (W a), Size (W b), Size (MUL a (W b)), Size (W (Maybe (a,b))),
                  Size a, Rep a, Rep b, Eq b
                 ) =>
                  String -> List (Maybe (a,b),a) -> IO ()
            t3 str arb = testAsyncMemory test str arb
        t3 "X1xBool" ((finiteCases 1000 :: List (Maybe (X1,Bool),X1)))
        t3 "X2xU4" ((finiteCases 1000 :: List (Maybe (X2,U4),X2)))
        t3 "X4xU5" ((finiteCases 1000 :: List (Maybe (X4,U5),X4)))

        -- test ROM
        let t4 :: (Integral a, Size a, Eq a, Rep a,
                 Eq b, Show b, Rep b,
                 Size (W a), Size (W b)) =>
                 String -> List (a,b) -> IO ()
            t4 str arb = testRomMemory test str arb

        t4 "X4xU5" ((finiteCases 1000 :: List (X4,U5)))
        t4 "X4xU8" ((finiteCases 1000 :: List (X4,U8)))
        t4 "X8xB"  ((finiteCases 1000 :: List (X8,Bool)))


testAsyncMemory :: forall w1 w2 .
                  ( Integral w1, Size w1, Eq w1, Rep w1
                  , Eq w2, Show w2, Rep w2
                  , Size (W w1), Size (W w2)
                  , Size (W (Maybe (w1,w2))))
                => TestSeq -> String -> List (Maybe (w1,w2),w1) -> IO ()
testAsyncMemory (TestSeq test _) tyName ws = do
    let (writes,rds) = unzip $ ws
        mem = asyncRead . writeMemory :: Seq (Maybe (w1,w2)) -> Seq w1 -> Seq w2

        driver = do
                outStdLogicVector "writes" (toS writes)
                outStdLogicVector "reads" (toS rds)
        dut = do
                ws' <- inStdLogicVector "writes"
                rs <- inStdLogicVector "reads"
                let o0 = mem (ws') (rs)
                outStdLogicVector "o0" (o0)
        res = shallow

        -- we look backwards in the writes, starting with what was written
        -- in the previous cycle. If we find a write for this address, we
        -- return that as the value. If we find a Nothing, we return Nothing.
        -- If we find a write to another address, we keep looking.
        -- In this way, writing an unknown value invalidates the whole memory.
        shallow :: Seq w2
        shallow = toS' $
                    [ List.head
                     [ val
                     | maybe_ab <- reverse $ take i (Nothing:writes) -- note this gets i-1 writes (up to previous cycle)
                     , let (filtr, val) = case maybe_ab of
                                            Nothing -> (True, Nothing)
                                            Just (a,b) -> (a == fromIntegral r,Just b)
                     , filtr
                     ]
                    | (i,r) <- zip [1..(length writes-1)] rds
                    ]

    test ("memory/async/" ++ tyName) (length writes) dut (driver >> matchExpected "o0" res)

testSyncMemory :: forall w1 w2 .
                  ( Integral w1, Size w1, Eq w1, Rep w1
                  , Eq w2, Show w2, Rep w2
                  , Size (W w1), Size (W w2)
                  , Size (W (Maybe (w1,w2))))
               => TestSeq -> String -> List (Maybe (w1,w2),w1) -> IO ()
testSyncMemory (TestSeq test _) tyName ws = do
    let (writes,rds) = unzip $  ws
        mem = syncRead . writeMemory :: Seq (Maybe (w1,w2)) -> Seq w1 -> Seq w2

        driver = do
                outStdLogicVector "writes" (toS writes)
                outStdLogicVector "reads" (toS rds)
        dut = do
                ws' <- inStdLogicVector "writes"
                rs <- inStdLogicVector "reads"
                let o0 = mem (ws') (rs)
                outStdLogicVector "o0" (o0)
        res = shallow

        -- see note in testAsyncMemory for semantics of generating expected output
        shallow :: Seq w2
        shallow = toS' $
                    [ Nothing ] ++
                    [ List.head
                     [ val
                     | maybe_ab <- reverse $ take i (Nothing:writes) -- note this gets i-1 writes (up to previous cycle)
                     , let (filtr, val) = case maybe_ab of
                                            Nothing -> (True, Nothing)
                                            Just (a,b) -> (a == fromIntegral r,Just b)
                     , filtr
                     ]
                    | (i,r) <- zip [1..(length writes-1)] rds
                    ]

    test ("memory/sync/" ++ tyName) (length writes) dut (driver >> matchExpected "o0" res)

testMatrixMemory :: forall w1 w2 .
                    ( Integral w1, Size w1, Eq w1, Rep w1
                    , Eq w2, Show w2, Rep w2
                    , Size (W w1)
                    , Size (W (Maybe (w1,w2))), Size (MUL w1 (W w2)))
                 => TestSeq -> String -> List (Maybe (w1,w2)) -> IO ()
testMatrixMemory (TestSeq test _) tyName ws = do
    let writes = ws
        mem = memoryToMatrix . writeMemory :: Seq (Maybe (w1,w2)) -> Seq (M.Matrix w1 w2)

        driver = do
                outStdLogicVector "i0" (toS writes)
        dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = mem (i0)
                outStdLogicVector "o0" (o0)
        res = shallow

        -- see note in testAsyncMemory for semantics of generating expected output
        shallow :: Seq (M.Matrix w1 w2)
        shallow = pack
                $ M.matrix
                $ [ toS' $
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

    test ("memory/matrix/" ++ tyName) (length writes) dut (driver >> matchExpected "o0" res)
    return ()

testRomMemory :: forall w1 w2 .
                 ( Integral w1, Size w1, Eq w1, Rep w1
                 , Eq w2, Show w2, Rep w2
                 , Size (W w1), Size (W w2))
              => TestSeq
              -> String
              -> List (w1,w2)
              -> IO ()
testRomMemory (TestSeq test _) tyName ws = do
    let (addr,vals) = unzip $ ws

        m :: Matrix w1 w2
        m = matrix $ take (size (error "" :: w1)) (cycle $ List.nub vals)
        driver = do
                outStdLogicVector "i0" (toS addr)
        dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = mem (i0)
                outStdLogicVector "o0" (o0)
        res = toS [ m M.! a | a <- addr ] :: Seq w2


        mem = funMap (\ a -> return (m M.! a)) :: Seq w1 -> Seq w2

    test ("memory/async/rom/" ++ tyName) (length addr) dut (driver >> matchExpected "o0" res)
    return ()
