{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification, DataKinds, TypeOperators #-}

module Memory where

import Language.KansasLava
import Language.KansasLava.Test

import Data.Sized.Fin
import Data.Sized.Matrix as M hiding (length)
import Data.Sized.Signed
import Data.Sized.Unsigned

import Data.List as List
import Data.Array.IArray

import GHC.TypeLits

type List a = [a]

type instance (1 + 1) = 2
type instance (1 + 2) = 3
type instance (1 + 3) = 4
type instance (1 + 4) = 5
type instance (1 + 5) = 6
type instance (1 + 6) = 7
type instance (1 + 7) = 8
type instance (1 + 8) = 9
type instance (1 + 9) = 10
type instance (1 + 266) = 267

type instance (2 + 4) = 6
type instance (4 + 4) = 8
type instance (4 + 5) = 9

type instance (256 + 10) = 266

type instance (2 * 4) = 8
type instance (4 * 4) = 16
type instance (4 * 5) = 20
type instance (256 * 10) = 2560




tests :: TestSeq -> IO ()
tests test = do
        --  Memories
        let t1 :: (SingI a, Eq b, Show b, Rep b,
                   SingI (W (Maybe (Fin a,b))), SingI (a * (W b))
                   ) =>
                 String -> List (Maybe (Fin a,b)) -> IO ()
            t1 str arb = testMatrixMemory test str arb

        t1 "X1xBool" ((finiteCases 1000 :: List (Maybe (Fin 1,Bool))))
        t1 "X1xU4" ((finiteCases 1000 :: List (Maybe (Fin 1,U4))))
        t1 "X2xU4" ((finiteCases 1000 :: List (Maybe (Fin 2,U4))))
        t1 "X4xU4" ((finiteCases 1000 :: List (Maybe (Fin 4,U4))))
        t1 "X16xS10" ((finiteCases 1000 :: List (Maybe (Fin 256,S10))))

        let t2 :: (Show b, Rep b, Eq b,
                   SingI a, SingI (W b), SingI (a * (W b)), SingI (W (Maybe (Fin a,b)))
                  ) =>
                  String -> List (Maybe (Fin a,b), Fin a) -> IO ()
            t2 str arb = testSyncMemory test str arb
        t2 "X1xBool" (finiteCases 1000 :: List (Maybe (Fin 1,Bool), Fin 1))
        t2 "X2xU4" ((finiteCases 1000 :: List (Maybe (Fin 2,U4), Fin 2)))
        t2 "X4xU5" ((finiteCases 1000 :: List (Maybe (Fin 4,U5), Fin 4)))

        let t3 :: (Show b, Rep b, Eq b,
                  SingI a, SingI (W b), SingI (a * (W b)), SingI (W (Maybe (Fin a,b)))
                  ) =>
                  String -> List (Maybe (Fin a,b),Fin a) -> IO ()
            t3 str arb = testAsyncMemory test str arb
        t3 "X1xBool" ((finiteCases 1000 :: List (Maybe (Fin 1,Bool), Fin 1)))
        t3 "X2xU4" ((finiteCases 1000 :: List (Maybe (Fin 2,U4), Fin 2)))
        t3 "X4xU5" ((finiteCases 1000 :: List (Maybe (Fin 4,U5), Fin 4)))

        -- test ROM
        let t4 :: (SingI a, Eq b, Show b, Rep b, SingI (W b)) =>
                  String -> List (Fin a,b) -> IO ()
            t4 str arb = testRomMemory test str arb

        t4 "X4xU5" ((finiteCases 1000 :: List (Fin 4,U5)))
        t4 "X4xU8" ((finiteCases 1000 :: List (Fin 4,U8)))
        t4 "X8xB"  ((finiteCases 1000 :: List (Fin 8,Bool)))


testAsyncMemory :: forall w1 w2 .
                   ( SingI w1,
                     Eq w2, Show w2, Rep w2 , SingI (W w2),
                     SingI (W (Maybe (Fin w1,w2)))
                   ) => TestSeq -> String -> List (Maybe (Fin w1,w2), Fin w1) -> IO ()
testAsyncMemory (TestSeq test _) tyName ws = do
    let (writes,rds) = unzip $ ws
        mem = asyncRead . writeMemory :: Seq (Maybe (Fin w1,w2)) -> Seq (Fin w1) -> Seq w2

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
                  ( SingI w1,
                    Eq w2, Show w2, Rep w2, SingI (W w2),
                    SingI (W (Maybe (Fin w1,w2))))
                  => TestSeq -> String -> List (Maybe (Fin w1,w2), Fin w1) -> IO ()
testSyncMemory (TestSeq test _) tyName ws = do
    let (writes,rds) = unzip $  ws
        mem = syncRead . writeMemory :: Seq (Maybe (Fin w1,w2)) -> Seq (Fin w1) -> Seq w2

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
                    (SingI w1, Eq w2, Show w2, Rep w2,
                     SingI (W (Maybe (Fin w1,w2))), SingI (w1 * (W w2)))
                   => TestSeq -> String -> List (Maybe (Fin w1,w2)) -> IO ()
testMatrixMemory (TestSeq test _) tyName ws = do
    let writes = ws
        mem = memoryToMatrix . writeMemory :: Seq (Maybe (Fin w1,w2)) -> Seq (M.Vector w1 w2)

        driver = do
                outStdLogicVector "i0" (toS writes)
        dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = mem (i0)
                outStdLogicVector "o0" (o0)
        res = shallow

        -- see note in testAsyncMemory for semantics of generating expected output
        shallow :: Seq (M.Vector w1 w2)
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
                  | x <- [0..(size (error "witness" :: (Fin w1)) - 1 )]
                  ]

    test ("memory/matrix/" ++ tyName) (length writes) dut (driver >> matchExpected "o0" res)
    return ()

testRomMemory :: forall w1 w2 .
                 (SingI w1, Eq w2, Show w2, Rep w2, SingI (W w2))
                 => TestSeq
              -> String
              -> List (Fin w1, w2)
              -> IO ()
testRomMemory (TestSeq test _) tyName ws = do
    let (addr,vals) = unzip $ ws

        m :: Vector w1 w2
        m = matrix $ take (size (error "" :: (Fin w1))) (cycle $ List.nub vals)
        driver = do
                outStdLogicVector "i0" (toS addr)
        dut = do
                i0 <- inStdLogicVector "i0"
                let o0 = mem (i0)
                outStdLogicVector "o0" (o0)
        res = toS [ m ! a | a <- addr ] :: Seq w2


        mem = funMap (\ a -> return (m ! a)) :: Seq (Fin w1) -> Seq w2

    test ("memory/async/rom/" ++ tyName) (length addr) dut (driver >> matchExpected "o0" res)
    return ()
