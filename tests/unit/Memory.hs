{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Memory where

import Language.KansasLava
import Language.KansasLava.Stream as S
import Language.KansasLava.Testing.Thunk

import Utils
import Data.Sized.Unsigned
import Data.Sized.Matrix as M hiding (length)
import Data.Sized.Signed
import Data.Sized.Arith
import Data.Sized.Ix

import Data.List as List

import Debug.Trace

tests :: TestSeq -> IO ()
tests test = do
    --  Memories
        let t :: (Eq b, Integral a, Show b,
                 Size (Column a), Size (Row a),
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


        -- test ROM
        
        let t str arb = testRomMemory test str arb
        
        t "X4xU5" (dubSeq (arbitrary :: Gen (X4,U5))) 
        t "X4xU8" (dubSeq (arbitrary :: Gen (X4,U8))) 


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
        test ("memory/async/" ++ tyName) (length writes) thu res

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
        test ("memory/sync/" ++ tyName) (length writes) thu res


testMatrixMemory :: forall w1 w2 . (Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Size (Column w1), Size (Row w1), Rep w2) => TestSeq -> String -> Gen (Maybe (w1,w2)) -> IO ()
testMatrixMemory (TestSeq test toList) tyName ws = do
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
        test ("memory/matrix/" ++ tyName) (length writes) thu (pack res)
        return ()

testRomMemory :: forall w1 w2 . (Integral w1, Size w1, Eq w1, Rep w1, Eq w2, Show w2, Rep w2, Size (Column w1), Size (Row w1)) => TestSeq -> String -> Gen (w1,w2) -> IO ()
testRomMemory (TestSeq test toList) tyName ws = do
        let (addr,vals) = unzip $ toList ws

        let m :: Matrix w1 w2
            m = matrix $ take (size (error "" :: w1)) (cycle $ List.nub vals)

        print $ take 10 addr
        print $ take 10 vals
        print $ m

        let mem = funMap (\ a -> return (m M.! a)) :: Seq w1 -> Seq w2

        let thu = Thunk mem
                $ \ mem -> mem (toSeq addr)

        let res :: Seq w2
            res = toSeq [ m M.! a | a <- addr ]
            
        test ("memory/async/rom/" ++ tyName) (length addr) thu res

        print ()
