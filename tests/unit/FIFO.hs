{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module FIFO where

import Language.KansasLava
import Language.KansasLava.Stream as S
import Language.KansasLava.Testing.Thunk

import Utils
import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Arith
import Data.Sized.Ix

import Debug.Trace

tests :: TestSeq -> IO ()
tests test = do
        -- testing FIFOs

        let t str arb = testFIFO test str (dubSeq arb)
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5)) (Witness :: Witness X1)
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5)) (Witness :: Witness X2)
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5)) (Witness :: Witness X3)
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5)) (Witness :: Witness X4)
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5)) (Witness :: Witness X5)
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5)) (Witness :: Witness X6)
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5)) (Witness :: Witness X7)
        t "U5"  (arbitrary :: Gen (Bool,Maybe U5)) (Witness :: Witness X8)

-- Need to fix memories first
--        t "U1"  (arbitrary :: Gen (Bool,Maybe U1)) (Witness :: Witness X1)
--        t "Bool"  (arbitrary :: Gen (Bool,Maybe Bool)) (Witness :: Witness X1)


testFIFO :: forall w sz sz1 . (Eq w, Rep w, Show w,
                               sz1 ~ ADD sz X1,
                               Size sz, Size sz1, 
                               Rep sz, Rep sz1,                 Num w,
                               Num sz, Num sz1)
        => TestSeq -> String -> Gen (Bool,Maybe w) -> Witness sz -> IO ()
testFIFO (TestSeq test toList) tyName ws wit = do
        let outBools :: [Bool]
            vals    :: [Maybe w]
            (outBools,vals) = unzip $ toList ws

        let cir = fifo wit low :: (Seq (Enabled w), Seq Bool) -> (Seq Bool, Seq (Enabled w))
        let thu :: Thunk (CSeq () (Bool,Enabled w))
            thu = Thunk cir
                        (\ f -> let inp = toHandShaken vals back
                                    (back,res) = f (inp,toSeq outBools)
                                 in pack (back,res)
                        )

        let fifoSize :: Int
            fifoSize = size (error "witness" :: sz)

        let -- fifoSpec b c d | trace (show ("fifoSpec",take 10 b, take 10 c,d)) False = undefined
            fifoSpec :: [Maybe w] -> [Bool] -> [Maybe w] -> [Maybe w]
--            fifoSpec _ _ state
--                        | length [ () | Just _ <- state ] == fifoSize
--                                && trace (show ("fifoLen",fifoSize)) False = undefined
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

        test ("fifo/sz_" ++ show fifoSize ++ "/" ++ tyName) (length vals) thu res

        --------------------------------------------------------------------------------------------

        -- A test for as fast as you can write

        let vals2 = cycle [ Just x | Just x <- vals ]

        let thu2 :: Thunk (CSeq () (Bool,Enabled w))
            thu2 = Thunk cir
                        (\ f -> let inp = toHandShaken vals2 back
                                    (back,res) = f (inp,high)
                                 in pack (back,res)
                        )

        let res2 :: Seq (Bool,Enabled w)
            res2 = pack (undefinedS,toSeq $ fifoSpec vals2 (repeat True) [])


        test ("fifo/speed/sz_" ++ show fifoSize ++ "/" ++ tyName) (length vals) thu2 res2

        --------------------------------------------------------------------------------------------
        return ()
