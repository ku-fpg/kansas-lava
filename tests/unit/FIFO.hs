{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module FIFO where

import Language.KansasLava
--import Language.KansasLava.Testing.Thunk

import Utils
import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Ix

tests :: TestSeq -> IO ()
tests test = do
        -- testing FIFOs

        let t :: forall w sz sz1 .
                 (Eq w, Rep w, Show w,
                               Size (W w),
                  sz1 ~ ADD sz X1,
                               Size (ADD (W w) X1),     --- Hmm
                  Size sz, Size sz1,
                  Rep sz, Rep sz1,
                  Num w, Num sz, Num sz1)
                 => String -> Gen (Bool,Maybe w) -> Witness sz -> IO ()
            t str arb = testFIFO test str (dubSeq arb)

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
                               Size (W w),
                               Size (ADD (W w) X1),     --- Hmm
                               Size sz, Size sz1,
                               Rep sz, Rep sz1,                 Num w,
                               Num sz, Num sz1)
        => TestSeq -> String -> Gen (Bool,Maybe w) -> Witness sz -> IO ()
testFIFO (TestSeq test toL) tyName ws wit = do
        let outBools :: [Bool]
            vals    :: [Maybe w]
            (outBools,vals) = unzip $ toL ws

        let
            cir = pack . fifo wit low . unpack :: Seq (Enabled w, Bool) -> Seq (Bool, Enabled w)

            driver = do
                outStdLogic      "flag"         $ toSeq outBools
                let vals' :: Seq (Enabled w)
                    vals' = toHandShaken vals (toSeq (map fst specRes))
                outStdLogicVector "vals"        ((enabledVal vals'))
                outStdLogic       "vals_en"     (isEnabled vals')

            dut = do
                flag    <- inStdLogic "flag"
                vls    <- inStdLogicVector "vals"
                vals_en <- inStdLogic "vals_en"
                let (ack,res') = unpack $ cir $ (pack (packEnabled vals_en (vls), flag) :: Seq (Enabled w,Bool))
                outStdLogicVector "res"    ((enabledVal res'))
                outStdLogic "res_en" (isEnabled res')
                outStdLogic "ack" ack

            specRes ::[(Bool,Enabled w)]
            specRes = fifoSpec vals (cycle outBools) []

            res :: Fabric ()
            res = do
                let specResSeq = toSeq $ map snd $ specRes
                outStdLogicVector "res" ((enabledVal specResSeq))
                outStdLogic "res_en" (isEnabled specResSeq)
                outStdLogic "ack" undefinedS


            fifoSize :: Int
            fifoSize = size (error "witness" :: sz)

--            fifoSpec b c d | trace (show ("fifoSpec",take 10 b, take 10 c,d)) False = undefined
            fifoSpec :: [Maybe w] -> [Bool] -> [Maybe w] -> [(Bool,Maybe w)]

--            fifoSpec _ _ state
--                        | -- length [ () | Just _ <- state ] == fifoSize &&
--                             trace (show ("fifoLen",length [ () | Just _ <- state ])) False = undefined

            fifoSpec [] _ _ = error "fifoSpec: no value to enqueue"
            fifoSpec (val:vals') outs state
                        | length [ () | Just _ <- state ] < fifoSize
                        = fifoSpec2 vals' outs (val:state) True
                          -- FIFO is full, so do not accept
                        | otherwise
                        = fifoSpec2  (val:vals') outs (Nothing:state) False

            fifoSpec2 _ [] _ _ = error "fifoSpec2: no ready/output signal"
            fifoSpec2 vals' (ready:outs) state ack =
                    case [ x | Just x <- reverse $ drop 3 state ] of
                        [] -> (ack,Nothing)   : fifoSpec vals' outs state
                        (x:_) -> (ack,Just x) : fifoSpec  vals' outs (nextState state ready)

            nextState state False = state
            nextState state True  = take 3 state ++ init [ Just x | Just x <- drop 3 state ]

        test ("fifo/sz_" ++ show fifoSize ++ "/" ++ tyName) (50) {-(length vals)-} driver dut res

{-
        --------------------------------------------------------------------------------------------

        -- A test for as fast as you can write

        let vals2 = cycle [ Just x | Just x <- vals ]

        let thu2 :: Thunk (CSeq () (Bool,Enabled w))
            thu2 = Thunk cir
                        (\ f -> let inp = toHandShaken vals2 back
                                    (back,res) = unpack $ f $ pack (inp,high)
                                 in pack (back,res)
                        )

        let res2 :: Seq (Bool,Enabled w)
            res2 = pack (undefinedS,toSeq $ fifoSpec vals2 (repeat True) [])


        test ("fifo/speed/sz_" ++ show fifoSize ++ "/" ++ tyName) (length vals) thu2 res2

        --------------------------------------------------------------------------------------------
        -- A test for phasing the fifo

        let vals3 = vals

        let accept = concat [ if (x `div` (fifoSize * 32)) `mod` 2 == 0
                              then if not v then [v,v,v] else [v]
                              else if     v then [v,v,v] else [v]
                            | (x,v) <- zip [0..] (cycle $ outBools)
                            ]

        let thu3 :: Thunk (CSeq () (Bool,Enabled w))
            thu3 = Thunk cir
                        (\ f -> let inp = toHandShaken vals3 back
                                    (back,res) = unpack $ f $ pack (inp,toSeq accept)
                                 in pack (back,res)
                        )

        let res3 :: Seq (Bool,Enabled w)
            res3 = pack (undefinedS,toSeq $ fifoSpec vals3 accept [])

        test ("fifo/phase/sz_" ++ show fifoSize ++ "/" ++ tyName) (length vals) thu3 res3

        --------------------------------------------------------------------------------------------
-}
        return ()
