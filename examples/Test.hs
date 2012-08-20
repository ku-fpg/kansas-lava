{-# LANGUAGE ScopedTypeVariables #-}
import Language.KansasLava
import Control.Monad.Fix
import System.IO

import Data.Word
import Data.Sized.Ix
import Data.Sized.Unsigned
import Control.Concurrent.STM

main = do
--        v <- newEmptyTMVarIO
--        s <- readTMVarS v

        let dut = do {-hWriteFabric stdout
                        [ IN (inStdLogic "x0"       :: SuperFabric IO (Seq Bool))
                        , IN (inStdLogic "x1"       :: SuperFabric IO (Seq Bool))
                        , IN (inStdLogicVector "x2" :: SuperFabric IO (Seq U8))
                        ] -}
                     hReadFabric stdin
                       [ OUT (outStdLogic "o0"       :: Seq Bool -> SuperFabric IO ())
                       , OUT (outStdLogic "o1"       :: Seq Bool -> SuperFabric IO ())
                       , OUT (outStdLogicVector "o2" :: Seq U2   -> SuperFabric IO ())
                       ]

            driver = do
{-
                    outStdLogic "x0" (low :: Seq Bool)
                    outStdLogic "x1" (high :: Seq Bool)
                    outStdLogicVector "x2" (2 :: Seq U2)
-}
                    a <- inStdLogic "o0" :: SuperFabric IO (Seq Bool)
                    b <- inStdLogic "o1" :: SuperFabric IO (Seq Bool)
                    c <- inStdLogicVector "o2" :: SuperFabric IO (Seq U2)
                    return (pack (a,b,c) :: Seq (Bool,Bool,U2))
        r <- runFabricWithDriver dut driver
        print "Hello"
        print r
--        sequence_ [ atomically $ putTMVar v $ optX $ return $ () | _ <- [1..100]]

{-
{-
        v <- newEmptyTMVarIO
        s <- readTMVarS v
        print ""
        writeFabric "foo" $ example s
        sequence_ [ atomically $ putTMVar v $ optX $ return $ () | _ <- [1..100]]
-}
        (x1,x2,x3,x4) <- readFabric "foo" $ example2
        print $ takeS 10 x1
        print $ takeS 10 x2
        print $ takeS 10 x3
        print $ takeS 10 x4


example :: Signal CLK () -> Fabric ()
example s = do
        outStdLogicVector "stopper" s
        outStdLogic "o0" (toS $ cycle [True,False,False])
        outStdLogic "o1" (toS $ cycle [True])
        outStdLogic "o2" (toS $ cycle [False])
        outStdLogicVector "oX" (toS $ cycle [0..255 :: Word8])


example2 :: Fabric (Seq Bool, Seq U2, Seq Bool, Seq Bool)
example2 = do
        x <- inStdLogic "i0"       :: Fabric (Seq Bool)
        y <- inStdLogicVector "i1" :: Fabric (Seq U2)
        x1 <- inStdLogic "i3"       :: Fabric (Seq Bool)
        x2 <- inStdLogic "i4"       :: Fabric (Seq Bool)
        return(x,y,x1,x2)
-}
bottom :: ()
bottom = error "opps"