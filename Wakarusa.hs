{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies, FlexibleContexts, CPP #-}
import Language.KansasLava hiding ((&))

import Language.KansasLava.Universal
import Language.KansasLava.Fabric
import Data.Sized.Unsigned
import Data.Sized.Ix

import Control.Monad.Fix
import Data.Set as Set
import Data.Map as Map
import Data.Maybe 


------------------------------------------------------------------------

prog1 :: STMT ()
prog1 = do
        o0 :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled)
        loop <- LABEL
        o0  := 99
        GOTO loop

        FORK loop

fab1 = compileToFabric prog1

run1 :: Seq Int
run1 = runFabricWithDriver fab1 $ do
                inStdLogicVector "o0" :: Fabric (Seq Int)

-- 2nd output should be 9 | ...
prog2 :: STMT ()
prog2 = do
        o0 :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled)
        VAR v0  :: VAR Int           <- SIGNAL $ var 9
        loop <- LABEL
        o0 := v0
        GOTO loop
        FORK loop

fab2 = compileToFabric prog2

run2 :: Seq Int
run2 = runFabricWithDriver fab2 $ do
                inStdLogicVector "o0" :: Fabric (Seq Int)

prog2b :: STMT ()
prog2b = do
        o0 :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled)
        i0 :: EXPR Int  <- INPUT (inStdLogicVector "i0")
        loop <- LABEL
        o0 := i0 ||| GOTO loop
        FORK loop

fab2b = compileToFabric prog2b

run2b :: Seq Int -> Seq Int
run2b inp = runFabricWithDriver fab2b $ do
                outStdLogicVector "i0" inp
                inStdLogicVector "o0" :: Fabric (Seq Int)


prog3 :: STMT ()
prog3 = do
        o0     :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled . probeS "o0")
        VAR v0 :: VAR Int   <- SIGNAL $ var 9
        loop <- LABEL
        v0 := 10
        o0 := v0
        GOTO loop
        FORK loop

fab3 = compileToFabric prog3

run3 :: Seq Int
run3 = runFabricWithDriver fab3 $ do
                inStdLogicVector "o0" :: Fabric (Seq Int)


prog4 :: STMT ()
prog4 = do
        o0     :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled . probeS "o0")
        VAR v0 :: VAR Int   <- SIGNAL $ var 0
        loop <- LABEL
        (v0 := v0 + 1) 
          ||| (o0 := v0) 
          ||| GOTO loop
        FORK loop

(&) m1 m2 = m1 >> m2

fab4 = compileToFabric prog4

run4 :: Seq Int
run4 = runFabricWithDriver fab4 $ do
                inStdLogicVector "o0" :: Fabric (Seq Int)

prog5 :: STMT ()
prog5 = do
        o0     :: REG Int   <- OUTPUT (outStdLogicVector "o0" . delayEnabled . probeS "o0")
        VAR v0 :: VAR Int   <- SIGNAL $ var 0
        loop <- LABEL
        (v0 := v0 + 1) 
        (o0 := v0) 
        GOTO loop
        FORK loop

fab5 = compileToFabric prog5

run5 :: Seq Int
run5 = runFabricWithDriver fab5 $ do
                inStdLogicVector "o0" :: Fabric (Seq Int)


prog6 :: STMT ()
prog6 = do
--        rAckBox :: ReadableAckBox Int <- connectReadableAckBox "iA" "oA"
        wAckBox@(WritableAckBox oB iB) :: WritableAckBox Int <- connectWritableAckBox "out" "ack"
        VAR v0 :: VAR Int   <- SIGNAL $ var 1
        loop <- LABEL
        putAckBox wAckBox v0 
                ||| (v0 := v0 + 1)
                ||| GOTO loop

        FORK loop

fab6 = compileToFabric prog6

run6 :: Patch () (Seq (Enabled Int))
              () (Seq Ack)
run6 ~(_,outAck) = runFabricWithDriver fab6 $ do
                outStdLogic "ack" outAck
                out <- inStdLogicVector "out"
                return ((),out)
                
                
prog7 :: STMT ()
prog7 = do
        o0 :: REG Int    <- OUTPUT (outStdLogicVector "o0" . enabledVal)
        i0 :: EXPR Int   <- INPUT (inStdLogicVector "i0")

        VAR v0 :: VAR Int   <- SIGNAL $ var 1

        loop <- LABEL
        o0 := i0 + v0
{-
                ||| (OP1 (bitNot) (OP0 high) :? GOTO loop)
--        putAckBox wAckBox v0 (return ())
-}
                ||| (v0 := v0 + 1)
                ||| GOTO loop



        FORK loop

fab7 = compileToFabric prog7

run7 :: Seq Int -> Seq Int
run7 inp = runFabricWithDriver fab7 $ do
        outStdLogicVector "i0" inp
        out <-inStdLogicVector "o0"
        return out


prog8 :: STMT ()
prog8 = do
        rAckBox :: ReadableAckBox Int <- connectReadableAckBox "iA" "oA"
        wAckBox :: WritableAckBox Int <- connectWritableAckBox "out" "ack"
        VAR v0 :: VAR Int   <- SIGNAL $ undefinedVar
        loop <- LABEL
        takeAckBox (v0 :=) rAckBox 
        putAckBox wAckBox v0 
                ||| GOTO loop
        FORK loop

fab8 = compileToFabric prog8

run8 :: Patch (Seq (Enabled Int)) (Seq (Enabled Int))
              (Seq Ack) (Seq Ack)
run8 ~(inp,outAck) = runFabricWithDriver fab8 $ do
        outStdLogicVector "iA" inp
        inAck <- inStdLogic "oA"
        outStdLogic "ack" outAck
        out <- inStdLogicVector "out"
        return (inAck,out)

infix 3 !%

(!%) :: (Variable var) => MEM ix a -> EXPR ix -> var a
(!%) = undefined



prog9 :: STMT ()
prog9 = do
        mem :: Memory X1 Int <- memory
        o0     :: REG Int   <- OUTPUT (outStdLogicVector "o0" . enabledVal)
        VAR v0 :: VAR Int   <- SIGNAL $ var 0

        loop <- LABEL
        o0 := v0 ||| v0 := v0 + 1 ||| writeM mem := OP2 (curry pack) 0 v0
--        o0 := 294
        o0 := OP2 asyncRead (readM mem) 0 
                ||| GOTO loop

        FORK loop

        return ()

fab9 = compileToFabric prog9

run9 :: Seq Int
run9 = runFabricWithDriver fab9 $ do
                inStdLogicVector "o0" :: Fabric (Seq Int)

prog10 :: STMT ()
prog10 = do
        start <- LABEL

--        mem :: Memory X16 Int <- memory
        o0     :: REG Int   <- OUTPUT (outStdLogicVector "o0" . enabledVal)
{-
        VAR v0 :: VAR Int   <- SIGNAL $ var 0
        VAR i  :: VAR X16   <- SIGNAL $ var 0        

        loop1 <- LABEL
        writeM mem := OP2 (curry pack) i v0
        i := i + 1
        v0 := v0 + 1
        o0 := OP1 (unsigned) i
        o0 := OP1 (\ b -> mux b (0,1)) (OP2 (.<.) i 10)
-}
        (OP0 high) :? o0 := 4000
        (OP0 low) :? o0 := 5000
{-
        (OP2 (.<.) i 10) :?
                (o0 := 999 ||| GOTO loop1)

        o0 := 1000

        i := 0
        loop <- LABEL
        o0 := OP2 asyncRead (readM mem) 0
--        i := i + 1
        (OP2 (.<.) i (OP0 $ pureS maxBound)) :? GOTO loop
-}
        FORK start

        return ()

fab10 = compileToFabric prog10

run10 :: Seq Int
run10 = runFabricWithDriver fab10 $ do
                inStdLogicVector "o0" :: Fabric (Seq Int)


data FOO = FOO Int deriving Show

foo = FOO

#define FOO foo __LINE__

fooy = print (FOO)

{-             
progX :: STMT ()
progX = do
--        rAckBox :: ReadableAckBox Int <- connectReadableAckBox "iA" "oA"
--        wAckBox :: WritableAckBox Int <- connectWritableAckBox "oB" "iB"

        s0 :: REG Int   <- OUTPUT (outStdLogicVector "XX")


        VAR v0  :: VAR Int           <- SIGNAL $ var 0
--        ARR a0  :: ARR X8 Int        <- ARRAY

        loop <- LABEL
        v0 := v0 + 1
        s0 := v0
--        takeAckBox rAckBox (v0 :=)
--        STEP
--        putAckBox wAckBox v0 (return ()) 
--        STEP
        GOTO loop


{-
        ST a0 0 99
        a0 := OP2 pack 0 99
        a0 := WT 0 99

        v0 := LD a0 0
-}


        FORK loop 

{-
fab0 = compileToFabric prog1
fab1 ::        (Seq (Enabled Int), Seq Ack)
     -> Fabric (Seq Ack, Seq (Enabled Int))
fab1 ~(inp,outAck) = do
        outStdLogicVector "iA" inp
        inAck <- inStdLogic "oA"
        outStdLogic "iB" outAck
        out <- inStdLogicVector "oB"
        return (inAck,out)

test args = runFabricWithDriver fab0 (fab1 args)
xs = take 1000 $ runAckBoxP (shallowAckBoxBridge (cycle [1,2,3,0],cycle [0,2,1]) $$ test) [1..1000]
-}

--t = fromJust (head [ fromUni p |  ("o0",p) <- snd (runFabric fab [("i0",toUni (toS [0..] :: Seq Int))]) ]) :: (Seq (Enabled Int))

          
          
{-
--        bar <- LABEL
--        oB := OP0 (pureS Nothing)
--        GOTO bar

{-        
        rec loop <- thread $ do
                PAR [ -- v0 := v0 + 1
                     o0 := i0
                    , GOTO loop
                    ]

-}
{-      

        rec loop <- thread $ do
                v0 := i0
--                v0 := v0 + 1
--                (OP1 (.==. 104) v0) :? do
                o0 := v0
                GOTO loop
-}
-}
-}          