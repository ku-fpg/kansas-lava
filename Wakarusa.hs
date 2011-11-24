{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies, FlexibleContexts #-}
import Language.KansasLava

import Language.KansasLava.Universal
import Language.KansasLava.Fabric
import Data.Sized.Unsigned
import Data.Sized.Ix

import Control.Monad.Fix
import Data.Set as Set
import Data.Map as Map
import Data.Maybe 


data ReadableAckBox a = ReadableAckBox (EXPR (Enabled a)) (REG ())

connectReadableAckBox
        :: forall a . (Rep a, Size (ADD (W a) X1), Show a)
        => String -> String -> STMT (ReadableAckBox a)
connectReadableAckBox inpName ackName = do
        i :: EXPR (Maybe a)   <- INPUT  (inStdLogicVector inpName)
        o :: REG ()           <- OUTPUT (outStdLogic ackName . isEnabled)
        return $ ReadableAckBox i o
                       
takeAckBox :: Rep a => ReadableAckBox a -> (EXPR a -> STMT ()) -> STMT ()
takeAckBox (ReadableAckBox iA oA) cont = do
        self <- LABEL
        do PAR [ OP1 (bitNot . isEnabled) iA :? GOTO self
               , OP1 (         isEnabled) iA :? do
                       PAR [ oA := OP0 (pureS ())
                           , cont (OP1 enabledVal iA)
                           ]
               ]

data WritableAckBox a = WritableAckBox (REG a) (EXPR Bool) 

connectWritableAckBox
        :: forall a . (Rep a, Size (ADD (W a) X1), Show a)
        => String -> String -> STMT (WritableAckBox a)
connectWritableAckBox outName ackName = do
        iB :: EXPR Bool <- INPUT  (inStdLogic ackName)
        oB :: REG a     <- OUTPUT (outStdLogicVector outName)
        return $ WritableAckBox oB iB

putAckBox :: Rep a => WritableAckBox a -> EXPR a -> STMT () -> STMT ()
putAckBox (WritableAckBox oB iB) val cont = do
        self <- LABEL 
        do PAR [ oB := val
               , OP1 (bitNot) iB :? GOTO self
               , iB              :? cont
               ]
               
prog1 :: STMT [LABEL]
prog1 = do
        rAckBox :: ReadableAckBox Int <- connectReadableAckBox "iA" "oA"
        wAckBox :: WritableAckBox Int <- connectWritableAckBox "oB" "iB"

--        iA :: EXPR (Maybe Int)   <- INPUT  (inStdLogicVector "iA")
--        oA :: REG ()             <- OUTPUT (outStdLogic "oA" . isEnabled)

--        iB :: EXPR Bool          <- INPUT  (inStdLogic "iB")
--        oB :: REG (Int)          <- OUTPUT (outStdLogicVector "oB")

        VAR v0                   <- REGISTER (0 :: Int)

        loop <- LABEL
        takeAckBox rAckBox (v0 :=)
        putAckBox wAckBox v0 (return ()) -- (GOTO loop)
        GOTO loop
{-
        bar <- LABEL 
        do PAR [ oB := v0
                , OP1 (bitNot) iB :? GOTO bar
--               , iB              :? GOTO foo
               ]
        GOTO foo
        
-}

{-

        oB := 9
        oA := OP0 (pureS ())
        v0 := OP0 (pureS Nothing)
        GOTO foo

        bar <- LABEL
        oA := OP0 (pureS ())
        GOTO bar
-}

        return [loop] -- bar]


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