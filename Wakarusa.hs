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


               
prog1 :: STMT [LABEL]
prog1 = do
        rAckBox :: ReadableAckBox Int <- connectReadableAckBox "iA" "oA"
        wAckBox :: WritableAckBox Int <- connectWritableAckBox "oB" "iB"

        VAR v0  :: VAR Int           <- REGISTER $ Nothing

        loop <- LABEL
        takeAckBox rAckBox (v0 :=)
        putAckBox wAckBox v0 (GOTO loop)

        return [loop] 


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