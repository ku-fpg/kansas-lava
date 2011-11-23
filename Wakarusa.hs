{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, DoRec, TypeFamilies #-}
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
--        oA :: REG Int            <- OUTPUT (outStdLogicVector "o0")
--        iA :: EXPR (Maybe Int)   <- INPUT (inStdLogicVector "i0")

        iA :: EXPR (Maybe Int)   <- INPUT  (inStdLogicVector "iA")
        oA :: REG ()             <- OUTPUT (outStdLogic "oA" . isEnabled)

        iB :: EXPR Bool          <- INPUT  (inStdLogic "iB")
        oB :: REG (Int)          <- OUTPUT (outStdLogicVector "oB")

        VAR v0                   <- REGISTER (Nothing :: Maybe Int)

        -- 

        foo <- LABEL
        do PAR [ OP1 (bitNot . isEnabled) iA :? GOTO foo
               , OP1 (         isEnabled) iA :? do
                       PAR [ oA := OP0 (pureS ())
                           , v0 := iA
                           ]
               ]
        bar <- LABEL 
        do PAR [ oB := OP1 enabledVal v0
                , OP1 (bitNot) iB :? GOTO bar
               , iB              :? GOTO foo
               ]
--        GOTO foo
--        oB := 9            


        return [foo]


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