{-# LANGUAGE TypeFamilies #-}
import Language.KansasLava as KL
import Data.Sized.Matrix
import Data.Sized.Signed
import Data.Sized.Unsigned

-- Example for Andrew, of FIFOs in use, in a shallow setting

main = do
        c <- reifyFabric fabric_example
        print c
        c <- reifyFabric fabric_example2
        print c
--        print $ circuitSignature c
--        c <- reifyFabric (example1 `driving` example2)
        print c

        c <- reifyFabric negate_example2
        print c
        c <- reifyFabric negate_example
        print c

negate_example :: Fabric ()
negate_example = do
                i0 <- inStdLogicVector "i0"
                let o0 = liftS1 negate (coerce i0 :: Seq (Signed X4))
                outStdLogicVector "o0" (coerce o0)

negate_example2 :: Fabric ()
negate_example2 = do
                i0 <- inStdLogic "i0"
                let o0 = liftS1 bitNot i0
                outStdLogic "o0" o0

fabric_example2 :: Fabric ()
fabric_example2 = do
        i0 <- inStdLogic "ins0"
        i0 <- inStdLogic "ins2"
        outStdLogic "sum" i0
        outStdLogic "prod" (low :: Seq Bool)


fabric_example :: Fabric ()
fabric_example = do
        i0 <- inStdLogic "ins0"
        i1 <- inStdLogic "ins1"
        let (c,s) = halfAdder i0 i1
        outStdLogic "carry" (delay c)
        outStdLogic "sum" s

example1 :: Fabric ()
example1 = do
        i0 <- inStdLogic "ins0"
        i1 <- inStdLogic "ins2"
        outStdLogic "carry" (i0 `and2` i1)
        outStdLogic "sum" (i0 `and2` i1)
        outStdLogic "extra_out" (i0 `and2` i1)
        outStdLogicVector "X" (0 :: Seq (Unsigned X4))

example2 :: Fabric ()
example2 = do
        i0 <- inStdLogic "carry"
        i1 <- inStdLogic "sum"
        i2 <- inStdLogic "extra_in"
        outStdLogic "x" (i0 `and2` i1)
        outStdLogic "s" (i0 `and2` i2)

halfAdder :: Seq Bool -> Seq Bool -> (Seq Bool,Seq Bool)
halfAdder a b = (carry,sum_)
        where carry = and2 a b
              sum_  = xor2 a b

--        print (example (fifo (Witness :: Witness X1) low) [Just x | x <- [1..] :: [U8]])

{-
-- An example wrapper, with a HO argument
example :: (Rep a, Rep b, Clock c, sig ~ CSeq c, c ~ ())
        => ((sig (Enabled a), sig Bool) -> (sig Bool, sig (Enabled b)))
         -> [Maybe a] -> [Maybe b]
example f xs = cs
   where
        res = toHandShaken xs ready'
        (ready',res') = f (res,ready)
        (ready,cs) = fromHandShaken res'



-}
