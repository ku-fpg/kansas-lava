{-# LANGUAGE TypeFamilies #-}
import Language.KansasLava
import Data.Sized.Unsigned
import Data.Sized.Matrix

-- Example for Andrew, of FIFOs in use, in a shallow setting

main = print (example (fifo (Witness :: Witness X1) low) [Just x | x <- [1..] :: [U8]]) 

-- An example wrapper, with a HO argument
example :: (Rep a, Rep b, Clock c, sig ~ CSeq c, c ~ ())
        => ((sig (Enabled a), sig Bool) -> (sig Bool, sig (Enabled b)))
         -> [Maybe a] -> [Maybe b]
example f xs = cs
   where
        res = toHandShaken xs ready'
        (ready',res') = f (res,ready)
        (ready,cs) = fromHandShaken res'
        
        
        
