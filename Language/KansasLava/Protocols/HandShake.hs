{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances #-}
module Language.KansasLava.Protocols.HandShake where

import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Types
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Types
import Language.KansasLava.Utils

import Data.Maybe  as Maybe
-- import Language.KansasLava.Radix as Radix
import Control.Concurrent
import System.IO
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)
--import System.Random

import qualified Prelude
import Prelude hiding (tail, lookup)


------------------------------------------------------------------------------------


{- The convention with handshaken signals is
  ...
 -> (lhs_inp, rhs_inp) 
 -> (lhs_out, rhs_out)

OR

 -> (lhs_inp, control_in, rhs_inp) 
 -> (lhs_out, control_out, rhs_out)

-}


-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toHandShake :: (Rep a, Clock c, sig ~ CSeq c)
	     => [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Ack				-- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
             -> sig (Enabled a)     
toHandShake = toHandShake' []

toHandShake' :: (Rep a, Clock c, sig ~ CSeq c)
             => [Int]		    -- ^ pauses between offering values
             -> [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Ack				-- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
             -> sig (Enabled a)     
toHandShake' pauses ys ack = toSeq (fn ys (fromSeq ack) pauses)
        where
--           fn xs cs | trace (show ("fn",take  5 cs,take 5 cs)) False = undefined
	   -- send the value *before* checking the Ack

           fn xs ys' [] = fn xs ys' (repeat 0)
           fn (x:xs) ys' (0:ps) = x :
                case (x,ys') of
                 (_,Nothing:_)          -> error "toHandShake: bad protocol state (1)"
                 (Just _,Just (Ack True) :rs) -> fn xs rs ps          -- has been written
                 (Just _,Just (Ack False):rs) -> fn (x:xs) rs (0:ps)  -- not written yet
                 (Nothing,Just _:rs)    -> fn xs rs ps    	      -- nothing to write (choose to use pause, though)
                 (_,[])                 -> error "toHandShake: can't handle empty list of values to receive"
           fn (x:xs) rs (p:ps) = Nothing : 
		case x of
				-- Allow extra Nothings to be consumed in the gaps
		   Nothing -> fn xs (Prelude.tail rs) (pred p:ps)
		   Just {} -> fn (x:xs) (Prelude.tail rs) (pred p:ps)
           fn [] ys' ps = fn (Prelude.repeat Nothing) ys' ps


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I'm sure this space-leaks.
fromHandShake :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
               => sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Ack, [Maybe a]) -- ^ ack flag sent back to FIFO and shallow list of values
fromHandShake = fromHandShake' []

fromHandShake' :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
               => [Int]
	       -> sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Ack, [Maybe a]) -- ^ ack flag sent back to FIFO and shallow list of values
fromHandShake' pauses inp = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp) pauses

	-- pretty simple API
	fn :: [Maybe (Enabled a)] -> [Int] -> [(Ack,Maybe a)]
	fn xs                 []     = fn xs (repeat 0)
        fn (Nothing:_)        _      = error "found an unknown value in HandShake input"
        fn (Just Nothing:xs)  ps     = (Ack False,Nothing) : fn xs ps
	fn (Just (Just v):xs) (0:ps) = (Ack True,Just v)   : fn xs ps
	fn (_:xs)             (p:ps) = (Ack False,Nothing) : fn xs (pred p:ps)
	fn []                 _      = error "fromHandShake: ack sequences should never end"

---------------------------------------------------------------------------
-- 'enableToHandShake' turns an Enabled signal into a (1-sided) Patch.

enableToHandShake :: (Rep a, Clock c, sig ~ CSeq c)
		  => sig (Enabled a)
		  -> Patch ()    (sig (Enabled a))
		           () () (sig Ack)

enableToHandShake inp ~(_,ack) = ((),(),res)
	where
		res = register Nothing 
		    $ cASE [ (isEnabled inp,inp)
			   , (fromAck ack, disabledS)
			   ] res

---------------------------------------------------------------------------

{-
test1 :: [Maybe Int] -> [Maybe Int]
test1 xs = res
  where
	hs :: CSeq () (Enabled Int)
	hs = toHandShake xs ack

	(ack, hs') = shallowHandShakeBridge (lhs_rs,rhs_rs) (hs,ack')

        (lhs_r,rhs_r) = split (mkStdGen 0)

        lhs_rs = [ floor (c * 10) | c <- randoms lhs_r :: [Float] ]
        rhs_rs = [ floor (c * 10) | c <- randoms rhs_r :: [Float] ]

	(ack',res) = fromHandShake hs'
-}

-- introduce protocol-compliant delays (in the shallow embedding)

shallowHandShakeBridge :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c, Show a) 
                       => ([Int],[Int])
                       -> (sig (Enabled a),sig Ack) 
                       -> (sig Ack, sig (Enabled a))
shallowHandShakeBridge (lhsF,rhsF) (inp,back) = (ack,res)
   where
	(ack,xs)  = fromHandShake' lhsF inp
	res       = toHandShake' rhsF xs back

----------------------------------------------------------------------------------------------------
-- These are functions that are used to thread together Hand shaking and FIFO.

-- | This function takes a MVar, and gives back a Handshaken signal that represents
-- the continuous sequence of contents of the MVar.

mVarToHandShake :: (Clock c, Rep a) => MVar a -> IO (CSeq c Ack -> (CSeq c (Enabled a)))
mVarToHandShake sfifo = do
        xs <- getFIFOContents sfifo
        return (toHandShake xs)
 where
        getFIFOContents :: MVar a -> IO [Maybe a]
        getFIFOContents var = unsafeInterleaveIO $ do
 	        x <- tryTakeMVar var
 	        xs <- getFIFOContents var
 	        return (x:xs)

handShakeToMVar :: (Clock c, Rep a) => MVar a -> (CSeq c Ack -> CSeq c (Enabled a)) -> IO ()
handShakeToMVar sfifo sink = do
        sequence_
                $ map (putMVar sfifo)
                $ Maybe.catMaybes
                $ (let (back,res) = fromHandShake $ sink back in res)
        return ()

{-
-- interactMVar
interactMVar :: forall src sink
         . (Rep src, Rep sink)
        => (forall clk sig . (Clock clk, sig ~ CSeq clk) => (sig (Enabled src),sig Ack) -> (sig Ack,sig (Enabled sink)))
        -> MVar src
        -> MVar sink
        -> IO ()
interactMVar fn varA varB = do
        inp_fifo <- mVarToHandShake varA

        handShakeToMVar varB $ \ rhs_back ->
                -- use fn at a specific (unit) clock
                let (lhs_back,rhs_out) = fn (lhs_inp,rhs_back :: CSeq () Ack)
                    lhs_inp = inp_fifo lhs_back
                in
                    rhs_out

hInteract :: (forall clk sig . (Clock clk, sig ~ CSeq clk)
                => (sig (Enabled Word8),sig Ack) -> (sig Ack, sig (Enabled Word8))
             )
          -> Handle
          -> Handle
          -> IO ()
hInteract fn inp out = do
        inp_fifo_var <- newEmptyMVar
        out_fifo_var <- newEmptyMVar

        -- send the inp handle to the inp fifo
        _ <- forkIO $ forever $ do
                bs <- BS.hGetContents inp
                sequence_ $ map (putMVar inp_fifo_var) $ BS.unpack bs

        -- send the out fifo to the out handle
        _ <- forkIO $ forever $ do
                x <- takeMVar out_fifo_var
                BS.hPutStr out $ BS.pack [x]

        interactMVar fn inp_fifo_var out_fifo_var

-----------------------------------------------------------------------

liftHandShake :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c)
              => (forall c' . (Clock c') => CSeq c' a)
              -> sig Ack
              -> sig (Enabled a)
liftHandShake seq' (Seq s_ack d_ack) = enabledS res

   where
        Seq s_seq d_seq = seq' :: CSeq () a     -- because of runST trick

        res = Seq (fn s_seq s_ack) 
                  (D $ Port "o0" $ E $ Entity (Prim "retime")
                                        [("o0",bitTypeOf res)]
                                        [("i0",bitTypeOf res, unD d_seq)
                                        ,("pulse",B, unD d_ack)
                                        ]
                  )                

        -- drop the head, when the ack comes back.
        fn (s `Cons` ss) ack = s `Cons` case ack of
                                 (XAckRep (XBool (WireVal True))  `Cons` acks) -> fn ss acks 
                                 (XAckRep (XBool (WireVal False)) `Cons` acks) -> fn (s `Cons` ss) acks 
                                 (XAckRep _               `Cons` _) -> Stream.repeat unknownX 

-}

