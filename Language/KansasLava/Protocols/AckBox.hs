{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies,
  TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes,
  UndecidableInstances #-}


-- | This module implements an Ack protocol. In this producer/consumer model,
-- the producer drives the data input of the consumer, using an enable to
-- indicate that data is present. The producer will then keep the data value
-- steady until it receives an Ack from the consumer, at which point it's free
-- to drive the data input with a different value. This assumes that consumer
-- latches the input, as it may change.
module Language.KansasLava.Protocols.AckBox where

import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Types
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Types
import Language.KansasLava.Protocols.Patch
import Language.KansasLava.Utils

import Data.Maybe  as Maybe
-- import Language.KansasLava.Radix as Radix
--import Control.Concurrent
--import System.IO
--import Control.Monad
--import System.IO.Unsafe (unsafeInterleaveIO)
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
toAckBox :: (Rep a, Clock c, sig ~ CSeq c)
         =>  Patch [Maybe a]  			(sig (Enabled a))
	           ()				(sig Ack)

toAckBox = toAckBox' []

-- | An AckBox producer that will go through a series of wait states after each
-- time it drives the data output.
toAckBox' :: (Rep a, Clock c, sig ~ CSeq c)
             => [Int]		    -- ^ list wait states after every succesful post
             -> Patch [Maybe a] 		(sig (Enabled a))
		      ()			(sig Ack)

toAckBox' pauses ~(ys,ack) = ((),toSeq (fn ys (fromSeq ack) pauses))
        where
--           fn xs cs | trace (show ("fn",take  5 cs,take 5 cs)) False = undefined
	   -- send the value *before* checking the Ack

           fn xs ys' [] = fn xs ys' (repeat 0)
           fn (x:xs) ys' (0:ps) = x :
                case (x,ys') of
                 (_,Nothing:_)          -> error "toAckBox: bad protocol state (1)"
                 (Just _,Just (Ack True) :rs) -> fn xs rs ps          -- has been written
                 (Just _,Just (Ack False):rs) -> fn (x:xs) rs (0:ps)  -- not written yet
                 (Nothing,Just _:rs)    -> fn xs rs ps    	      -- nothing to write (choose to use pause, though)
                 (_,[])                 -> error "toAckBox: can't handle empty list of values to receive"
           fn (x:xs) rs (p:ps) = Nothing :
		case x of
				-- Allow extra Nothings to be consumed in the gaps
		   Nothing -> fn xs (Prelude.tail rs) (pred p:ps)
		   Just {} -> fn (x:xs) (Prelude.tail rs) (pred p:ps)
           fn [] ys' ps = fn (Prelude.repeat Nothing) ys' ps


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I'm sure this space-leaks.
fromAckBox :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
           => Patch (sig (Enabled a))		[Maybe a]
		    (sig Ack)			()
fromAckBox = fromAckBox' []

-- | An ackBox that goes through a series of intermediate states each time
-- consumes a value from the input stream and then issues an Ack.
fromAckBox' :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
           => [Int]
           -> Patch (sig (Enabled a))		[Maybe a]
		    (sig Ack)			()
fromAckBox' pauses ~(inp,_) = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp) pauses

	-- pretty simple API
	fn :: [Maybe (Enabled a)] -> [Int] -> [(Ack,Maybe a)]
	fn xs                 []     = fn xs (repeat 0)
        fn (Nothing:_)        _      = error "found an unknown value in AckBox input"
        fn (Just Nothing:xs)  ps     = (Ack False,Nothing) : fn xs ps
	fn (Just (Just v):xs) (0:ps) = (Ack True,Just v)   : fn xs ps
	fn (_:xs)             (p:ps) = (Ack False,Nothing) : fn xs (pred p:ps)
	fn []                 _      = error "fromAckBox: ack sequences should never end"

---------------------------------------------------------------------------
-- | 'enableToAckBox' turns an Enabled signal into a (1-sided) Patch.
enableToAckBox :: (Rep a, Clock c, sig ~ CSeq c)
	       => Patch (sig (Enabled a))    (sig (Enabled a))
		        ()  		     (sig Ack)
enableToAckBox ~(inp,ack) = ((),res)
	where
		res = register Nothing
		    $ cASE [ (isEnabled inp,inp)
			   , (fromAck ack, disabledS)
			   ] res

{-
beat :: (Clock c, sig ~ CSeq c) =>
	Patch ()		(sig (Enabled ()))
	      ()	()	(sig Ack)
beat ~(_,_) = ((),(),enabledS (pureS ()))
-}
---------------------------------------------------------------------------

{-
test1 :: [Maybe Int] -> [Maybe Int]
test1 xs = res
  where
	hs :: CSeq () (Enabled Int)
	hs = toAckBox xs ack

	(ack, hs') = shallowAckBoxBridge (lhs_rs,rhs_rs) (hs,ack')

        (lhs_r,rhs_r) = split (mkStdGen 0)

        lhs_rs = [ floor (c * 10) | c <- randoms lhs_r :: [Float] ]
        rhs_rs = [ floor (c * 10) | c <- randoms rhs_r :: [Float] ]

	(ack',res) = fromAckBox hs'
-}

-- | This introduces protocol-compliant delays (in the shallow embedding)
shallowAckBoxBridge :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c, Show a)
                       => ([Int],[Int])
                       -> Patch (sig (Enabled a))		(sig (Enabled a))
				(sig Ack)		 	(sig Ack)
shallowAckBoxBridge (lhsF,rhsF) = patch
  where
	patch = fromAckBox' lhsF `bus` toAckBox' rhsF

----------------------------------------------------------------------------------------------------
-- These are functions that are used to thread together Hand shaking and FIFO.

-- | This function takes a MVar, and gives back a Handshaken signal that represents
-- the continuous sequence of contents of the MVar.
{-
mVarToAckBox :: (Clock c, Rep a) => MVar a -> IO (CSeq c Ack -> (CSeq c (Enabled a)))
mVarToAckBox sfifo = do
        xs <- getFIFOContents sfifo
        return (toAckBox xs)
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
                $ (let (back,res) = fromAckBox $ sink back in res)
        return ()

-}



{-
-- interactMVar
interactMVar :: forall src sink
         . (Rep src, Rep sink)
        => (forall clk sig . (Clock clk, sig ~ CSeq clk) => (sig (Enabled src),sig Ack) -> (sig Ack,sig (Enabled sink)))
        -> MVar src
        -> MVar sink
        -> IO ()
interactMVar fn varA varB = do
        inp_fifo <- mVarToAckBox varA

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

liftAckBox :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c)
              => (forall c' . (Clock c') => CSeq c' a)
              -> sig Ack
              -> sig (Enabled a)
liftAckBox seq' (Seq s_ack d_ack) = enabledS res

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

