{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies,
  TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes,
  UndecidableInstances #-}

-- | This module implements an Ready protocol. In this producer/consumer model,
-- the consumer issues a Ready signal to the producer, at which time the
-- producer can drive the data input to the consumer, signaling data valid with
-- an Enable. The producer will hold the consumer data input steady until it
-- receives another Ready.
module Language.KansasLava.Protocols.ReadyBox where

import Language.KansasLava.Rep
import Language.KansasLava.Seq
--import Language.KansasLava.Signal
--import Language.KansasLava.Stream (Stream(..))
--import qualified Language.KansasLava.Stream as Stream
import Language.KansasLava.Types
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Types
import Language.KansasLava.Protocols.Patch

import Data.Maybe  as Maybe
-- import Language.KansasLava.Radix as Radix
--import Control.Concurrent
--import System.IO
--import Control.Monad
--import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Prelude
import Prelude hiding (tail, lookup)

--import Debug.Trace

------------------------------------------------------------------------------------


{- The convention with ReadyBoxn signals is
  ...
 -> (lhs_inp, rhs_inp)
 -> (lhs_out, rhs_out)

OR

 -> (lhs_inp, control_in, rhs_inp)
 -> (lhs_out, control_out, rhs_out)

-}


-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toReadyBox :: (Rep a, Clock c, sig ~ CSeq c)
         =>  Patch [Maybe a]  			(sig (Enabled a))
	           ()				(sig Ready)
toReadyBox = toReadyBox' []

-- | A readybox that goes through a sequence of intermediate states after
-- issuing each enable, and before it looks for the next Ready.
toReadyBox' :: (Rep a, Clock c, sig ~ CSeq c)
             => [Int]		    -- ^ list wait states after every succesful post
             -> Patch [Maybe a]  			(sig (Enabled a))
		      ()				(sig Ready)
toReadyBox' pauses ~(ys,full) = ((),toSeq (fn ys (fromSeq full) pauses))
        where
--           fn xs cs ps | trace (show ("fn",take 5 ps)) False = undefined
	   -- send the value *before* checking the Ready
           fn xs fs ps =
                case fs of
                 (Nothing:_)              -> error "toReadyBox: bad protocol state (1)"
                 (Just (Ready True) : fs') ->
			case (xs,ps) of
			   (x:xs',0:ps')       -> x : fn xs' fs' ps'     -- write it (it may be Nothing)
			   (Nothing:xs',p:ps') -> Nothing : fn xs' fs' (pred p : ps')
			   (_:_,p:ps')         -> Nothing : fn xs fs' (pred p : ps')
			   (_:_,[])            -> fn xs fs (repeat 0)
			   (_,_)               -> Nothing : fn xs fs' ps  -- nothing to write
                 (Just (Ready False) : fs')    -> Nothing : fn xs fs' ps -- not ready yet
		 [] 			       -> error "toReadyBox: Ready seq should never end"


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I'm sure this space-leaks.
fromReadyBox :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
           => Patch (sig (Enabled a))		[Maybe a]
		    (sig Ready)			()
fromReadyBox = fromReadyBox' (repeat 0)

-- | Like fromReadyBox, but which goes through a series of intermediate states
-- after receiving an enable before issuing another Ready.
fromReadyBox' :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
           => [Int]
           -> Patch (sig (Enabled a))		[Maybe a]
		    (sig Ready)			()
fromReadyBox' ps ~(inp,_) = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp) ps

	-- pretty simple API
	fn :: [Maybe (Enabled a)] -> [Int] -> [(Ready,Maybe a)]
        fn xs (0:ps') = (Ready True,v) : rest
         where
	    (v,rest) = case xs of
			(Nothing:_)          -> error "found an unknown value in ReadyBox input"
        		(Just Nothing:xs')   -> (Nothing,fn xs' (0:ps'))	-- nothing read yet
			(Just v':xs')        -> (v',fn xs' ps')
			[]                   -> error "fromReadyBox: Ready sequences should never end"
        fn xs (p:ps') = (Ready False,Nothing) : fn (Prelude.tail xs) (pred p:ps')
	fn xs []      = fn xs (repeat 0)
{-
test1 xs = xs'
    where
	e = toReadyBox' (repeat 0) xs (full :: Seq Ready)
 	(full,xs') = fromReadyBox' [0..] e
-}

---------------------------------------------------------------------------


{-
test2 :: [Maybe Int] -> [Maybe Int]
test2 xs = res
  where
	hs :: CSeq () (Enabled Int)
	hs = toReadyBox xs full

	(full, hs') = shallowReadyBoxBridge ([0..],[0..]) (hs,full')

	(full',res) = fromReadyBox hs'
-}

-- | Introduces protocol-compliant delays (in the shallow embedding)
shallowReadyBoxBridge :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c, Show a)
                       => ([Int],[Int])
                       -> Patch (sig (Enabled a))		(sig (Enabled a))
				(sig Ready)		 	(sig Ready)
shallowReadyBoxBridge (lhsF,rhsF) = patch
  where
	patch = fromReadyBox' lhsF `bus` toReadyBox' rhsF


----------------------------------------------------------------------------------------------------
-- These are functions that are used to thread together Hand shaking and FIFO.

-- | This function takes a MVar, and gives back a ReadyBox signal that represents
-- the continuous sequence of contents of the MVar.
{-
mVarToReadyBox :: (Clock c, Rep a) => MVar a -> IO (CSeq c Ready -> (CSeq c (Enabled a)))
mVarToReadyBox sfifo = do
        xs <- getFIFOContents sfifo
        return (toReadyBox xs)
 where
        getFIFOContents :: MVar a -> IO [Maybe a]
        getFIFOContents var = unsafeInterleaveIO $ do
 	        x <- tryTakeMVar var
 	        xs <- getFIFOContents var
 	        return (x:xs)

mailBoxToMVar :: (Clock c, Rep a) => MVar a -> (CSeq c Ready -> CSeq c (Enabled a)) -> IO ()
mailBoxToMVar sfifo sink = do
        sequence_
                $ map (putMVar sfifo)
                $ Maybe.catMaybes
                $ (let (back,res) = fromReadyBox $ sink back in res)
        return ()
-}

{-
-- interactMVar
interactMVar :: forall src sink
         . (Rep src, Rep sink)
        => (forall clk sig . (Clock clk, sig ~ CSeq clk) => (sig (Enabled src),sig Ready) -> (sig Ready,sig (Enabled sink)))
        -> MVar src
        -> MVar sink
        -> IO ()
interactMVar fn varA varB = do
        inp_fifo <- mVarToReadyBox varA

        ReadyBoxToMVar varB $ \ rhs_back ->
                -- use fn at a specific (unit) clock
                let (lhs_back,rhs_out) = fn (lhs_inp,rhs_back :: CSeq () Ready)
                    lhs_inp = inp_fifo lhs_back
                in
                    rhs_out

hInteract :: (forall clk sig . (Clock clk, sig ~ CSeq clk)
                => (sig (Enabled Word8),sig Ready) -> (sig Full, sig (Enabled Word8))
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
                sequence_ $ map (putMVar inp_fifo_var) $ BS.unpFull bs

        -- send the out fifo to the out handle
        _ <- forkIO $ forever $ do
                x <- takeMVar out_fifo_var
                BS.hPutStr out $ BS.pFull [x]

        interactMVar fn inp_fifo_var out_fifo_var

-----------------------------------------------------------------------

liftReadyBox :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c)
              => (forall c' . (Clock c') => CSeq c' a)
              -> sig Full
              -> sig (Enabled a)
liftReadyBox seq' (Seq s_Full d_Full) = enabledS res

   where
        Seq s_seq d_seq = seq' :: CSeq () a     -- because of runST trick

        res = Seq (fn s_seq s_Full)
                  (D $ Port "o0" $ E $ Entity (Prim "retime")
                                        [("o0",bitTypeOf res)]
                                        [("i0",bitTypeOf res, unD d_seq)
                                        ,("pulse",B, unD d_Full)
                                        ]
                  )

        -- drop the head, when the Full comes back.
        fn (s `Cons` ss) Full = s `Cons` case Full of
                                 (XFullRep (XBool (WireVal True))  `Cons` Fulls) -> fn ss Fulls
                                 (XFullRep (XBool (WireVal False)) `Cons` Fulls) -> fn (s `Cons` ss) Fulls
                                 (XFullRep _               `Cons` _) -> Stream.repeat unknownX

-}

