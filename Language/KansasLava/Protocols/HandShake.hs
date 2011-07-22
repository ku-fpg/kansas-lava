{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances #-}
module Language.KansasLava.Protocols.HandShake where

import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled

import Data.Maybe  as Maybe
-- import Language.KansasLava.Radix as Radix
import Control.Concurrent
import System.IO
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Random

import qualified Prelude
import Prelude hiding (tail, lookup)


------------------------------------------------------------------------------------
-- An Ack is always in response to an incoming packet or message
newtype Ack = Ack { unAck :: Bool }
	deriving (Eq,Ord)
	
instance Show Ack where
	show (Ack True)  = "@"
	show (Ack False) = "~"
	
	

instance Rep Ack where
  data X Ack = XAckRep { unXAckRep :: (X Bool) }
  type W Ack = W Bool
  -- The template for using representations
  unX             = liftM Ack   . unX  . unXAckRep
  optX            = XAckRep     . optX . liftM unAck 
  toRep           = toRep       . unXAckRep
  fromRep         = XAckRep     . fromRep
  repType Witness = repType (Witness :: Witness Bool)
  showRep         = showRepDefault

toAck :: (Signal sig) => sig Bool -> sig Ack
toAck = coerce Ack

fromAck :: (Signal sig) => sig Ack -> sig Bool
fromAck = coerce unAck


-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toHandShake :: (Rep a, Clock c, sig ~ CSeq c)
             => [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Ack				-- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
             -> sig (Enabled a)     

toHandShake ys ack = toSeq (fn ys (fromSeq ack))
        where
--           fn xs cs | trace (show ("fn",take  5 cs,take 5 cs)) False = undefined
	   -- send the value *before* checking the Ack
           fn (x:xs) ys' = x :
                case (x,ys') of
                 (_,Nothing:_)          -> error "toHandShake: bad protocol state (1)"
                 (Just _,Just (Ack True) :rs) -> fn xs rs      -- has been written
                 (Just _,Just (Ack False):rs) -> fn (x:xs) rs   -- not written yet
                 (Nothing,Just _:rs)    -> fn xs rs     	-- nothing to write
                 (_,[])                 -> error "toHandShake: can't handle empty list of values to receive"
           fn [] ys' = fn (Prelude.repeat Nothing) ys'


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I'm sure this space-leaks.
fromHandShake :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
               => sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Ack, [Maybe a]) -- ^ ack flag sent back to FIFO and shallow list of values
fromHandShake inp = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp)

	-- pretty simple API
	fn :: [Maybe (Enabled a)] -> [(Ack,Maybe a)]
        fn (Nothing:_)         = error "found an unknown value in HandShake input"
        fn (Just Nothing:xs)   = (Ack False,Nothing) : fn xs
	fn ((Just v):xs)       = (Ack True,v)        : fn xs
	fn []                  = error "fromHandShake: ack sequences should never end"


---------------------------------------------------------------------------

{-
test1 :: [Maybe Int] -> [Maybe Int]
test1 xs = res
  where
	hs :: CSeq () (Enabled Int)
	hs = toHandShake xs ack

	(hs', ack) = shallowHandShakeBridge (mkStdGen 1000) (const 1.5,const 1.5) (hs,ack')

	(ack',res) = fromHandShake hs'
-- -}

-- introduce protocol-compliant delays (in the shallow embedding)

shallowHandShakeBridge :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c, Show a) 
                       => StdGen
                       -> (Integer -> Float,Integer -> Float)
                       -> (sig (Enabled a),sig Ack) 
                       -> (sig (Enabled a),sig Ack)
shallowHandShakeBridge stdGen (lhsF,rhsF) (inp,back)
        = unpack (toSeq $ fn lhs_rs rhs_rs (fromSeq inp) (fromSeq back) [])
   where
        (lhs_r,rhs_r) = split stdGen

        lhs_rs = [ c < lhsF t | (c,t) <- zip (randoms lhs_r) [0..] ] 
        rhs_rs = [ c < rhsF t | (c,t) <- zip (randoms rhs_r) [0..] ] 

        fn :: [Bool] -> [Bool] -> [Maybe (Enabled a)] -> [Maybe Ack] -> [a] -> [(Enabled a,Ack)]
--        fn _ _ (x:_) _ store | trace (show ("fn",x,store)) False = undefined
        fn (True:lhss) rhss (Just (Just a):as) bs store = fn2 lhss rhss as bs (store ++ [a]) (Ack True)
        fn (_:lhss)    rhss (Just _:as)        bs store = fn2 lhss rhss as bs (store)        (Ack False)
        fn _ _ _ _ _ = error "failure in shallowHandShakenBridge (fn)"

--        fn2 _ _ _ _ store bk | trace (show ("fn2",store,bk)) False = undefined
        fn2 lhss (True:rhss) as bs (s:ss) bk = (Just s,bk) :
                                                 case bs of
                                                   (Just (Ack True) : bs')  -> fn lhss rhss as bs' ss
                                                   (Just (Ack False) : bs') -> fn lhss rhss as bs' (s:ss)
                                                   _ -> error "failure in shallowHandShakenBridge (fn2/case)"

        fn2 lhss (_:rhss)    as bs store bk  = (Nothing,bk)   : fn lhss rhss as (Prelude.tail bs) store
        fn2 _ _ _ _ _ _ = error "failure in shallowHandShakenBridge (fn2)"


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

