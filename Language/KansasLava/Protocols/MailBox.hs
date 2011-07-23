{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances #-}
module Language.KansasLava.Protocols.MailBox where

import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.HandShake

import Data.Maybe  as Maybe
-- import Language.KansasLava.Radix as Radix
import Control.Concurrent
import System.IO
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Prelude
import Prelude hiding (tail, lookup)

--import Debug.Trace

------------------------------------------------------------------------------------


{- The convention with MailBoxn signals is
  ...
 -> (lhs_inp, rhs_inp) 
 -> (lhs_out, rhs_out)

OR

 -> (lhs_inp, control_in, rhs_inp) 
 -> (lhs_out, control_out, rhs_out)

-}

------------------------------------------------------------------------------------
-- An Full is always in response to an incoming pFullet or message
newtype Full = Full { unFull :: Bool }
	deriving (Eq,Ord)
	
instance Show Full where
	show (Full True)  = "@"
	show (Full False) = "~"
	
	

instance Rep Full where
  data X Full = XFullRep { unXFullRep :: (X Bool) }
  type W Full = W Bool
  -- The template for using representations
  unX             = liftM Full   . unX  . unXFullRep
  optX            = XFullRep     . optX . liftM unFull 
  toRep           = toRep        . unXFullRep
  fromRep         = XFullRep     . fromRep
  repType Witness = repType (Witness :: Witness Bool)
  showRep         = showRepDefault

toFull :: (Signal sig) => sig Bool -> sig Full
toFull = coerce Full

fromFull :: (Signal sig) => sig Full -> sig Bool
fromFull = coerce unFull


-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toMailBox :: (Rep a, Clock c, sig ~ CSeq c)
             => [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Full				-- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
             -> sig (Enabled a)     
toMailBox = toMailBox' (repeat 0)

toMailBox' :: (Rep a, Clock c, sig ~ CSeq c)
             => [Int]		    -- ^ list wait states after every succesful post
             -> [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Full	    -- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
             -> sig (Enabled a)     

toMailBox' pauses ys full = toSeq (fn ys (fromSeq full) pauses)
        where
--           fn xs cs ps | trace (show ("fn",take 5 ps)) False = undefined
	   -- send the value *before* checking the Full
           fn xs fs ps = 
                case fs of
                 (Nothing:_)              -> error "toMailBox: bad protocol state (1)"
                 (Just (Full False) : fs') -> 
			case (xs,ps) of
			   (x:xs',0:ps') -> x : fn xs' fs' ps'     -- write it (it may be Nothing)
			   (_:_,p:ps')   -> Nothing : fn xs fs (pred p : ps')
			   (_,_)     	 -> Nothing : fn xs fs ps  -- nothing to write
                 (Just (Full True):rs)   -> Nothing : fn xs rs ps -- not ready yet
		 [] 			 -> error "toMailBox: Full seq should never end"


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I'm sure this space-leaks.
fromMailBox :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
               => sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Full, [Maybe a]) -- ^ Full flag sent back to FIFO and shallow list of values
fromMailBox = fromMailBox' (repeat 0)

fromMailBox' :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
               => [Int]			-- ^ list of wait counts before reading
               -> sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Full, [Maybe a]) -- ^ Full flag sent back to FIFO and shallow list of values
fromMailBox' ps inp = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp) ps

	-- pretty simple API
	fn :: [Maybe (Enabled a)] -> [Int] -> [(Full,Maybe a)]
        fn xs (0:ps') = (Full False,v) : rest
         where
	    (v,rest) = case xs of
			(Nothing:_)          -> error "found an unknown value in MailBox input"
        		(Just Nothing:xs')   -> (Nothing,fn xs' (0:ps'))	-- nothing read yet
			(Just v':xs')        -> (v',fn xs' ps')
			[]                   -> error "fromMailBox: Full sequences should never end"
        fn xs (p:ps') = (Full True,Nothing) : fn (Prelude.tail xs) (pred p:ps')
	fn _ []       = error "list of pauses should be infinite"
{-
test1 xs = xs'
    where
	e = toMailBox' (repeat 0) xs (full :: Seq Full)
 	(full,xs') = fromMailBox' [0..] e
-}

---------------------------------------------------------------------------

-- Connect a HandShake to an MailBox. The other way round requires a FIFO.
handShakeMailBox :: (Rep a, Clock c, sig ~ CSeq c)
	=> (sig (Enabled a),sig Full) 
        -> (sig Ack, sig (Enabled a))
handShakeMailBox (inp,full) = (toAck ack,out)
   where
	ack = isEnabled inp `and2` bitNot (fromFull full)
	out = packEnabled ack (enabledVal inp)

---------------------------------------------------------------------------


test2 :: [Maybe Int] -> [Maybe Int]
test2 xs = res
  where
	hs :: CSeq () (Enabled Int)
	hs = toMailBox xs full

	(full, hs') = shallowMailBoxBridge ([0..],[0..]) (hs,full')

	(full',res) = fromMailBox hs'

-- introduce protocol-compliant delays (in the shallow embedding)

shallowMailBoxBridge :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c, Show a) 
                       => ([Int],[Int])
                       -> (sig (Enabled a),sig Full) 
                       -> (sig Full, sig (Enabled a))
shallowMailBoxBridge (lhsF,rhsF) (inp,back) = (full,res)
   where
	(full,xs) = fromMailBox' lhsF inp
	res       = toMailBox' rhsF xs back

{-
        (lhs_r,rhs_r) = split stdGen

        lhs_rs = [ c < lhsF t | (c,t) <- zip (randoms lhs_r) [0..] ] 
        rhs_rs = [ c < rhsF t | (c,t) <- zip (randoms rhs_r) [0..] ] 

        fn :: [Bool] -> [Bool] -> [Maybe (Enabled a)] -> [Maybe Full] -> [a] -> [(Enabled a,Full)]
--        fn _ _ (x:_) _ store | trace (show ("fn",x,store)) False = undefined

        fn (True:lhss) rhss (Just (Just a):as) bs store = fn2 lhss rhss as bs (store ++ [a]) (Full True)
        fn (_:lhss)    rhss (Just _:as)        bs store = fn2 lhss rhss as bs (store)        (Full False)
        fn _ _ _ _ _ = error "failure in shallowMailBoxnBridge (fn)"

--        fn2 _ _ _ _ store bk | trace (show ("fn2",store,bk)) False = undefined
        fn2 lhss (True:rhss) as bs (s:ss) bk = (Just s,bk) :
                                                 case bs of
                                                   (Just (Full True) : bs')  -> fn lhss rhss as bs' ss
                                                   (Just (Full False) : bs') -> fn lhss rhss as bs' (s:ss)
                                                   _ -> error "failure in shallowMailBoxnBridge (fn2/case)"

        fn2 lhss (_:rhss)    as bs store bk  = (Nothing,bk)   : fn lhss rhss as (Prelude.tail bs) store
        fn2 _ _ _ _ _ _ = error "failure in shallowMailBoxnBridge (fn2)"
-}

----------------------------------------------------------------------------------------------------
-- These are functions that are used to thread together Hand shaking and FIFO.

-- | This function takes a MVar, and gives back a MailBox signal that represents
-- the continuous sequence of contents of the MVar.

mVarToMailBox :: (Clock c, Rep a) => MVar a -> IO (CSeq c Full -> (CSeq c (Enabled a)))
mVarToMailBox sfifo = do
        xs <- getFIFOContents sfifo
        return (toMailBox xs)
 where
        getFIFOContents :: MVar a -> IO [Maybe a]
        getFIFOContents var = unsafeInterleaveIO $ do
 	        x <- tryTakeMVar var
 	        xs <- getFIFOContents var
 	        return (x:xs)

mailBoxToMVar :: (Clock c, Rep a) => MVar a -> (CSeq c Full -> CSeq c (Enabled a)) -> IO ()
mailBoxToMVar sfifo sink = do
        sequence_
                $ map (putMVar sfifo)
                $ Maybe.catMaybes
                $ (let (back,res) = fromMailBox $ sink back in res)
        return ()

{-
-- interactMVar
interactMVar :: forall src sink
         . (Rep src, Rep sink)
        => (forall clk sig . (Clock clk, sig ~ CSeq clk) => (sig (Enabled src),sig Full) -> (sig Full,sig (Enabled sink)))
        -> MVar src
        -> MVar sink
        -> IO ()
interactMVar fn varA varB = do
        inp_fifo <- mVarToMailBox varA

        MailBoxToMVar varB $ \ rhs_back ->
                -- use fn at a specific (unit) clock
                let (lhs_back,rhs_out) = fn (lhs_inp,rhs_back :: CSeq () Full)
                    lhs_inp = inp_fifo lhs_back
                in
                    rhs_out

hInteract :: (forall clk sig . (Clock clk, sig ~ CSeq clk)
                => (sig (Enabled Word8),sig Full) -> (sig Full, sig (Enabled Word8))
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

liftMailBox :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c)
              => (forall c' . (Clock c') => CSeq c' a)
              -> sig Full
              -> sig (Enabled a)
liftMailBox seq' (Seq s_Full d_Full) = enabledS res

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

