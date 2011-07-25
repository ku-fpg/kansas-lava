{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances #-}
module Language.KansasLava.Protocols.MailBox where

import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Stream (Stream(..))
import qualified Language.KansasLava.Stream as Stream
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Protocols.Enabled

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
-- An Ready is always in response to an incoming packet or message
newtype Ready = Ready { unReady :: Bool }
	deriving (Eq,Ord)
	
instance Show Ready where
	show (Ready True)  = "R"
	show (Ready False) = "~"
	

instance Rep Ready where
  data X Ready = XReadyRep { unXReadyRep :: (X Bool) }
  type W Ready = W Bool
  -- The template for using representations
  unX             = liftM Ready   . unX  . unXReadyRep
  optX            = XReadyRep     . optX . liftM unReady 
  toRep           = toRep        . unXReadyRep
  fromRep         = XReadyRep     . fromRep
  repType Witness = repType (Witness :: Witness Bool)
  showRep         = showRepDefault

toReady :: (Signal sig) => sig Bool -> sig Ready
toReady = coerce Ready

fromReady :: (Signal sig) => sig Ready -> sig Bool
fromReady = coerce unReady


-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toMailBox :: (Rep a, Clock c, sig ~ CSeq c)
             => [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Ready				-- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
             -> sig (Enabled a)     
toMailBox = toMailBox' (repeat 0)

toMailBox' :: (Rep a, Clock c, sig ~ CSeq c)
             => [Int]		    -- ^ list wait states after every succesful post
             -> [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Ready	    -- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
             -> sig (Enabled a)     

toMailBox' pauses ys full = toSeq (fn ys (fromSeq full) pauses)
        where
--           fn xs cs ps | trace (show ("fn",take 5 ps)) False = undefined
	   -- send the value *before* checking the Ready
           fn xs fs ps = 
                case fs of
                 (Nothing:_)              -> error "toMailBox: bad protocol state (1)"
                 (Just (Ready True) : fs') -> 
			case (xs,ps) of
			   (x:xs',0:ps')       -> x : fn xs' fs' ps'     -- write it (it may be Nothing)
			   (Nothing:xs',p:ps') -> Nothing : fn xs' fs (pred p : ps')
			   (_:_,p:ps')         -> Nothing : fn xs fs (pred p : ps')
			   (_:_,[])            -> fn xs fs (repeat 0)
			   (_,_)               -> Nothing : fn xs fs ps  -- nothing to write
                 (Just (Ready False):rs)         -> Nothing : fn xs rs ps -- not ready yet
		 [] 			       -> error "toMailBox: Ready seq should never end"


-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I'm sure this space-leaks.
fromMailBox :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
               => sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Ready, [Maybe a]) -- ^ Ready flag sent back to FIFO and shallow list of values
fromMailBox = fromMailBox' (repeat 0)

fromMailBox' :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
               => [Int]			-- ^ list of wait counts before reading
               -> sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Ready, [Maybe a]) -- ^ Ready flag sent back to FIFO and shallow list of values
fromMailBox' ps inp = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp) ps

	-- pretty simple API
	fn :: [Maybe (Enabled a)] -> [Int] -> [(Ready,Maybe a)]
        fn xs (0:ps') = (Ready True,v) : rest
         where
	    (v,rest) = case xs of
			(Nothing:_)          -> error "found an unknown value in MailBox input"
        		(Just Nothing:xs')   -> (Nothing,fn xs' (0:ps'))	-- nothing read yet
			(Just v':xs')        -> (v',fn xs' ps')
			[]                   -> error "fromMailBox: Ready sequences should never end"
        fn xs (p:ps') = (Ready False,Nothing) : fn (Prelude.tail xs) (pred p:ps')
	fn xs []      = fn xs (repeat 0)
{-
test1 xs = xs'
    where
	e = toMailBox' (repeat 0) xs (full :: Seq Ready)
 	(full,xs') = fromMailBox' [0..] e
-}

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
                       -> (sig (Enabled a),sig Ready) 
                       -> (sig Ready, sig (Enabled a))
shallowMailBoxBridge (lhsF,rhsF) (inp,back) = (full,res)
   where
	(full,xs) = fromMailBox' lhsF inp
	res       = toMailBox' rhsF xs back

----------------------------------------------------------------------------------------------------
-- These are functions that are used to thread together Hand shaking and FIFO.

-- | This function takes a MVar, and gives back a MailBox signal that represents
-- the continuous sequence of contents of the MVar.

mVarToMailBox :: (Clock c, Rep a) => MVar a -> IO (CSeq c Ready -> (CSeq c (Enabled a)))
mVarToMailBox sfifo = do
        xs <- getFIFOContents sfifo
        return (toMailBox xs)
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
                $ (let (back,res) = fromMailBox $ sink back in res)
        return ()

{-
-- interactMVar
interactMVar :: forall src sink
         . (Rep src, Rep sink)
        => (forall clk sig . (Clock clk, sig ~ CSeq clk) => (sig (Enabled src),sig Ready) -> (sig Ready,sig (Enabled sink)))
        -> MVar src
        -> MVar sink
        -> IO ()
interactMVar fn varA varB = do
        inp_fifo <- mVarToMailBox varA

        MailBoxToMVar varB $ \ rhs_back ->
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

liftMailBox0 :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c)
              => (forall c' . (Clock c') => CSeq c' a)
              -> sig Ready
              -> sig (Enabled a)
liftMailBox0 seq' (Seq s_Ready d_Ready) = res
   where
        Seq s_seq d_seq = seq' :: CSeq () a     -- because of runST trick, we can use *any* clock

	ty = bitTypeOf (undefined :: Seq a)

	e = Entity (External "flux")
                   [("o_en",B)
                   ,("o_val",ty)
		   ,("o_clk_en",B)
		   ]
                   [("i0",ty, unD d_seq)
                   ,("ready",B, unD d_Ready)
                   ]

	res :: sig (Enabled a)
        res = Seq (fn0 s_seq s_Ready) 
                  (D $ Port "o0" $ E $
			Entity (Prim "pair") 
				[("o0",bitTypeOf res)]
				[("i0",B,Port "o_en" $ E $ e)
				,("i1",ty,Port "o_val" $ E $ e)
				]
                  )                

	-- ignore the first ready.
        fn0 ss (XReadyRep _ `Cons` readys) = 
		XMaybe (pureX False, unknownX) `Cons` fn ss readys

        fn ss (XReadyRep (XBool (WireVal True)) `Cons` readys) 
		= case ss of
		   (s `Cons` ss') -> XMaybe (pureX True, s) `Cons` fn ss' readys
        fn ss (XReadyRep (XBool (WireVal False)) `Cons` readys) 
		= XMaybe (pureX False, unknownX) `Cons` fn ss readys
        fn _ (XReadyRep _ `Cons` _) = Stream.repeat unknownX



