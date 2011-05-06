{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp,
    RankNTypes  #-}

module Language.KansasLava.HandShake where

import Control.Concurrent
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe as Maybe
import Data.Sized.Arith as Arith
import Data.Sized.Ix as X
import Data.Word

import Language.KansasLava.Comb
import Language.KansasLava.Protocols
import Language.KansasLava.Rep
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Types
import Language.KansasLava.Utils

import System.IO

----------------------------------------------------------------------------------------------------

-- | Take a list of shallow values and create a stream which can be sent into
--   a FIFO, respecting the write-ready flag that comes out of the FIFO.
toHandShaken :: (Rep a, Clock c, sig ~ CSeq c)
             => [Maybe a]           -- ^ shallow values we want to send into the FIFO
             -> sig Bool
             -> sig (Enabled a)     -- ^ takes a flag back from FIFO that indicates successful write
                                    --   to a stream of values sent to FIFO
toHandShaken ys ready = toSeq (fn ys (fromSeq ready))
        where
--           fn xs cs | trace (show ("fn",take  5 cs,take 5 cs)) False = undefined
           fn (x:xs) ys' = x :
                case (x,ys') of
                 (_,Nothing:_)          -> error "toHandShaken: bad protocol state (1)"
                 (Just _,Just True:rs)  -> fn xs rs     -- has been written
                 (Just _,Just False:rs) -> fn (x:xs) rs -- not written yet
                 (Nothing,Just _:rs)    -> fn xs rs     -- nothing to write
                 (_,[])                 -> error "toHandShaken: can't handle empty list of values to receive"
           fn [] _ = error "toHandShaken: can't handle empty list of values to issue"



-- | Take stream from a FIFO and return an asynchronous read-ready flag, which
--   is given back to the FIFO, and a shallow list of values.
-- I suspect this space-leaks.
fromHandShaken :: (Rep a, Clock c, sig ~ CSeq c)
               => sig (Enabled a)       -- ^ fifo output sequence
               -> (sig Bool, [Maybe a]) -- ^ read-ready flag sent back to FIFO and shallow list of values
fromHandShaken inp = (toSeq (map fst internal), map snd internal)
   where
        internal = fn (fromSeq inp)

        fn (x:xs) = (True,rep) : rest
           where
                (rep,rest) = case x of
                               Nothing       -> error "fromVariableHandshake: bad reply to ready status"
                               Just Nothing  -> (Nothing,fn xs)
                               Just (Just v) -> (Just v,fn xs)
        fn [] = (False,Nothing) : fn []


-- introduce protocol-compliant delays.

shallowHandShakenBridge :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c, Show a) => (sig Bool,sig Bool) -> (sig (Enabled a),sig Bool) -> (sig (Enabled a),sig Bool)
shallowHandShakenBridge (lhsS,rhsS) (inp,back)
        = unpack (toSeq $ fn (fromSeq lhsS) (fromSeq rhsS) (fromSeq inp) (fromSeq back) [])
   where
        fn :: [Maybe Bool] -> [Maybe Bool] -> [Maybe (Enabled a)] -> [Maybe Bool] -> [a] -> [(Enabled a,Bool)]
--        fn _ _ (x:_) _ store | trace (show ("fn",x,store)) False = undefined
        fn (Just True:lhss) rhss (Just (Just a):as) bs store = fn2 lhss rhss as bs (store ++ [a]) True
        fn (_:lhss)      rhss (Just _:as)        bs store = fn2 lhss rhss as bs (store)        False
        fn _ _ _ _ _ = error "failure in shallowHandShakenBridge (fn)"

--        fn2 _ _ _ _ store bk | trace (show ("fn2",store,bk)) False = undefined
        fn2 lhss (Just True:rhss) as bs (s:ss) bk = (Just s,bk) :
                                                 case bs of
                                                   (Just True : bs')  -> fn lhss rhss as bs' ss
                                                   (Just False : bs') -> fn lhss rhss as bs' (s:ss)
                                                   _ -> error "failure in shallowHandShakenBridge (fn2/case)"

        fn2 lhss (Just _:rhss)    as bs store bk  = (Nothing,bk)   : fn lhss rhss as (tail bs) store
        fn2 _ _ _ _ _ _ = error "failure in shallowHandShakenBridge (fn2)"

{-
test = take 100 res
  where
        input :: [Int]
        input = [1..]

        inp :: Seq (Enabled Int)
        inp = toHandShaken (concat [[Just x, Nothing,Nothing]| x <- input ]) inp_back

--        bridge = id
        bridge = shallowHandShakenBridge (randomBools (mkStdGen 0)  (const 0.9),randomBools (mkStdGen 1) (const 0.1))

        (outp,inp_back) = bridge (inp,out_back)

        inp_back :: Seq Bool
        res :: [Maybe Int]
        (out_back,res) = fromHandShaken outp
-}

----------------------------------------------------------------------------------------------------
-- These are functions that are used to thread together Hand shaking and FIFO.

-- | This function takes a MVar, and gives back a Handshaken signal that represents
-- the continuous sequence of contents of the MVar.

mVarToHandShaken :: (Clock c, Rep a) => MVar a -> IO (CSeq c Bool -> (CSeq c (Enabled a)))
mVarToHandShaken sfifo = do
        xs <- getFIFOContents sfifo
        return (toHandShaken xs)
 where
        getFIFOContents :: MVar a -> IO [Maybe a]
        getFIFOContents var = unsafeInterleaveIO $ do
 	        x <- tryTakeMVar var
 	        xs <- getFIFOContents var
 	        return (x:xs)

handShakeToMVar :: (Clock c, Rep a) => MVar a -> (CSeq c Bool -> CSeq c (Enabled a)) -> IO ()
handShakeToMVar sfifo sink = do
        sequence_
                $ map (putMVar sfifo)
                $ Maybe.catMaybes
                $ (let (back,res) = fromHandShaken $ sink back in res)
        return ()


-- interactMVar
interactMVar :: forall src sink
         . (Rep src, Rep sink)
        => (forall clk sig . (Clock clk, sig ~ CSeq clk) => I (sig (Enabled src)) (sig Bool) -> O (sig Bool) (sig (Enabled sink)))
        -> MVar src
        -> MVar sink
        -> IO ()
interactMVar fn varA varB = do
        inp_fifo <- mVarToHandShaken varA

        handShakeToMVar varB $ \ rhs_back ->
                -- use fn at a specific (unit) clock
                let (lhs_back,rhs_out) = fn (lhs_inp,rhs_back :: CSeq () Bool)
                    lhs_inp = inp_fifo lhs_back
                in
                    rhs_out

hInteract :: (forall clk sig . (Clock clk, sig ~ CSeq clk)
                => I (sig (Enabled Word8)) (sig Bool) -> O (sig Bool) (sig (Enabled Word8))
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

interact :: (forall clk sig . (Clock clk, sig ~ CSeq clk) => I (sig (Enabled Word8)) (sig Bool) -> O (sig Bool) (sig (Enabled Word8))) -> IO ()
interact fn = do
        hSetBinaryMode stdin True
        hSetBuffering stdin NoBuffering
        hSetBinaryMode stdout True
        hSetBuffering stdout NoBuffering
        hInteract fn stdin stdout

----------------------------------------------------------------------------------------------------
-- ********* FIFO stuff below this ********************

incGroup :: (Rep x, Num x, Bounded x) => Comb x -> Comb x
incGroup x = mux2 (x .==. maxBound) (0,x + 1)

-- | Make a sequence obey the given reset signal, returning given value on a reset.
resetable :: forall a c. (Clock c, Rep a) => CSeq c Bool -> Comb a -> CSeq c a -> CSeq c a
resetable rst val x = mux2 rst (liftS0 val,x)

fifoFE :: forall c a counter ix .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c
        )
      => Witness ix
         -- ^ depth of FIFO
      -> CSeq c Bool
         -- ^ hard reset option
      -> (CSeq c (Enabled a), CSeq c counter)
         -- ^ input, and Seq trigger of how much to decrement the counter
      -> (CSeq c Bool, CSeq c (Enabled (ix,a)), CSeq c counter)
         -- ^ backedge for input, and write request for memory, and internal counter.
fifoFE Witness rst (inp,dec_by) = (inp_done0,wr,in_counter1)
  where
--      mem :: Seq ix -> Seq a
--      mem = pipeToMemory env env wr

        inp_done0 :: CSeq c Bool
        inp_done0 = inp_ready `and2` isEnabled inp

        wr :: CSeq c (Enabled (ix,a))
        wr = packEnabled (inp_done0)
                         (pack (wr_addr,enabledVal inp))

        wr_addr :: CSeq c ix
        wr_addr = resetable rst 0
                $ register 0
                $ mux2 inp_done0 (liftS1 incGroup wr_addr,wr_addr)

        in_counter0 :: CSeq c counter
        in_counter0 = resetable rst 0
                    $ in_counter1
                        + mux2 inp_done0 (1,0)
                        - dec_by

        in_counter1 :: CSeq c counter
        in_counter1 = register 0 in_counter0

--      out :: Seq (Enabled a)
--      out = packEnabled (out_counter1 .>. 0) (mem rd_addr0)

        inp_ready :: CSeq c Bool
        inp_ready = (in_counter1 .<. fromIntegral (size (error "witness" :: ix)))
                        `and2`
                    (bitNot rst)

fifoBE :: forall a c counter ix .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c
        )
      => Witness ix
      -> CSeq c Bool    -- ^ reset
--      -> (Comb Bool -> Comb counter -> Comb counter)
--      -> Seq (counter -> counter)
      -> (CSeq c counter,CSeq c (Enabled a))
        -- inc from FE
        -- input from Memory read
      -> CSeq c Bool
      -> ((CSeq c ix, CSeq c Bool, CSeq c counter), CSeq c (Enabled a))
        -- address for Memory read
        -- dec to FE
        -- internal counter, and
        -- output for HandShaken
fifoBE Witness rst (inc_by,mem_rd) out_ready =
    let
        rd_addr0 :: CSeq c ix
        rd_addr0 = resetable rst 0
                 $ mux2 out_done0 (liftS1 incGroup rd_addr1,rd_addr1)

        rd_addr1 = register 0
                 $ rd_addr0

        out_done0 :: CSeq c Bool
        out_done0 = out_ready `and2` (isEnabled out)

        out :: CSeq c (Enabled a)
        out = packEnabled ((out_counter1 .>. 0) `and2` bitNot rst `and2` isEnabled mem_rd) (enabledVal mem_rd)

        out_counter0 :: CSeq c counter
        out_counter0 = resetable rst 0
                     $ out_counter1
                        + inc_by
                        - mux2 out_done0 (1,0)

        out_counter1 = register 0 out_counter0
    in
        ((rd_addr0, out_done0,out_counter1) , out)

fifoCounter :: forall counter . (Num counter, Rep counter) => Seq Bool -> Seq Bool -> Seq Bool -> Seq counter
fifoCounter rst inc dec = counter1
    where
        counter0 :: Seq counter
        counter0 = resetable rst 0
                 $ counter1
                        + mux2 inc (1,0)
                        - mux2 dec (1,0)

        counter1 = register 0 counter0

fifoCounter' :: forall counter . (Num counter, Rep counter) => Seq Bool -> Seq counter -> Seq counter -> Seq counter
fifoCounter' rst inc dec = counter1
    where
        counter0 :: Seq counter
        counter0 = resetable rst 0
                 $ counter1
                        + inc -- mux2 inc (1,0)
                        - dec -- mux2 dec (1,0)

        counter1 = register 0 counter0

fifo :: forall a c counter ix .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c
        )
      => Witness ix
      -> CSeq c Bool
      -> I (CSeq c (Enabled a)) (CSeq c Bool)
      -> O (CSeq c Bool) (CSeq c (Enabled a))
fifo w_ix rst (inp,out_ready) =
    let
        wr :: CSeq c (Maybe (ix, a))
        inp_ready :: CSeq c Bool
        (inp_ready, wr, _) = fifoFE w_ix rst (inp,dec_by)

        inp_done2 :: CSeq c Bool
        inp_done2 = resetable rst low $ register False $ resetable rst low $ register False $ resetable rst low $ isEnabled wr

        mem :: CSeq c ix -> CSeq c (Enabled a)
        mem = enabledS . pipeToMemory wr

        ((rd_addr0,out_done0,_),out) = fifoBE w_ix rst (inc_by,mem rd_addr0) out_ready

        dec_by = liftS1 (\ b -> mux2 b (1,0)) out_done0
        inc_by = liftS1 (\ b -> mux2 b (1,0)) inp_done2
    in
        (inp_ready, out)

fifoZ :: forall a c counter ix .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c
        )
      => Witness ix
      -> CSeq c Bool
      -> I (CSeq c (Enabled a)) (CSeq c Bool)
      -> O (CSeq c Bool) (CSeq c (Enabled a),CSeq c counter)
fifoZ w_ix rst (inp,out_ready) =
    let
        wr :: CSeq c (Maybe (ix, a))
        inp_ready :: CSeq c Bool
        (inp_ready, wr, counter) = fifoFE w_ix rst (inp,dec_by)

        inp_done2 :: CSeq c Bool
        inp_done2 = resetable rst low $ register False $ resetable rst low $ register False $ resetable rst low $ isEnabled wr

        mem :: CSeq c ix -> CSeq c (Enabled a)
        mem = enabledS . pipeToMemory wr

        ((rd_addr0,out_done0,_),out) = fifoBE w_ix rst (inc_by,mem rd_addr0) out_ready

        dec_by = liftS1 (\ b -> mux2 b (1,0)) out_done0
        inc_by = liftS1 (\ b -> mux2 b (1,0)) inp_done2
    in
        (inp_ready, (out,counter))

{-
fifoToMatrix :: forall a counter counter2 ix iy iz c .
         (Size counter
        , Size ix
        , Size counter2, Rep counter2, Num counter2
        , counter ~ ADD ix X1
        , counter2 ~ ADD iy X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Size iy
        , Rep iy, StdLogic ix, StdLogic iy, StdLogic a,
        WIDTH ix ~ ADD (WIDTH iz) (WIDTH iy),
        StdLogic counter, StdLogic counter2,
        StdLogic iz, Size iz, Rep iz, Num iy
        , WIDTH counter ~ ADD (WIDTH iz) (WIDTH counter2)
        , Num iz
        , Clock c
        )
      => Witness ix
      -> Witness iy
      -> CSeq c Bool
      -> HandShaken c (CSeq c (Enabled a))
      -> HandShaken c (CSeq c (Enabled (M.Matrix iz a)))
fifoToMatrix w_ix@Witness w_iy@Witness rst hs = HandShaken $ \ out_ready ->
    let
        wr :: CSeq c (Maybe (ix, a))
        wr = fifoFE w_ix rst (hs,dec_by)

        inp_done2 :: CSeq c Bool
        inp_done2 = resetable rst low
                  $ register False
                  $ resetable rst low
                  $ register False
                  $ resetable rst low
                  $ isEnabled wr

        mem :: CSeq c (Enabled (M.Matrix iz a))
        mem = enabledS
                $ pack
                $ fmap (\ f -> f rd_addr0)
                $ fmap pipeToMemory
                $ splitWrite
                $ mapEnabled (mapPacked $ \ (a,d) -> (factor a,d))
                $ wr

        ((rd_addr0,out_done0),out) = fifoBE w_iy rst (inc_by,mem) <~~ out_ready

        dec_by = mulBy (Witness :: Witness iz) out_done0
        inc_by = divBy (Witness :: Witness iz) rst inp_done2
    in
        out

-- Move into a Commute module?
-- classical find the implementation problem.
splitWrite :: forall a a1 a2 d c . (Rep a1, Rep a2, Rep d, Size a1) => CSeq c (Pipe (a1,a2) d) -> M.Matrix a1 (CSeq c (Pipe a2 d))
splitWrite inp = M.forAll $ \ i -> let (g,v)   = unpackEnabled inp
                                       (a,d)   = unpack v
                                       (a1,a2) = unpack a
                                    in packEnabled (g .&&. (a1 .==. pureS i))
                                                   (pack (a2,d))

-}
mulBy :: forall x sz c . (Clock c, Size sz, Num sz, Num x, Rep x) => Witness sz -> CSeq c Bool -> CSeq c x
mulBy Witness trig = mux2 trig (pureS $ fromIntegral $ size (error "witness" :: sz),pureS 0)

divBy :: forall x sz c . (Clock c, Size sz, Num sz, Rep sz, Num x, Rep x) => Witness sz -> CSeq c Bool -> CSeq c Bool -> CSeq c x
divBy Witness rst trig = mux2 issue (1,0)
        where
                issue = trig .&&. (counter1 .==. (pureS $ fromIntegral (size (error "witness" :: sz) - 1)))

                counter0 :: CSeq c sz
                counter0 = cASE [ (rst,0)
                                , (trig,counter1 + 1)
                                ] counter1
                counter1 :: CSeq c sz
                counter1 = register 0
                         $ mux2 issue (0,counter0)



{-
-- Turn a Hand Shaken signal into a packetized Hand Shaken signal.
mkPacketFIFO ::

-}




