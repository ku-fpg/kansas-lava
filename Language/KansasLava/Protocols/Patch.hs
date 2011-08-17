{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, TypeOperators, NoMonomorphismRestriction #-}
module Language.KansasLava.Protocols.Patch
	where

import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Types

import Language.KansasLava.Rep
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Seq
import Language.KansasLava.Signal
import Language.KansasLava.Probes

import Data.Sized.Unsigned (U8)
import Data.Sized.Matrix as M

import qualified Data.ByteString.Lazy as B
import Control.Applicative

---------------------------------------------------------------------------
-- The Patch.
---------------------------------------------------------------------------

type Patch lhs_in 	   rhs_out	
	   lhs_out         rhs_in 	
	= (lhs_in,rhs_in) -> (lhs_out,rhs_out)

---------------------------------------------------------------------------
-- Executing the Patch, monadic style.
---------------------------------------------------------------------------

-- | A common pattern, a single immutable structure on the output.
unitPatch :: a -> Patch () a
		        () ()
unitPatch a = \ _ -> ((),a)

runPatch :: (Unit u1, Unit u2)
 	 => Patch u1   a
	 	  u2  () -> a
runPatch p = a
 where
   (_,a) = p (unit,unit)

-- TODO: rm
nullPatch :: Patch a  a
		   b  b
nullPatch ~(a,b) = (b,a)

idPatch :: Patch a  a
	         b  b
idPatch ~(a,b) = (b,a)

fstPatch :: Patch a   b
		  c   e -> Patch (a :> f) (b :> f)
				 (c :> g) (e :> g)
fstPatch p = p `stack` nullPatch

sndPatch :: Patch a   b
		  c   d -> Patch (f :> a) (f :> b)
				 (g :> c) (g :> d)
sndPatch p = nullPatch `stack` p

data Iso a b = Iso (a -> b) (b -> a)

forwardPatch :: (li -> ro)
	    -> Patch li ro
	             b  b
forwardPatch f1 ~(li,ri) = (ri,f1 li)

backwardPatch :: (ri -> lo) 
	    -> Patch a  a
	             lo ri
backwardPatch f2 ~(li,ri) = (f2 ri,li)

-- TODO: above or ??
infixr 3 `stack`
stack :: Patch li1		ro1
               lo1    		ri1
      -> Patch li2		ro2
               lo2    		ri2
      -> Patch (li1 :> li2)			(ro1 :> ro2)
               (lo1 :> lo2)   			(ri1 :> ri2)
stack p1 p2 inp = (lo1 :> lo2,ro1 :> ro2)
   where
	(li1 :> li2,ri1 :> ri2)	     = inp
	(lo1,ro1)		     = p1 (li1,ri1)
	(lo2,ro2)		     = p2 (li2,ri2)

matrixStack :: (m ~ (Matrix x), Size x)
 	=> m (Patch li   ro
	   	    lo   ri)
	-> Patch (m li)         (m ro)
		 (m lo)         (m ri)
matrixStack m inp = ( fmap (\ (l,_) -> l) m'
		    , fmap (\ (_,r) -> r) m'
		    )
   where
	(m_li,m_ri)	= inp
	m' = (\ p li ri -> p (li,ri)) <$> m <*> m_li <*> m_ri


-- | loopPatch is a fixpoint style combinator, for backedges.

loopPatch :: Patch (a :> b) (a :> c)
		   (d :> e) (d :> f) 
	  -> Patch b c
		   e f

loopPatch g ~(b,f) = (e,c)
  where
    (d:>e,a:>c) = g (a:>b,d:>f)

------------------------------------------------
-- Unit
------------------------------------------------

class Unit unit where
	unit :: unit

instance Unit () where unit = ()
instance (Unit a,Unit b) => Unit (a,b) where unit = (unit,unit)
instance (Unit a,Unit b) => Unit (a :> b) where unit = (unit :> unit)

------------------------------------------------
-- File I/O for Patches
------------------------------------------------

-- | 'readPatch' reads a file into Patch, which will become the
-- lefthand side of a chain of patches.
rawReadPatch :: FilePath -> IO (Patch ()			[Maybe U8]
			              ()			())
rawReadPatch fileName = do
     fileContents <- B.readFile fileName
     return $ unitPatch $ map (Just . fromIntegral) $ B.unpack fileContents

readPatch :: (Read a)
		  => FilePath -> IO (Patch ()	[Maybe a]
	  		                   ()	())
readPatch fileName = do
     fileContents <- readFile fileName
     return $ unitPatch $ map (Just . read) $ words $ fileContents

-- | 'writePatch' runs a complete circuit for the given 
-- number of cycles, writing the result to a given file.
rawWritePatch :: FilePath 
	   -> Int
	   -> Patch () 		[Maybe U8] 
	 	    ()		()
	   -> IO ()
rawWritePatch fileName n patch = do
  B.writeFile fileName $ B.pack [ fromIntegral x  | Just x <- take n $ runPatch patch ]

writePatch :: (Show a)
	   => FilePath 
	   -> Int
	   -> Patch () 		[Maybe a]
	 	    ()	 	()
	   -> IO ()
writePatch fileName n patch = do
  writeFile fileName $ unlines [ show x | Just x <- take n $ runPatch patch ]

-- | 'comparePatch' compares the input to the contents of a file, if there
-- there a mismatch, it will print a message giving the element number that 
-- failed.  If everything matches, it prints out a "Passed" message
comparePatch :: (Read a, Show a, Eq a)
             => FilePath
             -> Int
             -> Patch ()     [Maybe a]
                      ()     ()
             -> IO (Patch ()     [Maybe a]
                          ()     () )
comparePatch fileName n patch = do
     fileContents <- readFile fileName
     let expectedVals = map read $ words $ fileContents
     listComparePatch expectedVals n patch

-- | 'listComparePatch' compares the input to the contents of a list, if 
-- there is a mismatch, it will print a message giving the element number 
-- that failed.  If everything matches, it prints out a "Passed" message
listComparePatch :: (Read a, Show a, Eq a)
             => [a]
             -> Int
             -> Patch ()     [Maybe a]
                      ()     ()
             -> IO (Patch ()     [Maybe a]
                          ()     () )
listComparePatch expectedVals n patch = do
     let inp = runPatch patch
     let incomingVals = [ x | Just x <- take n $ inp ]
     putStr $ checkAgainstList 0 expectedVals incomingVals
     return $ unitPatch inp
  where
     checkAgainstList :: (Show a, Eq a) => Int -> [a] -> [a] -> String
     checkAgainstList elemNum []      _                = "Passed, quantity compared:  " ++ (show elemNum)
     checkAgainstList elemNum _       []               = "Passed, but some data in file was not reached, quantity compared:  " ++ (show elemNum)
     checkAgainstList elemNum (e:exs) (i:ins) | (e==i) = checkAgainstList (elemNum+1) exs ins
     checkAgainstList elemNum (e:_)   (i:_)            = "Failed:  Element " ++ (show (elemNum+1)) ++ ": \tExpected:  " ++ (show e) ++ " \tActual:  " ++ (show i)

---------------------------------------------------------------------------
-- Functions that connect streams
---------------------------------------------------------------------------

infixr 5 $$
bus = ($$)

infixr 5 `bus`
($$), bus :: 
  	 Patch li1 	o
	       lo1  	i
      -> Patch o 	ro2
	       i 	ri2 
      -> Patch li1	ro2	
	       lo1 	ri2
(p1 $$ p2) inp = (lhs_out1,rhs_out2)
   where
	(lhs_in,rhs_in) 	     = inp
	(lhs_out1,rhs_out1) = p1 (lhs_in,lhs_out2)
	(lhs_out2,rhs_out2) = p2 (rhs_out1,rhs_in)

-- | 'readyToAckBridge' converts from a ready interface to an ACK interface 
-- by preemptively giving the ready signal, and holding the resulting data 
-- from the device on the input side if no ACK is received by the device on 
-- the output side.  If data is currently being held, then the ready signal 
-- will not be given.  This bridge is fine for deep embedding (can be 
-- represented in hardware).
readyToAckBridge :: forall a c sig . (Rep a, Clock c, sig ~ CSeq c)
    => Patch    (sig (Enabled a))           (sig (Enabled a))
                (sig Ready)                 (sig Ack)
readyToAckBridge ~(inp, ack_in0) = (toReady ready, out)
    where
        (in_en, in_val) = unpack inp
        ack_in = fromAck ack_in0

        captureData = in_en .&&. bitNot dataHeld .&&. bitNot ack_in

        dataHolder  = delay
                    $ cASE [ (captureData, in_val)
                           ] dataHolder

        dataHeld    = register False
                    $ cASE [ (captureData, high)
                           , (ack_in, low)
                           ] dataHeld

        out         = cASE [ (dataHeld, packEnabled dataHeld dataHolder)
                           ] inp

        ready       = bitNot dataHeld

-- | 'ackToReadyBridge' converts from a Ack interface to an Ready interface 
-- by ANDing the ready signal from the receiving component with the input 
-- enable from the sending component.  This may not be necessary at times 
-- if the sending component ignores ACKs when no data is sent. This bridge 
-- is fine for deep embedding (can be represented in hardware).
ackToReadyBridge :: (Rep a, Clock c, sig ~ CSeq c)
    => Patch    (sig (Enabled a))           (sig (Enabled a))
                (sig Ack)                 (sig Ready)
ackToReadyBridge ~(inp, ready_in) = (toAck ack, out)
    where
        out = inp
        ack = (fromReady ready_in) .&&. (isEnabled inp)

-- | 'unsafeAckToReadyBridge' converts from a Ack interface to an Ready interface
-- by running the ready signal from the receiving component to the Ack input for 
-- the sending component.  This may unsafe if the sending component does not 
-- ignore Acks when no data is sent.  Otherwise, this should be safe.  This 
-- bridge is fine for deep embedding (can be represented in hardware).
unsafeAckToReadyBridge :: (Rep a, Clock c, sig ~ CSeq c)
    => Patch    (sig (Enabled a))           (sig (Enabled a))
                (sig Ack)                 (sig Ready)
unsafeAckToReadyBridge ~(inp, ready_in) = (toAck ack, out)
    where
        out = inp
        ack = fromReady ready_in

{-
-- | A 'bridge' is way of connecting a HandShake to a MailBox.
-- This is efficent; internally an 'and' gate, and represents a 
-- good 'bridging' interface, because both side can play master, 
-- and try initiate the transfer at the same time, possibly 
-- improving clock speeds.

-- This does feel like the correct place for this

bridge :: (Rep a, Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled a)) 		(sig (Enabled a)) 
		 (sig Ack) 			(sig Ready) 
bridge (inp,ready) = (toAck ack,out)
   where
	ack = isEnabled inp `and2` fromReady ready
	out = packEnabled ack (enabledVal inp)
-}

probePatch :: (Probe a, Probe b)
   => String      
   -> Patch    a   a
               b   b
probePatch probeName ~(inp1, inp2) = (out2, out1)
   where
       (out1, out2) = id
                    $ probe probeName
                    $ (inp1, inp2)

probeDataPatch :: (Probe a)
    => String      
    -> Patch    a        a
                b        b
probeDataPatch probeName ~(inp1, inp2) = (out2, out1)
    where
        out1 = id
             $ probe probeName
             $ inp1
        out2 = inp2

probeHandshakePatch :: (Probe b)
    => String      
    -> Patch    a        a
                b        b
probeHandshakePatch probeName ~(inp1, inp2) = (out2, out1)
    where
        out1 = inp1
        out2 = id
             $ probe probeName
             $ inp2

---------------------------------------------------------------------------------
-- Functions that fork streams.
---------------------------------------------------------------------------------

-- | This duplicates the incomming datum.
-- This has the behavior that neither branch sees the value
-- until both can recieve it.
dupPatch :: (Clock c, sig ~ CSeq c, Rep a)
         => Patch (sig (Enabled a))     (sig (Enabled a)  :> sig (Enabled a))	
	          (sig Ack)             (sig Ack      :> sig Ack) 
dupPatch = ackToReadyBridge $$ dupPatch' $$ stack readyToAckBridge readyToAckBridge where
 dupPatch' ~(inp,rA :> rB) = (toReady go, (out :> out)) 
    where
	go = fromReady rA .&&. fromReady rB
	out = packEnabled (go .&&. isEnabled inp) (enabledVal inp)

-- | This duplicate the incoming datam over many handshaken streams.
matrixDupPatch :: (Clock c, sig ~ CSeq c, Rep a, Size x)
         => Patch (sig (Enabled a))     (Matrix x (sig (Enabled a)))
	          (sig Ack)             (Matrix x (sig Ack))
matrixDupPatch = ackToReadyBridge $$ matrixDupPatch' $$ matrixStack (pure readyToAckBridge) where
 matrixDupPatch' ~(inp,readys) = (toReady go, pure out) 
    where
	go = foldr1 (.&&.) $ map fromReady $ M.toList readys
	out = packEnabled (go .&&. isEnabled inp) (enabledVal inp)

unzipPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep b)
         => Patch (sig (Enabled (a,b)))     (sig (Enabled a) :> sig (Enabled b))
	          (sig Ack)                 (sig Ack       :> sig Ack)
unzipPatch = dupPatch $$ 
		stack (forwardPatch $ mapEnabled (fst . unpack))
		      (forwardPatch $ mapEnabled (snd . unpack))

matrixUnzipPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x)
         => Patch (sig (Enabled (Matrix x a)))    (Matrix x (sig (Enabled a)))
	          (sig Ack)          		  (Matrix x (sig Ack))
matrixUnzipPatch = 
	matrixDupPatch $$
	matrixStack (forAll $ \ x ->  forwardPatch (mapEnabled $ \ v -> v .!. pureS x))


deMuxPatch :: forall c sig a . (Clock c, sig ~ CSeq c, Rep a)
  => Patch (sig (Enabled Bool)    :> sig (Enabled a)) 		  (sig (Enabled a) :> sig (Enabled a))
	   (sig Ack               :> sig Ack)		          (sig Ack	   :> sig Ack)
deMuxPatch = fe $$ matrixDeMuxPatch $$ be
  where
	fe = fstPatch (forwardPatch ((unsigned)))
	be = backwardPatch (\ ~(b :> c) -> matrix [c,b]) `bus`
	     forwardPatch (\ m -> ((m M.! (1 :: X2)) :> (m M.! 0)))
	

matrixDeMuxPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x)
  => Patch (sig (Enabled x)    :> sig (Enabled a)) 		  (Matrix x (sig (Enabled a)))
	   (sig Ack            :> sig Ack)		          (Matrix x (sig Ack))
matrixDeMuxPatch = matrixDeMuxPatch' $$ matrixStack (pure readyToAckBridge) where
 matrixDeMuxPatch' ~(ix :> inp, m_ready) = (toAck ackCond :> toAck ackIn,out) 
    where
	-- set when ready to try go
	go = isEnabled ix .&&. isEnabled inp .&&. fromReady (pack m_ready .!. enabledVal ix)

	-- outputs
	ackCond = go
	ackIn   = go
	out     = forAll $ \ x -> packEnabled (go .&&. enabledVal ix .==. pureS x) (enabledVal inp)

---------------------------------------------------------------------------------
-- Functions that combine streams
---------------------------------------------------------------------------------

-- no unDup (requires some function / operation, use zipPatch).

zipPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep b)
  => Patch (sig (Enabled a)  :> sig (Enabled b))	(sig (Enabled (a,b)))
	   (sig Ack          :> sig Ack)	  	(sig Ack)
zipPatch = zipPatch' $$ readyToAckBridge where
 zipPatch' ~(in1 :> in2, outReady) = (toAck ack1 :> toAck ack2, out) 
    where
	go = fromReady outReady .&&. isEnabled in1 .&&. isEnabled in2
	ack1 = go
	ack2 = go

	out = packEnabled go (pack (enabledVal in1, enabledVal in2))

matrixZipPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x)
         => Patch (Matrix x (sig (Enabled a)))	(sig (Enabled (Matrix x a)))   
	          (Matrix x (sig Ack))		(sig Ack)          		  
matrixZipPatch = matrixZipPatch' $$ readyToAckBridge where
 matrixZipPatch' ~(mIn, outReady) = (mAcks, out)
   where
	go    = fromReady outReady .&&. foldr1 (.&&.) (map isEnabled $ M.toList mIn)
	
	mAcks = fmap toAck $ pure go
	mIn'  = fmap enabledVal mIn 
	out   = packEnabled go (pack mIn' :: sig (Matrix x a))

-- | 'muxPatch' chooses a the 2nd or 3rd value, based on the Boolean value.
muxPatch :: (Clock c, sig ~ CSeq c, Rep a)
  => Patch (sig (Enabled Bool) :> sig (Enabled a)  :> sig (Enabled a))	(sig (Enabled a))
	   (sig Ack            :> sig Ack          :> sig Ack)	  	(sig Ack)

muxPatch = fe `bus` matrixMuxPatch
   where
	fe = forwardPatch (\ ~(a :> b :> c) -> ((unsigned) a :> matrix [c,b])) `bus`
	     backwardPatch (\ ~(a :> m) -> (a :> (m M.! (1 :: X2)) :> (m M.! 0)))

-- | 'matrixMuxPatch' chooses the n-th value, based on the index value.
matrixMuxPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x)
  => Patch (sig (Enabled x)    :> Matrix x (sig (Enabled a)))		(sig (Enabled a))
	   (sig Ack            :> Matrix x (sig Ack))		  	(sig Ack)
matrixMuxPatch = matrixMuxPatch' $$ readyToAckBridge where
 matrixMuxPatch' ~((cond :> m),ack) = ((toAck ackCond :> m_acks),out)
   where
	-- set when conditional value on cond port
	go = fromReady ack .&&. isEnabled cond

	-- only respond/ack when you are ready to go, the correct lane, and have input
	acks :: Matrix x (sig Bool)
	acks = forEach m $ \ x inp -> go 
				.&&. (enabledVal cond .==. pureS x) 
				.&&. isEnabled inp

	-- TODO: make this balanced
	ackCond = foldr1 (.||.) $ M.toList acks
	m_acks = fmap toAck acks

	out = cASE (zip (M.toList acks) (M.toList m))
		   disabledS


---------------------------------------------------------------------------------
-- Trivial FIFOs
---------------------------------------------------------------------------------

-- (There are larger FIFO's in the Kansas Lava Cores package.)


fifo2 :: forall c sig a . (Clock c, sig ~ CSeq c, Rep a) 
    => Patch (sig (Enabled a)) 	 (sig (Enabled a))
             (sig Ack)         (sig Ack)
fifo2 = ackToReadyBridge $$ fifo2' where
 fifo2' ~(inp,ack) = (toReady ready, out)
    where
        dataIncoming = isEnabled inp

        dataOutRead = (state ./=. 0) .&&. fromAck ack

        -- State:
        --   0 = First empty, second empty
        --   1 = First full, second empty
        --   2 = First empty, second full
        --   3 = First full, second full, read second store
        --   4 = First full, second full, read first store
        state :: sig X5
        state = register 0
              $ cASE [ ((state.==.0) .&&. dataIncoming,                  1)
                     , ((state.==.1) .&&. dataIncoming .&&. dataOutRead, 2)
                     , ((state.==.1) .&&. dataIncoming,                  4)
                     , ((state.==.1) .&&. dataOutRead,                   0)
                     , ((state.==.2) .&&. dataIncoming .&&. dataOutRead, 1)
                     , ((state.==.2) .&&. dataIncoming,                  3)
                     , ((state.==.2) .&&. dataOutRead,                   0)
                     , ((state.==.3) .&&. dataOutRead,                   1)
                     , ((state.==.4) .&&. dataOutRead,                   2)
                     ] state

        fstStore :: sig a
        fstStore = cASE [ (((state.==.0) .||. (state.==.2)) .&&. dataIncoming, enabledVal inp)
                        ]
                 $ delay fstStore

        sndStore :: sig a
        sndStore = cASE [ ((state.==.1) .&&. dataIncoming, enabledVal inp)
                        ]
                 $ delay sndStore

        ready = (state ./=. 3) .&&. (state ./=. 4)

        outval = cASE [ (((state.==.2) .||. (state.==.3)), sndStore)
                      ] fstStore

        out = packEnabled (state ./=. 0) outval

fifo1 :: forall c sig a . (Clock c, sig ~ CSeq c, Rep a) 
      => Patch (sig (Enabled a)) 	(sig (Enabled a))
	       (sig Ack)		(sig Ack)
fifo1 ~(inp,ack) = (toAck have_read, out)
   where
	have_read = (state .==. 0) .&&. isEnabled inp
	written   = (state .==. 1) .&&. fromAck ack

	state :: sig X2
	state = register 0
	      $ cASE [ (have_read,	pureS 1)
		     , (written,	pureS 0)
		     ] state

	store :: sig a
	store = cASE [ (have_read,enabledVal inp)
		     ]
	      $ delay store

	out = packEnabled (state .==. 1) store

---------------------------------------------------------------------------------
-- Retiming
---------------------------------------------------------------------------------

matrixExpandPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x, Num x, Enum x)
         => Patch (sig (Enabled (Matrix x a)))	(sig (Enabled a)) 
	          (sig Ack)			(sig Ack)
matrixExpandPatch =
	   forwardPatch (\ a -> (() :> a))
	$$ backwardPatch (\ (_ :> b) -> b)
	$$ stack 
		 (unitPatch (coord :: Matrix x x) $$ cyclePatch)
		 (matrixUnzipPatch)
	$$ matrixMuxPatch

matrixContractPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x, Num x, Enum x)
         => Patch (sig (Enabled a)) (sig (Enabled (Matrix x a)))	
	          (sig Ack)	    (sig Ack)
matrixContractPatch =
	   forwardPatch (\ a -> (() :> a))
	$$ backwardPatch (\ (_ :> b) -> b)
	$$ fstPatch (unitPatch (coord :: Matrix x x) $$ cyclePatch)
	$$ matrixDeMuxPatch
	$$ matrixZipPatch

---------------------------------------------------------------------------------
-- Other stuff
---------------------------------------------------------------------------------

-- | unitClockPatch forces the handshaking to use the unit clock. Which is useful for testing.
unitClockPatch :: (sig ~ CSeq ()) =>
	Patch (sig a)		(sig a)
	      (sig b)           (sig b)
unitClockPatch ~(li,ri) = (ri,li)


-- | cyclePatch cycles through a constant list (actually a matrix) of values.
-- Generates an async ROM on hardware.
cyclePatch :: forall a c ix sig .
        ( Size ix
        , Rep a
        , Rep ix
        , Num ix
        , Clock c
	, sig ~ CSeq c
        )
	=> Patch (Matrix ix a)	(sig (Enabled a))
	         ()		(sig Ack)
cyclePatch ~(m,ack) = ((),out)
  where
	ix :: sig ix
	ix = register 0
	   $ cASE [ (fromAck ack, ix + 1) ]
		  ix

	out = packEnabled high
            $ funMap (\ x -> return (m M.! x))
		     ix
	


-----------------------------------------------------------------------

{-
 - TODO: Change to 
   bus    --> >==>
   bridge --> 
   >==>, >~~> goes away (use >==>)
 -}

{-
-- Mailbox based signal with Ready
infixr 5 >==>
(>==>) :: (Clock c, sig ~ CSeq c, back ~ Ready, o ~ Enabled a, Rep a)
      => Patch li1 		(sig o)
	       lo1  	 s1	(sig back) 
      -> Patch (sig o) 		ro2
	       (sig back) s2  	ri2 
      -> Patch li1 			ro2	
	       lo1 (s1 :>        s2) 	ri2
(>==>) = bus

-- HandShake-based signal with Ack
infixr 5 >~~>
(>~~>) :: (Clock c, sig ~ CSeq c, back ~ Ack, o ~ Enabled a, Rep a)
      => Patch li1 		(sig o) 
	       lo1  	 s1	(sig back) 
      -> Patch (sig o) 		ro2
	       (sig back) s2  	ri2 
      -> Patch li1 			ro2	
	       lo1 (s1 :>        s2) 	ri2
(>~~>) = bus

-- HandShake-based left hand side, Mailbox based right hand side.
infixr 5 >~=>
(>~=>) :: (Clock c, sig ~ CSeq c, o ~ Enabled a, Rep a)
      => Patch li1 		(sig o)
	       lo1  	 s1	(sig Ack)
      -> Patch (sig o) 		ro2
	       (sig Ready) s2  	ri2 
      -> Patch li1 			ro2	
	       lo1 (s1 :>        s2) 	ri2
p1 >~=> p2 = mapStatus (\ (a :> () :> b) -> (a :> b)) patch 
   where patch = p1 `bus` bridge `bus` p2

--------------------------------------------------------------------------------
-- Stuff that will go
--------------------------------------------------------------------------------

mapStatus :: (a -> b) -> Patch lhs_in rhs_out lhs_out a rhs_in
 		     -> Patch lhs_in rhs_out lhs_out b rhs_in
mapStatus f p inp = let (lhs_out,bot_out,rhs_out) = p inp
		    in (lhs_out,f bot_out,rhs_out)

noStatus :: Patch lhs_in rhs_out lhs_out a  rhs_in
         -> Patch lhs_in rhs_out lhs_out () rhs_in
noStatus = mapStatus (const ())

-}

--------------------------------------------------

{-
- an idea
packPatch :: (Clock c, sig ~ CSeq c, Rep in1, Rep in2)
	Patch (sig in1 :> sig in2)			(sig (in1 :> in2))
	      (sig Ready :> sig Ready)		()	(sig Ack)
-}

--------------------------------------------------

{-
-- WRONG place
beat :: (Clock c, sig ~ CSeq c) => 
	Patch ()		(sig (Enabled ()))
	      ()	()	(sig Ack)
beat ~(_,_) = ((),(),enabledS (pureS ()))
-}

{-
matrixDupPatch :: (Clock c, sig ~ CSeq c, Rep a)
         => Patch (sig (Enabled a))     (sig (Enabled a)  :> sig (Enabled a))	
	          (sig Ready)        () (sig Ready         :> sig Ready) 
matrixDupPatch ~(inp,rA :> rB) = (toReady go, (), (out :> out))
  where
	go = fromReady rA .&&. fromReady rB
	out = packEnabled (go .&&. isEnabled inp) (enabledVal inp)
-}


{-
TODO!
unpackPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep b)
  => Patch (sig (Enabled a)  :> sig (Enabled b))	(sig (Enabled (a,b)))
	   (sig Ack          :> sig Ack)	  ()	(sig Ready)
unpackPatch ~(in1 :> in2, outReady) = (toAck ack1 :> toAck ack2, (), out)
   where
	go = fromReady outReady .&&. isEnabled in1 .&&. isEnabled in2
	ack1 = go
	ack2 = go

	out = packEnabled go (pack (enabledVal in1, enabledVal in2))
-}

-- groupPatch :: 

{-
chopPatch :: Patch (sig (Enabled (Matrix x a))		(sig (Enabled a))
		   (sig Ack)				(sig ?)
chopPatch = 
-}	

{-
 OLD CODE
muxPatch ~((cond :> sA :> sB),ack) = ((toAck ackCond :> toAck ackA :> toAck ackB),(),out)
   where
	-- set when conditional value on cond port
	go = fromReady ack .&&. isEnabled cond
	
	-- when are you ack'd the input?
	ackA = go .&&. enabledVal cond          .&&. isEnabled sA		
	ackB = go .&&. bitNot (enabledVal cond) .&&. isEnabled sB

	-- ack the conditional if *either* A or B are accepted.
	ackCond = ackA .||. ackB

	-- output depending on what is accepted.
	out = cASE [ (ackA, sA)
		   , (ackB, sB)
		   ] disabledS
-}
