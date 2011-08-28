{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, TypeOperators, NoMonomorphismRestriction #-}
-- |
-- When writing interacting sub-components for hardware,  some form of handshaking or intra-component cooperation is required. One common solution is using central control logic. This allows for maximizing global throughpu-- t, but at the cost of composability of the sub-components. Another solution is to allow components to act independently, and throttle the communicate between components using bus protocols.
--
-- A simple bus protocol is using validity and acknowledge signals beside a data signal.
--
-- [ABOUT]
--    inwardcomponent :: (sig Bool,sig a) -> sig Ack
--    outwardcomponent ::  sig Ack -> (sig Bool,sig a)
--
-- [In Kansas Lava]
--
--    outcomponent :: sig Ack  ->  sig (Maybe a)
--    ..
--
-- If we consider a component that is part of a computation pipeline, such as a stream processor from type A to type B, we have the type.
--
-- 	:: ... -> (sig (Maybe A), sig Ack) -> (sig Ack, sig (Maybe B))
--
--
--                  +-----------+
-- sig (Maybe a) -->|           |--> sig (Maybe B)
--                  | component |
-- sig Ack       <--|           |<-- sig Ack
--                  +-----------+
--
-- That is we have a signal A in, with its Ack back, and a signal B out, with the Ack in from the right hand side.
--
-- If we label the ports
--
--      +-----------+
-- A -->|           |--> B
--      | component |
-- C <--|           |<-- D
--      +-----------+
--
-- We have
--
--   component :: ... -> (A,D) -> (C,B)
--
-- So the left hand of each tuple is the left hand side connections, and right hand side of the tuple is the right hand side connections.
--
--
-- This form, A,D -> C,B is hard to read, we invent an abstraction to help us.
--
-- data Patch a b c d = (a,d) (c,b)
--
-- In this way, our "Patch" can be written
--
--   Patch (sig (Maybe a)) (sig (Maybe b))
--         (sig Ack)       (sig Ack)
--
-- This style allows the type synonym to pictorially represent the patch directly.
--
-- Once we have settled on the datatype Patch, the next question is can we build
-- combinators to help us thread together these patches.
--
-- ($$) ::
--   	 Patch li1 	o
-- 	       lo1  	i
--       -> Patch o 	ro2
-- 	       i 	ri2
--       -> Patch li1	ro2
-- 	       lo1 	ri2
--
-- This composes two patches left to right, in a pipeline.
--
-- Using this patch idiom, we can represent fifos, stream processors,
-- and various protocols between Patches, all in a type save manner.
--
-- 			---------------
--
-- How do we represent two handshaken signals going into a signal
-- processor?
--
-- zipPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep b)
--   => Patch (sig (Enabled a)  :> sig (Enabled b))  (sig (Enabled (a,b)))
-- 	   (sig Ack          :> sig Ack)	  (sig Ack)
--
-- :> is an infix tuple, with a right associativity.
--
-- zipPatch takes two streams, and merges them, aka zip. We line up the sig (Enabled ..) above the corresponding Ack.
--
-- As a final example, consider
--
-- matrixContractPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x, Num x, Enum x)
--          => Patch (sig (Enabled a)) (sig (Enabled (Matrix x a)))
-- 	          (sig Ack)	    (sig Ack)
-- matrixContractPatch =
-- 	   forwardPatch (\ a -> (() :> a))
-- 	$$ backwardPatch (\ (_ :> b) -> b)
-- 	$$ fstPatch (unitPatch (coord :: Matrix x x) $$ cyclePatch)
-- 	$$ matrixDeMuxPatch
-- 	$$ matrixZipPatch
--
-- What happens here? forward/backwards create a new channel, fstPatch creates a stream of values. matrixDeMuxPatch takes the stream to bits, and matrixZipPatch builds a single Matrix.
--
--
--                             |       |-->|     |
--   ()-> (coords --> cycle) ->| DeMux |-->| zip |
-- -->--> -------------------->|       |-->|     |-->
module Language.KansasLava.Protocols.Patch
  -- (Patch, bus, ($$),
  -- idPatch,forwardPatch, backwardPatch,dupPatch,zipPatch,
  -- fstPatch,matrixDupPatch,matrixStack,matrixZipPatch,
  -- fifo1, fifo2)
  where

import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Types

import Language.KansasLava.Rep
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Seq
import Language.KansasLava.Comb
import Language.KansasLava.Signal
import Language.KansasLava.Probes

import Data.Sized.Unsigned (U8)
import Data.Sized.Matrix as M

import qualified Data.ByteString.Lazy as B
import Control.Applicative

---------------------------------------------------------------------------
-- The Patch.
---------------------------------------------------------------------------

-- | A Patch is a data signal with an associated control signal. The 'lhs_in'
-- type parameter is the type of the data input, the 'rhs_out' type parameter is
-- the type of the data output. The 'rhs_in' is the type of the control input
-- (e.g. a 'ready' signal), and the 'rhs_out' is the type of the control output
-- (e.g. 'ack').
type Patch lhs_in 	   rhs_out
	   lhs_out         rhs_in
	= (lhs_in,rhs_in) -> (lhs_out,rhs_out)

---------------------------------------------------------------------------
-- Executing the Patch, monadic style.
---------------------------------------------------------------------------

-- | Unitpatch produces a constant data output. The control inputs/outputs are
-- unit, so they contain no data.
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
nullPatch = idPatch -- ~(a,b) = (b,a)

-- | A patch that passes through data and control.
idPatch :: Patch a  a
	         b  b
idPatch ~(a,b) = (b,a)

-- | Given a patch, add add to the data and control inputs/outputs a second set
-- of signal that are passed-through. The signals of the argument patch o fstPatch will
-- appear as the first element of the pair in the resulting patch.
fstPatch :: Patch a   b
		  c   e -> Patch (a :> f) (b :> f)
				 (c :> g) (e :> g)
fstPatch p = p `stack` nullPatch

-- | Given a patch, add add to the data and control inputs/outputs a second set
-- of signal that are passed-through. The signals of the argument patch o fstPatch will
-- appear as the second element of the pair in the resulting patch.
sndPatch :: Patch a   b
		  c   d -> Patch (f :> a) (f :> b)
				 (g :> c) (g :> d)
sndPatch p = nullPatch `stack` p

-- | Lift a function to a patch, applying the function to the data input.
forwardPatch :: (li -> ro)
	    -> Patch li ro
	             b  b
forwardPatch f1 ~(li,ri) = (ri,f1 li)

-- | Lift a function to a patch, applying the function to the control input.
backwardPatch :: (ri -> lo)
	    -> Patch a  a
	             lo ri
backwardPatch f2 ~(li,ri) = (f2 ri,li)

-- TODO: above or ??

infixr 3 `stack`
-- | Given two patches, tuple their data/control inputs and outputs.
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

-- | Given a homogeneous list (Matrix) of patches, combine them into a single
-- patch, collecting the data/control inputs/outputs into matrices.
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


openPatch :: Patch c (() :> c)
	           d (() :> d)
openPatch = forwardPatch (\ a -> (() :> a)) $$
	    backwardPatch (\ ~(_ :> a) -> a)

-------------------------------------------------------------------------------
-- Sink Patches - throw away (ignore) data
-------------------------------------------------------------------------------

mapPatch :: forall a b c sig . (Num a, Rep a, Rep b, Clock c, sig ~ CSeq c)
	 => (Comb a -> Comb b)
	 -> Patch (sig (Enabled a)) (sig (Enabled b))
	   	  (sig Ack)	    (sig Ack)
mapPatch = forwardPatch . mapEnabled
	

-------------------------------------------------------------------------------
-- Sink Patches - throw away (ignore) data
-------------------------------------------------------------------------------

-- | A sink patch throws away its data input (generating a () data
-- output). sinkReadyPatch uses an enabled/ready protocol.
sinkReadyPatch :: forall a c sig . (Num a, Rep a, Clock c, sig ~ CSeq c)
    => Patch    (sig (Enabled a))           ()
                (sig Ready)                 ()
sinkReadyPatch ~(_, ()) = (toReady ready, ())
  where
        ready = high
-- | A sink patch throws away its data input (generating a () data
-- output). sinkReadyPatch uses an enabled/ack protocol.
sinkAckPatch :: forall a c sig . (Num a, Rep a, Clock c, sig ~ CSeq c)
    => Patch    (sig (Enabled a))           ()
                (sig Ack)                   ()
sinkAckPatch ~(inp, ()) = (toAck ack, ())
  where
        (ack,_) = unpack inp

-------------------------------------------------------------------------------
-- Source Patches - generate a stream of constant values
-------------------------------------------------------------------------------

-- | A source patch takes no input and generates a stream of values. It
-- corresponds to a top-level input port. sourceReadyPatch uses the
-- ready/enabled protocol.
sourceReadyPatch :: forall a c sig . (Num a, Rep a, Clock c, sig ~ CSeq c)
    => a
    -> Patch    ()           (sig (Enabled a))
                ()           (sig Ready)
sourceReadyPatch baseVal ~((), ready_in) = ((), out)
  where
        out = packEnabled (fromReady ready_in) (pureS baseVal)

-- | A source patch takes no input and generates a stream of values. It
-- corresponds to a top-level input port. sourceReadyPatch uses the enabled/ack
-- protocol.
sourceAckPatch :: forall a c sig . (Num a, Rep a, Clock c, sig ~ CSeq c)
    => a
    -> Patch    ()           (sig (Enabled a))
                ()           (sig Ack)
sourceAckPatch baseVal ~((), _) = ((), out)
  where
        out = packEnabled high (pureS baseVal)

------------------------------------------------
-- Unit
------------------------------------------------

-- | An instance of the Unit type contains a value that carries no information.
class Unit unit where
  -- | The name of the specific value.
  unit :: unit

instance Unit () where unit = ()
instance (Unit a,Unit b) => Unit (a,b) where unit = (unit,unit)
instance (Unit a,Unit b) => Unit (a :> b) where unit = (unit :> unit)
instance (Unit a,Size x) => Unit (Matrix x a) where unit = pure unit

------------------------------------------------
-- File I/O for Patches
------------------------------------------------

-- | 'rawReadPatch' reads a binary file into Patch, which will become the
-- lefthand side of a chain of patches.
rawReadPatch :: FilePath -> IO (Patch ()			[Maybe U8]
			              ()			())
rawReadPatch fileName = do
     fileContents <- B.readFile fileName
     return $ unitPatch $ map (Just . fromIntegral) $ B.unpack fileContents

-- | 'readPatch' reads an encoded file into Patch, which will become the
-- lefthand side of a chain of patches.
readPatch :: (Read a)
		  => FilePath -> IO (Patch ()	[Maybe a]
	  		                   ()	())
readPatch fileName = do
     fileContents <- readFile fileName
     return $ unitPatch $ map (Just . read) $ words $ fileContents

-- | 'rawWritePatch' runs a complete circuit for the given
-- number of cycles, writing the result to a given file in binary format.
rawWritePatch :: (Unit u1, Unit u2)
           => FilePath
	   -> Int
	   -> Patch u1 		[Maybe U8]
	 	    u2		()
	   -> IO ()
rawWritePatch fileName n patch = do
  B.writeFile fileName $ B.pack [ fromIntegral x  | Just x <- take n $ runPatch patch ]

-- | 'writePatch' runs a complete circuit for the given
-- number of cycles, writing the result to a given file in string format.
writePatch :: (Show a, Unit u1, Unit u2)
	   => FilePath
	   -> Int
	   -> Patch u1 		[Maybe a]
	 	    u2	 	()
	   -> IO ()
writePatch fileName n patch = do
  writeFile fileName $ unlines [ show x | Just x <- take n $ runPatch patch ]

-- | 'comparePatch' compares the input to the contents of a file, if there
-- there a mismatch, it will print a message giving the element number that
-- failed.  If everything matches, it prints out a "Passed" message
comparePatch :: (Read a, Show a, Eq a, Unit u1, Unit u2)
             => FilePath
             -> Int
             -> Patch u1     [Maybe a]
                      u2     ()
             -> IO (Patch ()     [Maybe a]
                          ()     () )
comparePatch fileName n patch = do
     fileContents <- readFile fileName
     let expectedVals = map read $ words $ fileContents
     listComparePatch expectedVals n patch

-- | 'listComparePatch' compares the input to the contents of a list, if
-- there is a mismatch, it will print a message giving the element number
-- that failed.  If everything matches, it prints out a "Passed" message
listComparePatch :: (Read a, Show a, Eq a, Unit u1, Unit u2)
             => [a]
             -> Int
             -> Patch u1     [Maybe a]
                      u2     ()
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


infixr 5 `bus`

-- | bus is a synonym for ($$). TODO: Remove this alias.
bus  ::
  	 Patch li1 	o
	       lo1  	i
      -> Patch o 	ro2
	       i 	ri2
      -> Patch li1	ro2
	       lo1 	ri2
bus = ($$)

-- | ($$) composes two patches serially, sharing a common control protocol. The
-- data output of the first patch is fed to the data input of the second
-- patch. The control output of the second patch is fed to the control input of
-- the first patch, and the control output of the first patch is fed to the
-- control input of the second patch.
infixr 5 $$
($$)  ::
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

-- | probePatch creates a patch with a named probe, probing both data and control
-- outputs.
probePatch :: (Probe a, Probe b)
   => String
   -> Patch    a   a
               b   b
probePatch probeName ~(inp1, inp2) = (out2, out1)
   where
       (out1, out2) = id
                    $ probe probeName
                    $ (inp1, inp2)

-- | probeDataPatch creates a patch with a named probe, probing the data input.
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

-- | probeDataPatch creates a patch with a named probe, probing the control input.
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
dupPatch :: forall c sig a . (Clock c, sig ~ CSeq c, Rep a)
         => Patch (sig (Enabled a))     (sig (Enabled a)  :> sig (Enabled a))
	          (sig Ack)             (sig Ack          :> sig Ack)
dupPatch =
	matrixDupPatch $$
	forwardPatch (\ m -> (m M.! 0 :> m M.! 1)) $$
	backwardPatch (\ ~(a :> b) -> matrix [a,b] :: Matrix X2 (sig Ack))

-- | This duplicate the incoming datam over many handshaken streams.
matrixDupPatch :: (Clock c, sig ~ CSeq c, Rep a, Size x)
         => Patch (sig (Enabled a))     (Matrix x (sig (Enabled a)))
	          (sig Ack)             (Matrix x (sig Ack))
matrixDupPatch = ackToReadyBridge $$ matrixDupPatch' $$ matrixStack (pure readyToAckBridge) where
 matrixDupPatch' ~(inp,readys) = (toReady go, pure out)
    where
	go = foldr1 (.&&.) $ map fromReady $ M.toList readys
	out = packEnabled (go .&&. isEnabled inp) (enabledVal inp)

-- | unzipPatch creates a patch that takes in an Enabled data pair, and produces a
-- pair of Enabled data outputs.
unzipPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep b)
         => Patch (sig (Enabled (a,b)))     (sig (Enabled a) :> sig (Enabled b))
	          (sig Ack)                 (sig Ack       :> sig Ack)
unzipPatch = dupPatch $$
		stack (forwardPatch $ mapEnabled (fst . unpack))
		      (forwardPatch $ mapEnabled (snd . unpack))

-- | matrixUnzipPatch is the generalization of unzipPatch to homogeneous matrices.
matrixUnzipPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x)
         => Patch (sig (Enabled (Matrix x a)))    (Matrix x (sig (Enabled a)))
	          (sig Ack)          		  (Matrix x (sig Ack))
matrixUnzipPatch =
	matrixDupPatch $$
	matrixStack (forAll $ \ x ->  forwardPatch (mapEnabled $ \ v -> v .!. pureS x))

-- | TODO: Andy write docs for this.
deMuxPatch :: forall c sig a . (Clock c, sig ~ CSeq c, Rep a)
  => Patch (sig (Enabled Bool)    :> sig (Enabled a)) 		  (sig (Enabled a) :> sig (Enabled a))
	   (sig Ack               :> sig Ack)		          (sig Ack	   :> sig Ack)
deMuxPatch = fe $$ matrixDeMuxPatch $$ be
  where
	fe = fstPatch (forwardPatch ((unsigned)))
	be = backwardPatch (\ ~(b :> c) -> matrix [c,b]) `bus`
	     forwardPatch (\ m -> ((m M.! (1 :: X2)) :> (m M.! 0)))

-- | matrixDeMuxPatch is the generalization of deMuxPatch to a matrix of signals.
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

-- | Combine two enabled data inputs into a single Enabled tupled data input.
zipPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep b)
  => Patch (sig (Enabled a)  :> sig (Enabled b))	(sig (Enabled (a,b)))
	   (sig Ack          :> sig Ack)	  	(sig Ack)
zipPatch ~(in1 :> in2, outReady) = (toAck ack :> toAck ack, out)
    where
	try = isEnabled in1 .&&. isEnabled in2
	ack = try .&&. fromAck outReady

	out = packEnabled try (pack (enabledVal in1, enabledVal in2))

-- | Extension of zipPatch to homogeneous matrices.
matrixZipPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x)
         => Patch (Matrix x (sig (Enabled a)))	(sig (Enabled (Matrix x a)))
	          (Matrix x (sig Ack))		(sig Ack)
matrixZipPatch ~(mIn, outReady) = (mAcks, out)
   where
	try    = foldr1 (.&&.) (map isEnabled $ M.toList mIn)

	mAcks = fmap toAck $ pure (try .&&. fromAck outReady)
	mIn'  = fmap enabledVal mIn
	out   = packEnabled try (pack mIn' :: sig (Matrix x a))

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
matrixMuxPatch  ~(~(cond :> m),ack) = ((toAck ackCond :> fmap toAck m_acks),out)
   where
	-- set when conditional value on cond port
 	try = isEnabled cond

	-- only respond/ack when you are ready to go, the correct lane, and have input
	gos :: Matrix x (sig Bool)
	gos = forEach m $ \ x inp -> try
				.&&. (enabledVal cond .==. pureS x)
				.&&. isEnabled inp

	ackCond = foldr1 (.||.) $ M.toList m_acks
	m_acks = fmap (\ g -> g .&&. fromAck ack) gos

	out = cASE (zip (M.toList gos) (M.toList m))	-- only one can ever be true
		   disabledS

---------------------------------------------------------------------------------
-- Trivial FIFOs
---------------------------------------------------------------------------------

-- (There are larger FIFO's in the Kansas Lava Cores package.)

-- | FIFO with depth 1.
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

-- | FIFO with depth 2.
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


---------------------------------------------------------------------------------
-- Retiming
---------------------------------------------------------------------------------

matrixExpandPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x, Num x, Enum x)
         => Patch (sig (Enabled (Matrix x a)))	(sig (Enabled a))
	          (sig Ack)			(sig Ack)
matrixExpandPatch =
	   openPatch
	$$ stack 
		 (cyclePatch (coord :: Matrix x x))
		 (matrixUnzipPatch)
	$$ matrixMuxPatch

matrixContractPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x, Num x, Enum x)
         => Patch (sig (Enabled a)) (sig (Enabled (Matrix x a)))
	          (sig Ack)	    (sig Ack)
matrixContractPatch =
	   openPatch 
	$$ fstPatch (cyclePatch (coord :: Matrix x x))
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
	=> Matrix ix a
	-> Patch ()		(sig (Enabled a))
	         ()		(sig Ack)
cyclePatch m ~(_,ack) = ((),out)
  where
	ix :: sig ix
	ix = register 0
	   $ cASE [ (fromAck ack, loopingInc ix) ]
		  ix

	out = packEnabled high
            $ funMap (\ x -> return (m M.! x))
		     ix

-- appendPatch appends constant list (matrix) before
-- a stream of handshaken values.

appendPatch :: forall a c ix sig .
        ( Size ix
        , Rep a
        , Rep ix
        , Num ix
        , Clock c
	, sig ~ CSeq c
        )
	=> Matrix ix a
	-> Patch (sig (Enabled a))	(sig (Enabled a))
	         (sig Ack)		(sig Ack)
appendPatch m ~(inp,ackOut) = (ackIn,out)
  where
	ix :: sig ix
	ix = register 0
	   $ cASE [ (fromAck ackOut, loopingInc ix) ]
		  ix

	st :: sig Bool
	st = register False
	   $ cASE [ (fromAck ackOut .&&. ix .==. (maxBound :: sig ix), high) ]
		  st

	ackIn :: sig Ack
	ackIn = mux2 st
		( ackOut
		, toAck low -- do not acccept anything until header has been sent
		)
	out :: sig (Enabled a)
	out = mux2 st 
		( inp
		, packEnabled high $ funMap (\ x -> return (m M.! x)) ix
		)
