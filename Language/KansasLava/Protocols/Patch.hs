{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies, GADTs #-}
{-# LANGUAGE ParallelListComp, TypeOperators #-}
module Language.KansasLava.Protocols.Patch
  -- (Patch, bus, ($$),
  -- emptyP,forwardP, backwardP,dupP,zipPatch,
  -- fstP,matrixDupP,matrixStackP,matrixZipPatch,
  -- fifo1, fifo2)
  where

import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Types

import Language.KansasLava.Rep
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Signal
--import Language.KansasLava.Probes

import Data.Sized.Unsigned (U8)
import Data.Sized.Matrix as M

import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Control.Monad.Fix

---------------------------------------------------------------------------
-- The Patch.
---------------------------------------------------------------------------

-- | A Patch is a data signal with an associated control signal. The 'lhs_in'
-- type parameter is the type of the data input, the 'rhs_out' type parameter is
-- the type of the data output. The 'rhs_in' is the type of the control input
-- (e.g. a 'ready' signal), and the 'lhs_out' is the type of the control output
-- (e.g. 'ack').
type Patch lhs_in 	   rhs_out
	   lhs_out         rhs_in
	= (lhs_in,rhs_in) -> (lhs_out,rhs_out)

---------------------------------------------------------------------------
-- Executing the Patch, monadic style.
---------------------------------------------------------------------------

-- | outputP produces a constant data output. The control inputs/outputs are
-- unit, so they contain no data.
outputP :: a -> Patch () a
		        () ()
outputP a = \ _ -> ((),a)


runP :: (Unit u1, Unit u2)
 	 => Patch u1   a
	 	  u2  () -> a
runP p = a
 where
   (_,a) = p (unit,unit)

execP :: Patch a b
 		   c d
 	  -> (a,d) -> (c,b)
execP = id


-- | A patch that passes through data and control.
emptyP :: Patch a  a
	         b  b
emptyP ~(a,b) = (b,a)

-- | Given a patch, add to the data and control inputs/outputs a second set of
-- signals that are passed-through. The signals of the argument patch to fstP
-- will appear as the first element of the pair in the resulting patch.
fstP :: Patch a   b
		  c   e -> Patch (a :> f) (b :> f)
				 (c :> g) (e :> g)
fstP p = p `stackP` emptyP

-- | Given a patch, add to the data and control inputs/outputs a second set of
-- signals that are passed-through. The signals of the argument patch to sndP
-- will appear as the second element of the pair in the resulting patch.
sndP :: Patch a   b
		  c   d -> Patch (f :> a) (f :> b)
				 (g :> c) (g :> d)
sndP p = emptyP `stackP` p

{-
-- | 'matrixSplicePatch' splices/inserts a patch into a matrix of signals at the position
-- given by index.  (Note:  The first position is index 0)
matrixSplicePatch :: (Size x, Integral x)
            => x
            -> Patch a   a
                     b   b -> Patch (Matrix x a) (Matrix x a)
                                    (Matrix x b) (Matrix x b)
matrixSplicePatch index p ~(mlhs_in, mrhs_in) = (mlhs_out, mrhs_out)
  where
    lhs_in = mlhs_in M.! index
    rhs_in = mrhs_in M.! index

    (lhs_out, rhs_out) = p (lhs_in, rhs_in)

    indexInt = (fromIntegral index) :: Int
    mlhs_out = M.matrix $ (take indexInt $ M.toList mrhs_in) ++ lhs_out:(drop (indexInt+1) $ M.toList mrhs_in)
    mrhs_out = M.matrix $ (take indexInt $ M.toList mlhs_in) ++ rhs_out:(drop (indexInt+1) $ M.toList mlhs_in)
-}

-- | Lift a function to a patch, applying the function to the data input.
forwardP :: (li -> ro)
	    -> Patch li ro
	             b  b
forwardP f1 ~(li,ri) = (ri,f1 li)

-- | Lift a function to a patch, applying the function to the control input.
backwardP :: (ri -> lo)
	    -> Patch a  a
	             lo ri
backwardP f2 ~(li,ri) = (f2 ri,li)

-- TODO: above or ??

infixr 3 `stackP`
-- | Given two patches, tuple their data/control inputs and outputs.
stackP :: Patch li1		ro1
               lo1    		ri1
      -> Patch li2		ro2
               lo2    		ri2
      -> Patch (li1 :> li2)			(ro1 :> ro2)
               (lo1 :> lo2)   			(ri1 :> ri2)
stackP p1 p2 inp = (lo1 :> lo2,ro1 :> ro2)
   where
	(li1 :> li2,ri1 :> ri2)	     = inp
	(lo1,ro1)		     = p1 (li1,ri1)
	(lo2,ro2)		     = p2 (li2,ri2)

-- | Given a homogeneous list (Matrix) of patches, combine them into a single
-- patch, collecting the data/control inputs/outputs into matrices.
matrixStackP :: (m ~ (Matrix x), Size x)
 	=> m (Patch li   ro
	   	    lo   ri)
	-> Patch (m li)         (m ro)
		 (m lo)         (m ri)
matrixStackP m inp = ( fmap (\ (l,_) -> l) m'
		    , fmap (\ (_,r) -> r) m'
		    )
   where
	(m_li,m_ri)	= inp
	m' = (\ p li ri -> p (li,ri)) <$> m <*> m_li <*> m_ri


-- | loopP is a fixpoint style combinator, for backedges.
loopP :: Patch (a :> b) (a :> c)
		   (d :> e) (d :> f)
	  -> Patch b c
		   e f
loopP g ~(b,f) = (e,c)
  where
    (d:>e,a:>c) = g (a:>b,d:>f)

openP :: Patch c (() :> c)
	           d (() :> d)
openP = forwardP (\ a -> (() :> a)) $$
	    backwardP (\ ~(_ :> a) -> a)
{-
swapP :: (a :> b) -> (b :> a)
assocP :: ((a :> b) :> c) -> (a :> b :> c)
unassocP :: (a :> b :> c) -> ((a :> b) :> c)
closeP :: (() :> b) -> b
copyP  :: a -> (a :> a) -- is DUP
-}

-------------------------------------------------------------------------------
-- maping a combinatorial circuit over a protocol.
-------------------------------------------------------------------------------

mapP :: forall a b c sig ack . (Rep a, Rep b, Clock c, sig ~ Signal c)
	 => (forall clk' . Signal clk' a -> Signal clk' b)
	 -> Patch (sig (Enabled a)) (sig (Enabled b))
	   	  (ack)		    (ack)
mapP = forwardP . mapEnabled

------------------------------------------------
-- Unit
------------------------------------------------

-- | An instance of the Unit type contains a value that carries no information.
class Unit unit where
  -- | The name of the specific value.
  unit :: unit


unUnit :: (Unit unit) => unit -> ()
unUnit _ = ()

instance Unit () where unit = ()
instance (Unit a,Unit b) => Unit (a,b) where unit = (unit,unit)
instance (Unit a,Unit b) => Unit (a :> b) where unit = (unit :> unit)
instance (Unit a,Size x) => Unit (Matrix x a) where unit = pure unit

------------------------------------------------
-- File I/O for Patches
------------------------------------------------

-- | 'rawReadP' reads a binary file into Patch, which will become the
-- lefthand side of a chain of patches.
rawReadP :: FilePath -> IO (Patch ()			[Maybe U8]
			          ()			())
rawReadP fileName = do
     fileContents <- B.readFile fileName
     return $ outputP $ map (Just . fromIntegral) $ B.unpack fileContents

-- | 'readPatch' reads an encoded file into Patch, which will become the
-- lefthand side of a chain of patches.
readP :: (Read a)
	  => FilePath -> IO (Patch ()	[Maybe a]
	                           ()	())
readP fileName = do
     fileContents <- readFile fileName
     return $ outputP $ map (Just . read) $ words $ fileContents

-- | 'rawWriteP' runs a complete circuit for the given
-- number of cycles, writing the result to a given file in binary format.
rawWriteP :: (Unit u1, Unit u2)
           => FilePath
	   -> Int
	   -> Patch u1 		[Maybe U8]
	 	    u2		()
	   -> IO ()
rawWriteP fileName n patch = do
  B.writeFile fileName $ B.pack [ fromIntegral x  | Just x <- take n $ runP patch ]

-- | 'writeP' runs a complete circuit for the given
-- number of cycles, writing the result to a given file in string format.
writeP :: (Show a, Unit u1, Unit u2)
	   => FilePath
	   -> Int
	   -> Patch u1 		[Maybe a]
	 	    u2	 	()
	   -> IO ()
writeP fileName n patch = do
  writeFile fileName $ unlines [ show x | Just x <- take n $ runP patch ]

{-
-- | 'compareP' compares the input to the contents of a file, if there
-- there a mismatch, it will print a message giving the element number that
-- failed.  If everything matches, it prints out a "Passed" message
compareP :: (Read a, Show a, Eq a, Unit u1, Unit u2)
             => FilePath
             -> Int
             -> Patch u1     [Maybe a]
                      u2     ()
             -> IO (Patch ()     [Maybe a]
                          ()     () )
compareP fileName n patch = do
     fileContents <- readFile fileName
     let expectedVals = map read $ words $ fileContents
     listCompareP expectedVals n patch

-- | 'listCompareP' compares the input to the contents of a list, if
-- there is a mismatch, it will print a message giving the element number
-- that failed.  If everything matches, it prints out a "Passed" message
listCompareP :: (Read a, Show a, Eq a, Unit u1, Unit u2)
             => [a]
             -> Int
             -> Patch u1     [Maybe a]
                      u2     ()
             -> IO (Patch ()     [Maybe a]
                          ()     () )
listCompareP expectedVals n patch = do
     let inp = runP patch
     let incomingVals = [ x | Just x <- take n $ inp ]
     putStr $ checkAgainstList 0 expectedVals incomingVals
     return $ outputP inp
  where
     checkAgainstList :: (Show a, Eq a) => Int -> [a] -> [a] -> String
     checkAgainstList elemNum []      _                = "Passed, quantity compared:  " ++ (show elemNum)
     checkAgainstList elemNum _       []               = "Passed, but some data in file was not reached, quantity compared:  " ++ (show elemNum)
     checkAgainstList elemNum (e:exs) (i:ins) | (e==i) = checkAgainstList (elemNum+1) exs ins
     checkAgainstList elemNum (e:_)   (i:_)            = "Failed:  Element " ++ (show (elemNum+1)) ++ ": \tExpected:  " ++ (show e) ++ " \tActual:  " ++ (show i)
-}

---------------------------------------------------------------------------
-- Functions that connect streams
---------------------------------------------------------------------------

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
	(lhs_in,rhs_in)     = inp
	(lhs_out1,rhs_out1) = p1 (lhs_in,lhs_out2)
	(lhs_out2,rhs_out2) = p2 (rhs_out1,rhs_in)

-- | 'readyToAckBridge' converts from a ready interface to an ACK interface
-- by preemptively giving the ready signal, and holding the resulting data
-- from the device on the input side if no ACK is received by the device on
-- the output side.  If data is currently being held, then the ready signal
-- will not be given.  This bridge is fine for deep embedding (can be
-- represented in hardware).
readyToAckBridge :: forall a c sig . (Rep a, Clock c, sig ~ Signal c)
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
ackToReadyBridge :: (Rep a, Clock c, sig ~ Signal c)
    => Patch    (sig (Enabled a))           (sig (Enabled a))
                (sig Ack)                 (sig Ready)
ackToReadyBridge ~(inp, ready_in) = (toAck ack, out)
    where
        out = inp
        ack = (fromReady ready_in) .&&. (isEnabled inp)

---------------------------------------------------------------------------------
-- Functions that fork streams.
---------------------------------------------------------------------------------

-- | This duplicates the incomming datum.
-- This has the behavior that neither branch sees the value
-- until both can recieve it.

dupP :: forall c sig a . (Clock c, sig ~ Signal c, Rep a)
         => Patch (sig (Enabled a))     (sig (Enabled a)  :> sig (Enabled a))
	          (sig Ack)             (sig Ack          :> sig Ack)

dupP ~(inp,ack1 :> ack2) = (toAck have_read,out1 :> out2)
  where
	have_read = (state .==. 0) .&&. isEnabled inp
	written1  = (state ./=. 0) .&&. fromAck ack1
	written2  = (state ./=. 0) .&&. fromAck ack2

	state :: sig X4
	state = register 0
	      $ cASE [ (have_read,	                pureS 1)
		     , (written1 .&&. written2,	        pureS 0)
		     , (written1 .&&. state .==. 1,	pureS 3)
		     , (written2 .&&. state .==. 1,	pureS 2)
		     , (written1 .&&. state .==. 2,	pureS 0)
		     , (written2 .&&. state .==. 3,	pureS 0)
		     ] state

	store :: sig a
	store = cASE [ (have_read,enabledVal inp)
		     ]
	      $ delay store

	out1 = packEnabled (state .==. 1 .||. state .==. 2) store
	out2 = packEnabled (state .==. 1 .||. state .==. 3) store

-- | This duplicate the incoming datam over many handshaken streams.
matrixDupP :: (Clock c, sig ~ Signal c, Rep a, Size x)
         => Patch (sig (Enabled a))     (Matrix x (sig (Enabled a)))
	          (sig Ack)             (Matrix x (sig Ack))
matrixDupP = ackToReadyBridge $$ matrixDupP' $$ matrixStackP (pure readyToAckBridge) where
 matrixDupP' ~(inp,readys) = (toReady go, pure out)
    where
	go = foldr1 (.&&.) $ map fromReady $ M.toList readys
	out = packEnabled (go .&&. isEnabled inp) (enabledVal inp)

-- | unzipP creates a patch that takes in an Enabled data pair, and produces a
-- pair of Enabled data outputs.
unzipP :: (Clock c, sig ~ Signal c, Rep a, Rep b)
         => Patch (sig (Enabled (a,b)))     (sig (Enabled a) :> sig (Enabled b))
	          (sig Ack)                 (sig Ack       :> sig Ack)
unzipP = dupP $$
		stackP (forwardP $ mapEnabled (fst . unpack))
		      (forwardP $ mapEnabled (snd . unpack))

-- | matrixUnzipP is the generalization of unzipP to homogeneous matrices.
matrixUnzipP :: (Clock c, sig ~ Signal c, Rep a, Rep x, Size x)
         => Patch (sig (Enabled (Matrix x a)))    (Matrix x (sig (Enabled a)))
	          (sig Ack)          		  (Matrix x (sig Ack))
matrixUnzipP =
	matrixDupP $$
	matrixStackP (forAll $ \ x ->  forwardP (mapEnabled $ \ v -> v .!. pureS x))

-- | TODO: Andy write docs for this.
deMuxP :: forall c sig a . (Clock c, sig ~ Signal c, Rep a)
  => Patch (sig (Enabled Bool)    :> sig (Enabled a)) 		  (sig (Enabled a) :> sig (Enabled a))
	   (sig Ack               :> sig Ack)		          (sig Ack	   :> sig Ack)
deMuxP = fe $$ matrixDeMuxP $$ be
  where
--	fe = fstP (forwardP ((unsigned)))
	fe = fstP (mapP (unsigned))
	be = backwardP (\ ~(b :> c) -> matrix [c,b]) $$
	     forwardP (\ m -> ((m M.! (1 :: X2)) :> (m M.! 0)))

-- | matrixDeMuxP is the generalization of deMuxP to a matrix of signals.
matrixDeMuxP :: forall c sig a x . (Clock c, sig ~ Signal c, Rep a, Rep x, Size x)
  => Patch (sig (Enabled x)    :> sig (Enabled a)) 		  (Matrix x (sig (Enabled a)))
	   (sig Ack            :> sig Ack)		          (Matrix x (sig Ack))
matrixDeMuxP = matrixDeMuxP' $$ matrixStackP (pure readyToAckBridge) where
 matrixDeMuxP' ~(ix :> inp, m_ready) = (toAck ackCond :> toAck ackIn,out)
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

-- no unDup (requires some function / operation, use zipP).

-- | Combine two enabled data inputs into a single Enabled tupled data input.
zipP :: (Clock c, sig ~ Signal c, Rep a, Rep b)
  => Patch (sig (Enabled a)  :> sig (Enabled b))	(sig (Enabled (a,b)))
	   (sig Ack          :> sig Ack)	  	(sig Ack)
zipP ~(in1 :> in2, outReady) = (toAck ack :> toAck ack, out)
    where
	try = isEnabled in1 .&&. isEnabled in2
	ack = try .&&. fromAck outReady

	out = packEnabled try (pack (enabledVal in1, enabledVal in2))

-- | Extension of zipP to homogeneous matrices.
matrixZipP :: forall c sig a x . (Clock c, sig ~ Signal c, Rep a, Rep x, Size x)
         => Patch (Matrix x (sig (Enabled a)))	(sig (Enabled (Matrix x a)))
	          (Matrix x (sig Ack))		(sig Ack)
matrixZipP ~(mIn, outReady) = (mAcks, out)
   where
	try    = foldr1 (.&&.) (map isEnabled $ M.toList mIn)

	mAcks = fmap toAck $ pure (try .&&. fromAck outReady)
	mIn'  = fmap enabledVal mIn
	out   = packEnabled try (pack mIn' :: sig (Matrix x a))

-- | 'muxP' chooses a the 2nd or 3rd value, based on the Boolean value.
muxP :: (Clock c, sig ~ Signal c, Rep a)
  => Patch (sig (Enabled Bool) :> sig (Enabled a)  :> sig (Enabled a))	(sig (Enabled a))
	   (sig Ack            :> sig Ack          :> sig Ack)	  	(sig Ack)

muxP = fe $$ matrixMuxP
   where
	fe = forwardP (\ ~(a :> b :> c) -> (mapEnabled (unsigned) a :> matrix [c,b])) $$
	     backwardP (\ ~(a :> m) -> (a :> (m M.! (1 :: X2)) :> (m M.! 0)))


-- | 'matrixMuxP' chooses the n-th value, based on the index value.
matrixMuxP :: forall c sig a x . (Clock c, sig ~ Signal c, Rep a, Rep x, Size x)
  => Patch (sig (Enabled x)    :> Matrix x (sig (Enabled a)))		(sig (Enabled a))
	   (sig Ack            :> Matrix x (sig Ack))		  	(sig Ack)
matrixMuxP  ~(~(cond :> m),ack) = ((toAck ackCond :> fmap toAck m_acks),out)
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
fifo1 :: forall c sig a . (Clock c, sig ~ Signal c, Rep a)
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
	store = delay
	      $ cASE [ (have_read,enabledVal inp)
		     ]
	       store

	out = packEnabled (state .==. 1) store

-- | FIFO with depth 2.
fifo2 :: forall c sig a . (Clock c, sig ~ Signal c, Rep a)
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

-- | 'matrixToElementsP' turns a matrix into a sequences of elements from the array, in ascending order.
matrixToElementsP :: forall c sig a x . (Clock c, sig ~ Signal c, Rep a, Rep x, Size x, Num x, Enum x)
         => Patch (sig (Enabled (Matrix x a)))	(sig (Enabled a))
	          (sig Ack)			(sig Ack)
matrixToElementsP =
	   openP
	$$ stackP
		 (cycleP (coord :: Matrix x x))
		 (matrixUnzipP)
	$$ matrixMuxP

-- | 'matrixFromElementsP' turns a sequence of elements (in ascending order) into a matrix.
-- ascending order.
matrixFromElementsP :: forall c sig a x . (Clock c, sig ~ Signal c, Rep a, Rep x, Size x, Num x, Enum x)
         => Patch (sig (Enabled a)) (sig (Enabled (Matrix x a)))
	          (sig Ack)	    (sig Ack)
matrixFromElementsP =
	   openP
	$$ fstP (cycleP (coord :: Matrix x x))
	$$ matrixDeMuxP
	$$ matrixZipP

---------------------------------------------------------------------------------
-- Other stuff
---------------------------------------------------------------------------------

-- | globalClockP forces the handshaking to use the CLK clock. Which is useful for testing.
globalClockP :: (clk ~ CLK, sig ~ Signal clk) =>
	Patch (sig a)		(sig a)
	      (sig b)           (sig b)
globalClockP ~(li,ri) = (ri,li)

-- | cycleP cycles through a constant list (actually a matrix) of values.
-- Generates an async ROM on hardware.
cycleP :: forall a c ix sig .
        ( Size ix
        , Rep a
        , Rep ix
        , Num ix
        , Clock c
	, sig ~ Signal c
        )
	=> Matrix ix a
	-> Patch ()		(sig (Enabled a))
	         ()		(sig Ack)
cycleP m ~(_,ack) = ((),out)
  where
	ix :: sig ix
	ix = register 0
	   $ cASE [ (fromAck ack, loopingIncS ix) ]
		  ix

	out = packEnabled high
            $ funMap (\ x -> return (m M.! x))
		     ix

constP :: forall a c ix sig .
        ( Size ix
        , Rep a
        , Rep ix
        , Num ix
        , Clock c
	, sig ~ Signal c
        )
	=> Matrix ix a
	-> Patch ()	(sig (Enabled a))
	         ()	(sig Ack)
constP m ~(_,ackOut) = ((),out)
  where
	ix :: sig ix
	ix = register 0
	   $ cASE [ (fromAck ackOut, loopingIncS ix) ]
		  ix

	st :: sig Bool
	st = register False
	   $ cASE [ (fromAck ackOut .&&. ix .==. (maxBound :: sig ix), high) ]
		  st

	out :: sig (Enabled a)
	out = mux st
		(packEnabled high $ funMap (\ x -> return (m M.! x)) ix
                , disabledS
		)


-- prependP appends constant list (matrix) before
-- a stream of handshaken values.

prependP :: forall a c ix sig .
        ( Size ix
        , Rep a
        , Rep ix
        , Num ix
        , Clock c
	, sig ~ Signal c
        )
	=> Matrix ix a
	-> Patch (sig (Enabled a))	(sig (Enabled a))
	         (sig Ack)		(sig Ack)
prependP m ~(inp,ackOut) = (ackIn,out)
  where
	ix :: sig ix
	ix = register 0
	   $ cASE [ (fromAck ackOut, loopingIncS ix) ]
		  ix

	st :: sig Bool
	st = register False
	   $ cASE [ (fromAck ackOut .&&. ix .==. (maxBound :: sig ix), high) ]
		  st

	ackIn :: sig Ack
	ackIn = mux st
		( toAck low -- do not acccept anything until header has been sent
		, ackOut
		)
	out :: sig (Enabled a)
	out = mux st
		( packEnabled high $ funMap (\ x -> return (m M.! x)) ix
		, inp
		)

---------------------------------------------------
-- These are patch order swappers : re-wiring only
{- To be finished

exp2Stack :: Patch ((a :> b) :> c)	(a :> b :> c)
	           ((d :> e) :> f)	(d :> e :> f)
exp2Stack = forwardP (\ ((a :> b) :>  c) -> (a :> b :> c)) $$
	    backwardP (\ (a :> b :> c) -> ((a :> b) :> c))

con2Stack :: Patch (a :> b :> c)	((a :> b) :> c)
	           (d :> e :> f)	((d :> e) :> f)
con2Stack = forwardP (\ (a :> b :> c) -> ((a :> b) :> c)) $$
	    backwardP (\ ((a :> b) :>  c) -> (a :> b :> c))

swapPatch :: Patch (a :> b)	(b :> a)
	           (c :> d)	(d :> c)
swapPatch = forwardP (\ (a :> b) -> (b :> a)) $$
	    backwardP (\ (b :> a) -> (a :> b))
-}
----------------------------------------------------


data MergePlan = PriorityMerge		-- ^ The first element always has priority
	       | RoundRobinMerge	-- ^ Turn about, can be slower

mergeP :: forall c sig a . (Clock c, sig ~ Signal c, Rep a)
 => MergePlan
 -> Patch ((sig (Enabled a)) :> (sig (Enabled a)))    (sig (Enabled a))
	   ((sig Ack)         :> (sig Ack))            (sig Ack)

mergeP plan = fe $$ matrixMergeP plan
  where
	fe = forwardP (\ ~(b :> c) -> (matrix [b,c])) $$
	     backwardP (\ ~m -> ( (m M.! (0 :: X2)) :> (m M.! (1 :: X2))))


matrixMergeP :: forall c sig a x . (Clock c, sig ~ Signal c, Rep a, Rep x, Size x, Num x, Enum x)
  => MergePlan
  -> Patch (Matrix x (sig (Enabled a)))		(sig (Enabled a))
	   (Matrix x (sig Ack))		  	(sig Ack)
matrixMergeP plan ~(mInp, ackOut) = (mAckInp, out)
 where
   isEs :: sig (Matrix x Bool)
   isEs = pack (fmap isEnabled mInp)

   -- Value to consider selecting.
   inpIndex :: sig x
   inpIndex = case plan of
		PriorityMerge   -> cASE (zip (map isEnabled $ M.toList mInp) (map pureS [0..])) (pureS 0)
		RoundRobinMerge -> let reg = register 0 (mux ((isEs .!. reg) .&&. bitNot (fromAck ackOut))
								-- stop looking if found enable
								-- an no ack
							     (loopingIncS reg,reg)) in reg

   mAckInp = forEach mInp $ \ x _inp -> toAck $ ((pureS x) .==. inpIndex) .&&. (fromAck ackOut)
   out = (pack mInp) .!. inpIndex

---------------------------------------------------------------------------
-- FabricPatch, the IO version of Patch
---------------------------------------------------------------------------

type FabricPatch fab
           lhs_in 	   rhs_out
	   lhs_out         rhs_in
	= (lhs_in,rhs_in) -> fab (lhs_out,rhs_out)

patchF :: (MonadFix fab)
            => Patch a b
                     c d -> FabricPatch fab a b
                                            c d
patchF patch inp = return (patch inp)

infixr 4 |$|
(|$|) :: (MonadFix fab)
      => FabricPatch fab a b
                         d e
      -> FabricPatch fab b c
                         e f
      -> FabricPatch fab a c
                         d f
f1 |$| f2 = \ ~(lhs_in,rhs_in) -> do
	rec ~(lhs_out1,rhs_out1) <- f1 (lhs_in,lhs_out2)
	    ~(lhs_out2,rhs_out2) <- f2 (rhs_out1,rhs_in)
        return (lhs_out1,rhs_out2)

runF :: (MonadFix fab)
 	 => FabricPatch fab  ()   a
	 	             ()   () -> fab a
runF p = do
   ~(_,a) <- p ((),())
   return a

buildF :: (MonadFix fab)
       => ((a,d) -> fab (c,b))
       -> FabricPatch fab a b
                          c d
buildF = id


-- | A fabric patch that passes through data and control.
emptyF :: (MonadFix fab)
       => FabricPatch
                fab a  a
	            b  b
emptyF = patchF emptyP

infixr 3 `stackF`
-- | Given two fabric patches, tuple their data/control inputs and outputs.
stackF :: (MonadFix fab)
       => FabricPatch fab
               li1		ro1
               lo1    		ri1
      -> FabricPatch fab
               li2		ro2
               lo2    		ri2
      -> FabricPatch fab
               (li1 :> li2)			(ro1 :> ro2)
               (lo1 :> lo2)   			(ri1 :> ri2)
stackF p1 p2 ~(li1 :> li2,ri1 :> ri2) = do
	(lo1,ro1)		     <- p1 (li1,ri1)
	(lo2,ro2)		     <- p2 (li2,ri2)
        return $ (lo1 :> lo2,ro1 :> ro2)

{-
f1 |$ f2 = f1 |$| fabricPatch f2
f1 $| f2 = fabricPatch f1 |$| f2
-}
