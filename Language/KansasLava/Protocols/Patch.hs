{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, TypeOperators #-}
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
-- Numonic for the Patch.

type Patch lhs_in 	   rhs_out	
	   lhs_out bot_out rhs_in 	
	= (lhs_in,rhs_in) -> (lhs_out,bot_out,rhs_out)

-- A common pattern, a single immutable structure on the output.
unitPatch :: a -> Patch ()      a
		        ()  ()  ()
unitPatch a = \ _ -> ((),(),a)

runPatch :: Patch ()     a
	 	  ()  b  () -> a
runPatch p = a
 where
   (_,_,a) = p ((),())


------------------------------------------------

fstPatch :: Patch a   b
		  c d e -> Patch (a :> f)   (b :> f)
				 (c :> g) d (e :> g)
fstPatch p ~(a :> f,e :> g) = (c :> g,d,b :> f)
   where
	(c,d,b) = p (a,e)

nullPatch :: Patch a 	a
		   b () b
nullPatch ~(a,b) = (b,(),a)

forwardPatch :: (li -> ro)
	    -> Patch li    ro
	             b  () b
forwardPatch f1 ~(li,ri) = (ri,(),f1 li)

backwardPatch :: (ri -> lo) 
	    -> Patch a     a
	             lo () ri
backwardPatch f2 ~(li,ri) = (f2 ri,(),li)


---------------------------------------------------------------------------

probePatch :: (Probe a, Probe b)
   => String      
   -> Patch    a        a
               b   ()   b
probePatch probeName ~(inp1, inp2) = (out2, (), out1)
   where
       (out1, out2) = id
                    $ probe probeName
                    $ (inp1, inp2)

---------------------------------------------------------------------------

-- | A 'bridge' is way of connecting a HandShake to a MailBox.
-- This is efficent; internally an 'and' and  a 'not' gate,
-- and represents a good 'bridging' interface, because
-- both side can play master, and try initiate the transfer at the same time, 
-- improving possible clock speeds.


-- This does feel like the correct place for this

bridge :: (Rep a, Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled a)) 		(sig (Enabled a)) 
		 (sig Ack) 		() 	(sig Ready) 
bridge (inp,ready) = (toAck ack,(),out)
   where
	ack = isEnabled inp `and2` fromReady ready
	out = packEnabled ack (enabledVal inp)

-- | This duplicates the incomming datum.
-- This has the behavior that neither branch sees the value
-- until both can recieve it.
dupPatch :: (Clock c, sig ~ CSeq c, Rep a)
         => Patch (sig (Enabled a))     (sig (Enabled a)  :> sig (Enabled a))	
	          (sig Ready)        () (sig Ready         :> sig Ready) 
dupPatch ~(inp,rA :> rB) = (toReady go, (), (out :> out))
  where
	go = fromReady rA .&&. fromReady rB
	out = packEnabled (go .&&. isEnabled inp) (enabledVal inp)

zipPatch :: (Clock c, sig ~ CSeq c, Rep a, Rep b)
  => Patch (sig (Enabled a)  :> sig (Enabled b))	(sig (Enabled (a,b)))
	   (sig Ack          :> sig Ack)	  ()	(sig Ready)
zipPatch ~(in1 :> in2, outReady) = (toAck ack1 :> toAck ack2, (), out)
   where
	go = fromReady outReady .&&. isEnabled in1 .&&. isEnabled in2
	ack1 = go
	ack2 = go

	out = packEnabled go (pack (enabledVal in1, enabledVal in2))

-- | 'muxPatch' chooses a the 2nd or 3rd value, based on the Boolean value.
muxPatch :: (Clock c, sig ~ CSeq c, Rep a)
  => Patch (sig (Enabled Bool) :> sig (Enabled a)  :> sig (Enabled a))	(sig (Enabled a))
	   (sig Ack            :> sig Ack          :> sig Ack)	  ()	(sig Ready)

muxPatch = noStatus $ fe `bus` matrixMuxPatch
   where
	fe = forwardPatch (\ ~(a :> b :> c) -> ((unsigned) a :> matrix [c,b])) `bus`
	     backwardPatch (\ ~(a :> m) -> (a :> (m M.! (1 :: X2)) :> (m M.! 0)))

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

-- | 'matrixMuxPatch' chooses the n-th value, based on the index value.
matrixMuxPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x)
  => Patch (sig (Enabled x)    :> Matrix x (sig (Enabled a)))		(sig (Enabled a))
	   (sig Ack            :> Matrix x (sig Ack))		  ()	(sig Ready)
matrixMuxPatch ~((cond :> m),ack) = ((toAck ackCond :> m_acks),(),out)
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

matrixDeMuxPatch :: forall c sig a x . (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x)
  => Patch (sig (Enabled x)    :> sig (Enabled a)) 		  (Matrix x (sig (Enabled a)))
	   (sig Ack            :> sig Ack)		()        (Matrix x (sig Ready))
matrixDeMuxPatch ~(ix :> inp, m_ready) = (toAck ackCond :> toAck ackIn,(),out)
   where
	-- set when ready to try go
	go = isEnabled ix .&&. isEnabled inp .&&. fromReady (pack m_ready .!. enabledVal ix)

	-- outputs
	ackCond = go
	ackIn   = go
	out     = forAll $ \ x -> packEnabled (go .&&. enabledVal ix .==. pureS x) (enabledVal inp)

unitClockPatch :: (sig ~ CSeq ()) =>
	Patch (sig a)		(sig a)
	      (sig b)       ()  (sig b)
unitClockPatch ~(li,ri) = (ri,(),li)


------------------------------------------------------------------------

{-
 - TODO: Change to 
   bus    --> >==>
   bridge --> 
   >==>, >~~> goes away (use >==>)
 -}


infixr 5 `bus`
bus ::   Patch li1 		o
	       lo1  	s1	i
      -> Patch o 		ro2
	       i 	s2  	ri2 
      -> Patch li1 			ro2	
	       lo1 (s1 :>        s2) 	ri2
(p1 `bus` p2) inp = (lhs_out1,(bot_out1 :> bot_out2),rhs_out2)
   where
	(lhs_in,rhs_in) 	     = inp
	(lhs_out1,bot_out1,rhs_out1) = p1 (lhs_in,lhs_out2)
	(lhs_out2,bot_out2,rhs_out2) = p2 (rhs_out1,rhs_in)

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

mapStatus :: (a -> b) -> Patch lhs_in rhs_out lhs_out a rhs_in
 		     -> Patch lhs_in rhs_out lhs_out b rhs_in
mapStatus f p inp = let (lhs_out,bot_out,rhs_out) = p inp
		    in (lhs_out,f bot_out,rhs_out)

noStatus :: Patch lhs_in rhs_out lhs_out a  rhs_in
         -> Patch lhs_in rhs_out lhs_out () rhs_in
noStatus = mapStatus (const ())

--------------------------------------------------

-- TODO: above or $4
infixr 3 `stack`
stack :: Patch li1		ro1
               lo1    s1	ri1
      -> Patch li2		ro2
               lo2    s2	ri2
      -> Patch (li1 :> li2)			(ro1 :> ro2)
               (lo1 :> lo2)    (s1 :> s2)	(ri1 :> ri2)
stack p1 p2 inp = (lo1 :> lo2,s1 :> s2,ro1 :> ro2)
   where
	(li1 :> li2,ri1 :> ri2)	     = inp
	(lo1,s1,ro1)		     = p1 (li1,ri1)
	(lo2,s2,ro2)		     = p2 (li2,ri2)

matrixStack :: (m ~ (Matrix x), Size x)
 	=> m (Patch li   ro
	   	    lo s ri)
	-> Patch (m li)		(m ro)
		 (m lo)  (m s)  (m ri)
matrixStack m inp = ( fmap (\ (l,_,_) -> l) m'
		    , fmap (\ (_,s,_) -> s) m'
		    , fmap (\ (_,_,r) -> r) m'
		    )
   where
	(m_li,m_ri)	= inp
	m' = (\ p li ri -> p (li,ri)) <$> m <*> m_li <*> m_ri

--------------------------------------------------


{-
- an idea
packPatch :: (Clock c, sig ~ CSeq c, Rep in1, Rep in2)
	Patch (sig in1 :> sig in2)			(sig (in1 :> in2))
	      (sig Ready :> sig Ready)		()	(sig Ack)
-}

--------------------------------------------------

-- WRONG place
beat :: (Clock c, sig ~ CSeq c) => 
	Patch ()		(sig (Enabled ()))
	      ()	()	(sig Ack)
beat ~(_,_) = ((),(),enabledS (pureS ()))

------------------------------------------------

-- | 'readPatch' reads a file into Patch, which will become the
-- lefthand side of a chain of patches.
rawReadPatch :: FilePath -> IO (Patch ()			[Maybe U8]
			           ()		()	())
rawReadPatch fileName = do
     fileContents <- B.readFile fileName
     return $ unitPatch $ map (Just . fromIntegral) $ B.unpack fileContents

readPatch :: (Read a)
		  => FilePath -> IO (Patch ()			[Maybe a]
	  		                  ()		()	())
readPatch fileName = do
     fileContents <- readFile fileName
     return $ unitPatch $ map (Just . read) $ words $ fileContents

-- | 'writePatch' runs a complete circuit for the given 
-- number of cycles, writing the result to a given file.
rawWritePatch :: FilePath 
	   -> Int
	   -> Patch () 		[Maybe U8] 
	 	    ()	st 	()
	   -> IO ()
rawWritePatch fileName n patch = do
  B.writeFile fileName $ B.pack [ fromIntegral x  | Just x <- take n $ runPatch patch ]

writePatch :: (Show a)
	   => FilePath 
	   -> Int
	   -> Patch () 		[Maybe a]
	 	    ()	st 	()
	   -> IO ()
writePatch fileName n patch = do
  writeFile fileName $ unlines [ show x | Just x <- take n $ runPatch patch ]
