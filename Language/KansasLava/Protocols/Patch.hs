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

import Data.Sized.Unsigned (U8)

import qualified Data.ByteString.Lazy as B

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

{-
-- | A (shallow only) infinite FIFO, for connecting
-- MailBox's on the left to HandShake's on the right.
-- If you need a synthesizable FIFO, you can find one in the kansas-lava-cores package.

shallowFIFO :: (Rep a, Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled a)) 		(sig (Enabled a)) 
		 (sig Ready) 		() 	(sig Ack) 
shallowFIFO (inp,ack) = (full,(),toAckBox (Nothing:vals) ack)
   where
	(full,vals) = fromReadyBox inp 
-}

-- | 'unitClockPatch' forces a handshake to use the unit clock.

unitClockPatch :: (sig ~ CSeq ()) =>
	Patch (sig a)		(sig a)
	      (sig b)       ()  (sig b)
unitClockPatch ~(li,ri) = (ri,(),li)


------------------------------------------------------------------------

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
readPatch :: FilePath -> IO (Patch ()			[Maybe U8]
			           ()		()	())
readPatch fileName = do
     fileContents <- B.readFile fileName
     return $ unitPatch $ map (Just . fromIntegral) $ B.unpack fileContents


-- | 'writePatch' runs a complete circuit for the given 
-- number of cycles, writing the result to a given file.
writePatch :: FilePath 
	   -> Int
	   -> Patch () 		[Maybe U8] 
	 	    ()	a 	()
	   -> IO ()
writePatch fileName n patch = do
  B.writeFile fileName $ B.pack [ fromIntegral x  | Just x <- take n $ runPatch patch ]
