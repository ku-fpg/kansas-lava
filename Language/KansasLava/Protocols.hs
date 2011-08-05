{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, ParallelListComp, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes, UndecidableInstances, TypeOperators #-}
module Language.KansasLava.Protocols (
	module Language.KansasLava.Protocols.Enabled,
	module Language.KansasLava.Protocols.Memory,
	module Language.KansasLava.Protocols.AckBox,
	module Language.KansasLava.Protocols.ReadyBox,
	module Language.KansasLava.Protocols.Types,
	nullPatch,
	forwardPatch,
	backwardPatch,
	bridge,
	shallowFIFO,
	bus,
	(>==>),
	(>~~>),
	(>~=>),
	beat
	, mapStatus
	, noStatus
	, stack
	
	) where

import Language.KansasLava.Protocols.Enabled
import Language.KansasLava.Protocols.Memory
import Language.KansasLava.Protocols.AckBox
import Language.KansasLava.Protocols.ReadyBox
import Language.KansasLava.Protocols.Types

import Language.KansasLava.Rep
import Language.KansasLava.Types
import Language.KansasLava.Utils
import Language.KansasLava.Seq
import Language.KansasLava.Signal

--import Language.KansasLava.Stream (Stream(..))
--import qualified Language.KansasLava.Stream as Stream

---------------------------------------------------------------------------

-- | A 'bridge' is way of connecting a HandShake to a MailBox.
-- This is efficent; internally an 'and' and  a 'not' gate,
-- and represents a good 'bridging' interface, because
-- both side can play master, and try initiate the transfer at the same time, 
-- improving possible clock speeds.

nullPatch :: Patch a 	a
		   b () b
nullPatch ~(a,b) = (b,(),a)

simplePatch :: (li -> ro,ri -> lo) 
	    -> Patch li    ro
	             lo () ri
simplePatch (f1,f2) ~(li,ri) = (f2 ri,(),f1 li)


forwardPatch :: (li -> ro)
	    -> Patch li    ro
	             b  () b
forwardPatch f1 ~(li,ri) = (ri,(),f1 li)

backwardPatch :: (ri -> lo) 
	    -> Patch a     a
	             lo () ri
backwardPatch f2 ~(li,ri) = (f2 ri,(),li)



bridge :: (Rep a, Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled a)) 		(sig (Enabled a)) 
		 (sig Ack) 		() 	(sig Ready) 
bridge (inp,ready) = (toAck ack,(),out)
   where
	ack = isEnabled inp `and2` fromReady ready
	out = packEnabled ack (enabledVal inp)

-- | A (shallow only) infinite FIFO, for connecting
-- MailBox's on the left to HandShake's on the right.
-- If you need a synthesizable FIFO, you can find one in the kansas-lava-cores package.

shallowFIFO :: (Rep a, Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled a)) 		(sig (Enabled a)) 
		 (sig Ready) 		() 	(sig Ack) 
shallowFIFO (inp,ack) = (full,(),toAckBox (Nothing:vals) ack)
   where
	(full,vals) = fromReadyBox inp 

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

beat :: (Clock c, sig ~ CSeq c) => 
	Patch ()		(sig (Enabled ()))
	      ()	()	(sig Ack)
beat ~(_,_) = ((),(),enabledS (pureS ()))

{-
liftHandShake1 :: forall sig c a . (Rep a, Clock c, sig ~ CSeq c)
              => (forall c' . (Clock c', sig' ~ CSeq c') => sig' a -> sig' (Enabled b))
	      -> Patch (sig (Enabled a))		(sig (Enabled b))
		       (sig (Ready))		()	(sig (Ack))
liftHandShake1 fn ~(en_a,ack) = (ready,(),en_b)
  where
	-- input
	(en_

	Seq s_seq _ = fn 

 (Seq s_Ready d_Ready) = res
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



-}