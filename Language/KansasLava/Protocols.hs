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
	-- for now
	, upFlux, downFlux, fluxCapacitor
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

import Language.KansasLava.Stream (Stream(..))
import qualified Language.KansasLava.Stream as Stream

---------------------------------------------------------------------------

-- | A 'bridge' is way of connecting a HandShake to a MailBox.
-- This is efficent; internally an 'and' and  a 'not' gate,
-- and represents a good 'bridging' interface, because
-- both side can play master, and try initiate the transfer at the same time, 
-- improving possible clock speeds.

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

upFlux :: forall a c1 sig1 c2 sig2 . (Rep a, Clock c1, Clock c2, sig1 ~ CSeq c1, sig2 ~ CSeq c2)
       => ( sig2 Bool, sig1 a ) -> sig2 (Enabled a)
upFlux ~( ~(Seq s_b d_b) , ~(Seq s_a d_a)) = res
  where
	res = Seq s_enB d_enB

	s_enB = upsample0 s_a s_b
	d_enB = D $ Port "o0" $ E $
			Entity (Prim "pair") 
				[("o0",bitTypeOf res)]
				[("i0",B,Port "o_en" $ E $ e)
				,("i1",ty,Port "o0" $ E $ e)
				]
	
	ty = bitTypeOf (undefined :: Seq a)

	e = Entity (External "upflux")
                   [("o_en",B)
                   ,("o0",ty)
		   ,("o_clk_en",B)
		   ]
                   [("i0",ty, unD d_a)
                   ,("go",B, unD d_b)
		   , ("clk",ClkTy, Pad $ OVar (-2) "clk")
		   , ("rst",B,     Pad $ OVar (-1) "rst")
                   ]

	-- first value is unknown, because this is a clock enable
        upsample0 ss readys = 
		XMaybe (pureX False, unknownX) `Cons` upsample ss readys

	-- Only steps when you have a Boolean yes
	upsample :: Rep a => Stream (X a) -> Stream (X Bool) -> Stream (X (Maybe a))
        upsample ss (XBool ((WireVal True)) `Cons` readys) 
		= case ss of
		   (s `Cons` ss') -> XMaybe (pureX True, s) `Cons` upsample ss' readys
        upsample ss (XBool ((WireVal False)) `Cons` readys) 
		= XMaybe (pureX False, unknownX) `Cons` upsample ss readys
        upsample _ (XBool _ `Cons` _) = Stream.repeat unknownX

downFlux :: forall a c1 sig1 c2 sig2 . (Rep a, Clock c1, Clock c2, sig1 ~ CSeq c1, sig2 ~ CSeq c2)
       => sig1 (Enabled a) -> ( sig1 Bool , sig2 a )
downFlux sig = (Seq s_out_b d_out_b, Seq s_out_a d_out_a )
   where
	(Seq s_in_b d_in_b,Seq s_in_a d_in_a) = unpack sig
	
	ty = bitTypeOf (undefined :: Seq a)


	s_out_b = s_in_b	-- pass through for shallow
	d_out_b = D $ Port "go" $ E $ e
	
	s_out_a = downsample s_in_b s_in_a
	d_out_a = D $ Port "o0" $ E $ e
	
	e = Entity (External "downflux")
                   [("go",B)
                   ,("o0",ty)
		   ]
                   [("i0",ty, unD d_in_a)
                   ,("en",B, unD d_in_b)
		   ,("width", GenericTy
                            , Generic (fromIntegral (repWidth (Witness :: Witness a))))
		   , ("clk",ClkTy, Pad $ OVar (-2) "clk")
		   , ("rst",B,     Pad $ OVar (-1) "rst")
                   ]

	downsample :: Rep a => Stream (X Bool) -> Stream (X a) -> Stream (X a)
	downsample (XBool (WireVal True) `Cons` ens) (s `Cons` ss) 
		= s `Cons` downsample ens ss
	downsample (XBool (WireVal False) `Cons` ens) (_ `Cons` ss) 
		= downsample ens ss
	downsample (XBool _ `Cons` _) _
		= Stream.repeat unknownX
		

fluxCapacitor :: forall a b c sig . (Rep a, Rep b, Clock c, sig ~ CSeq c)
	=> (forall c' sig' . (Clock c', sig' ~ CSeq c') => sig' a -> sig' b)
	-> sig (Enabled a) -> sig (Enabled b)
fluxCapacitor f sig = upFlux ( clk_en, f slow_a )
    where
	clk_en :: sig Bool
	slow_a :: Seq a	-- can be *any*, so we use the standard clock for wiring.
	( clk_en, slow_a ) = downFlux sig

