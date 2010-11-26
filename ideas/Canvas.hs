{-# LANGUAGE RankNTypes,GADTs, ExistentialQuantification, KindSignatures #-}

import Language.KansasLava

newReg :: RTL c (NewReg c)
newReg = NewReg_

data NewReg c = NewReg (forall v a . (IsReg v) => v c a)

class IsReg (v :: * -> * -> *) where
	fromReg :: Reg c a -> v c a

data Reg c a  = Reg (CSeq c a) Int

instance IsReg Reg  where
	fromReg = id

-- Project out the final answer
instance IsReg CSeq where
	fromReg (Reg seq _) = seq

-- everything except ($)
infixl 1 :=

data RTL c a where
	(:=) 	:: forall b   c . Reg c b -> CSeq c b -> RTL c ()
	Return 	:: forall a   c . a -> RTL c a
	Bind 	:: forall a b c . RTL c b -> (b -> RTL c a) -> RTL c a
	NewReg_	:: forall a   c . RTL c (NewReg c)

instance Monad (RTL c) where
	return = Return
	(>>=)  = Bind

foo :: RTL () (Seq Int)
foo = do
	NewReg r <- newReg
	r := r + (1 :: Seq Int)
	return r
	

interp :: RTL c a -> IO a	
interp (Bind (Return a) b) = interp (b a)
interp (Bind NewReg_ b)    = interp (b (NewReg (fromReg (Reg (undefined) 99))))
--interp (Bind (a := b) k)   = 
interp (a := b)		   = interp (Bind (a := b) (return))
interp (Return a)          = return a
	
main = do
	x <- interp foo
	print x
	
