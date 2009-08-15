{-# LANGUAGE TypeFamilies, FlexibleContexts, TemplateHaskell  #-}
module Language.KansasLava.Vector where

import Language.Haskell.TH
import Language.KansasLava.TH

--data X1 a = X1 a
--data TwoOf a = TwoOf a a


data	Vector i a = Vector [a]
	deriving Show

(!) :: (Enum n) => Vector n a -> n -> a
(!) (Vector xs) n = xs !! fromEnum n

instance Functor (Vector i) where
	fmap f (Vector xs) = Vector (fmap f xs)

coords :: (Enum i, Bounded i) => Vector i i
coords = Vector [minBound .. maxBound]

fromList :: Vector i a -> [a]
fromList (Vector xs) = xs

toList :: (Enum i, Bounded i) => [a] -> Vector i a
toList xs = check (Vector xs) minBound maxBound
    where 
	check :: (Enum i) => Vector i a -> i -> i -> Vector i a
	check res low high = if fromEnum low == 0 && (fromEnum high == length xs - 1)
			     then res
			     else error "bad side of list for toList, in Vector"

--------------------------------

data X1 = Zero deriving Eq
instance Show X1 where	show = show . fromEnum 
instance Enum X1 where	toEnum 0	= Zero
			toEnum n	= error $ "trying to represent " ++ show n ++ " inside bounding type"
			fromEnum Zero 	= 0
instance Num X1 where	fromInteger = toEnum . fromIntegral


toEnumV :: (Enum a) => Int -> Int -> Maybe a 				
toEnumV n m | n == m    = Nothing
	    | otherwise = Just (toEnum n)

fromEnumV :: (Enum t) => Maybe t -> Int -> Int
fromEnumV Nothing  s = s
fromEnumV (Just v) _ = fromEnum v

fromEnumV' :: (SizedType u, Enum (Pred u)) => u -> Int
fromEnumV' u = case demote u of
		     Nothing -> elementCount u - 1
		     Just v  -> fromEnum v

--------------------------------

class SizedType s where
	type Pred s
	elementCount :: s -> Int
	promote :: Maybe (Pred s) -> s
	demote ::  s -> Maybe (Pred s)

data Void = Void

instance SizedType Void		  where type Pred Void 		= Void
					elementCount _ 		= 0
					promote _ 		= error "promote"
					demote _ 		= error "demote"
{-
instance SizedType X1 	  where type Pred X1 	= Void
					elementCount _ 		= 1
					promote Nothing 	= Zero
					promote (Just Void)	= Zero
					demote Zero 		= Nothing
					
instance SizedType TwoOf 	  where type Pred TwoOf 	= X1
					elementCount _  	= 2
					promote         	= One
					demote  (One v) 	= v
instance SizedType ThreeOf 	  where type Pred ThreeOf 	= TwoOf
					elementCount _  	= 3
					promote         	= Two
					demote  (Two v) 	= v
instance SizedType FourOf 	  where type Pred FourOf 	= ThreeOf
					elementCount _  	= 4
					promote         	= Three
					demote  (Three v) 	= v
-}
{-
instance Num Zero where
	fromInteger 0 = Zero
	fromInteger other = error $ "other" ++ show other

instance Show One where
	show (One Nothing)  = "1"
	show (One (Just s)) = show s

instance Num One where
	fromInteger 1 = One Nothing
	fromInteger n = One (Just (fromInteger n))	
-- instance Num 

class Vector n where
	data V n :: * -> *
	(!) :: V n a -> n -> a
	toList :: V n a -> [a]
	fromList :: [a] -> V n a

instance Vector Zero where
	data V Zero a = X1 a
	(X1 a) ! 0 = a
	toList (X1 a) = [a]
	fromList [a] = X1 a

instance Vector One where
	data V One a = TwoOf a a
	(TwoOf a0 a1) ! 0 = a0
	(TwoOf a0 a1) ! 1 = a1
	toList (TwoOf a0 a1) = [a0,a1]
	fromList [a0,a1] = TwoOf a0 a1

-}

--instance Vect
me = [d| f = show . fromEnum 
     |]

$(sizedTypeGenFor 16)

-- $(sizedTypeGen (mkName "TwoOf") (mkName "One") (mkName "X1") 1)
-- $(sizedTypeGen (mkName "ThreeOf") (mkName "Two") (mkName "TwoOf") 2)

-- $(me2 ''ThreeOf (mkName "Two") ''TwoOf)
-- $(me2 ''FourOf (mkName "Three") ''ThreeOf)


-- InstanceD [] (AppT (ConT ''Enum) (ConT t))  []]

{-[InstanceD [] (AppT (ConT GHC.Enum.Enum) (ConT Language.KansasLava.Vector.TwoOf)) 
[FunD toEnum [Clause [VarP n_0] (NormalB (AppE (ConE Language.KansasLava.Vector.One)
 (CondE (InfixE (Just (VarE n_0)) (VarE GHC.Classes.==) (Just (LitE (IntegerL 1)))) 
(ConE Data.Maybe.Nothing) (AppE (ConE Data.Maybe.Just) (AppE (VarE toEnum) (VarE n_0)))))) []],
FunD fromEnum [Clause [ConP Language.KansasLava.Vector.One [ConP Data.Maybe.Nothing []]] (NormalB (LitE (IntegerL 1))) [],Clause [ConP Language.KansasLava.Vector.One [ConP Data.Maybe.Just [VarP v_1]]] (NormalB (AppE (VarE fromEnum) (VarE v_1))) []]]]

-}
