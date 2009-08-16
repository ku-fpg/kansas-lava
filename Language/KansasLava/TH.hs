{-# LANGUAGE TypeFamilies, FlexibleContexts, TemplateHaskell  #-}
module Language.KansasLava.TH where

import Language.Haskell.TH
import Data.Array (Ix)

sizedTypeGenFor :: Int -> Q [Dec]
sizedTypeGenFor maxVal = do
	decs <- sequence [ sizedTypeGen (fromIntegral n)				-- count
			| n <- [1..maxVal]
			]
	return $ concat decs

-- example; this can be simplified now we are using X<n>.
-- $(sizedTypeGen (mkName "TwoOf") (mkName "One") (mkName "OneOf") 1)

-- We should just use 
--  newtype Xn = Xn Int
-- and be done with it.

-- v == the number of possible enumerations
-- define a smart constructor, that does bounds checking, because
-- 10 + 10 :: X16 gives 20.

sizedTypeGen v = do
     let ty   = mkName ("X" ++ show (v - 1))
     let cons = mkName ("X" ++ show v)

     n <- newName "n"
     n' <- newName "n'"
     return 
	[ NewtypeD [] ty [] 
	   (NormalC cons [(NotStrict,ConT ''Int)
			 ])
	   [''Eq,''Ord,''Ix]
	, InstanceD [] (AppT (ConT ''Enum) (ConT ty))  
		[FunD 'toEnum [Clause [VarP n]
		   (NormalB (CondE (InfixE (Just (VarE n)) 
						 (VarE $ mkName ">=") 
						 (Just (LitE (IntegerL v)))) 
					 (AppE (VarE 'error) 
			          	       (LitE (StringL "out of range fixed range value"))
					 )
					 (AppE (ConE cons) 
			          	       (VarE n)
					 )
			     )) []]
		, FunD 'fromEnum 
		     [ Clause [ConP cons [VarP n]] 
				(NormalB (VarE n)) []
		     ]
		]
	, InstanceD [] (AppT (ConT ''Show) (ConT ty))  
		[ ValD (VarP 'show) 
		       (NormalB (InfixE (Just (VarE 'show)) 
					(VarE $ mkName ".") 
					(Just (VarE 'fromEnum)))) []
		]
	, InstanceD [] (AppT (ConT ''Num) (ConT ty))  
	       ([ ValD (VarP 'fromInteger) 
		       (NormalB (InfixE (Just (VarE 'toEnum)) 
					(VarE $ mkName ".") 
					(Just (VarE 'fromIntegral)))) []
		] ++ 
		[ FunD (mkName op)
		     [ Clause [ ConP cons [VarP n]
			      , ConP cons [VarP n']
			      ] 
			(NormalB (AppE (ConE cons)
				       (AppE (AppE (VarE (mkName op))
			             	           (VarE n)
			                     )
				             (VarE n')
				        )
				  )) []
		     ]
		| op <- words "+ - *"
		])
	, InstanceD [] (AppT (ConT ''Bounded) (ConT ty))  
		[ ValD (VarP 'minBound) 
		       (NormalB  (LitE (IntegerL 0))) []
		, ValD (VarP 'maxBound) 
		       (NormalB  (LitE (IntegerL (v - 1)))) []
		]
	]
{-
     return 
	[ NewtypeD [] ty [] 
	   (NormalC cons [(NotStrict,AppT (ConT ''Maybe) 
					   (ConT ty'))
			 ]) 
	   [''Eq]
	, InstanceD [] (AppT (ConT ''Enum) (ConT ty))  
		[FunD 'toEnum [Clause [VarP n]
		   (NormalB (AppE (ConE cons) 
			          (CondE (InfixE (Just (VarE n)) 
						 (VarE $ mkName "==") 
						 (Just (LitE (IntegerL v)))) 
					 (ConE 'Nothing) 
					 (AppE (ConE 'Just) 
					       (AppE (VarE 'toEnum) (VarE n)
					       )
					 )
				   )
			     )
		    ) []]
		, FunD 'fromEnum 
		     [ Clause [ConP cons [ConP 'Nothing []]] 
				(NormalB (LitE (IntegerL v))) []		
		     , Clause [ConP cons [ConP 'Just [VarP n]]] 
				(NormalB (AppE (VarE 'fromEnum) (VarE n))) []
		     ]
		]
	, InstanceD [] (AppT (ConT ''Show) (ConT ty))  
		[ ValD (VarP 'show) 
		       (NormalB (InfixE (Just (VarE 'show)) 
					(VarE $ mkName ".") 
					(Just (VarE 'fromEnum)))) []
		]
	, InstanceD [] (AppT (ConT ''Num) (ConT ty))  
		[ ValD (VarP 'fromInteger) 
		       (NormalB (InfixE (Just (VarE 'toEnum)) 
					(VarE $ mkName ".") 
					(Just (VarE 'fromIntegral)))) []
		]
	]
-}
