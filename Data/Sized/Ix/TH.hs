{-# LANGUAGE TemplateHaskell  #-}
module Data.Sized.Ix.TH where

import Language.Haskell.TH
import Data.Array (Ix)

sizedTypeGenForUpto :: Int -> Q [Dec]
sizedTypeGenForUpto maxVal = do
	decs <- sequence [ sizedTypeGen (fromIntegral n)				-- count
			| n <- [1..maxVal]
			]
	return $ concat decs

-- v == the number of possible enumerations
-- define a smart constructor, that does bounds checking, because
-- 10 + 10 :: X16 gives 20.

sizedTypeGen :: Integer -> Q [Dec]
sizedTypeGen v = do
     let ty   = mkName ("X" ++ show v)
     let cons = mkName ("X" ++ show v)

     n <- newName "n"
     n' <- newName "n'"

     let cond op x why = 
	           CondE (InfixE (Just (VarE n)) 
				 (VarE $ mkName op) 
				 (Just (LitE (IntegerL x)))) 
		       	 (AppE (VarE 'error) 
				     (AppE (AppE (VarE $ mkName "++")
	          	                (LitE (StringL $ 
						"fixed range type exceeded " 
					     ++ why 
					     ++ " bound : X" 
					     ++ show (v - 1) 
					     ++ ", value = " 
					     )))
					(AppE (VarE 'show) (VarE n)))
			 )

     return 
	[ {- FunD (newName $ "mkX" ++ show (v - 1)) [Clause [VarP n]
	, -} NewtypeD [] ty [] 
	   (NormalC cons [(NotStrict,ConT ''Int)
			 ])
	   [''Eq,''Ord,''Ix]
	, InstanceD [] (AppT (ConT ''Enum) (ConT ty))  
		[FunD 'toEnum [Clause [VarP n]
		   (NormalB ( cond ">=" v "upper"
			    $ cond "<" 0  "lower"	
			    $ (AppE (ConE cons) 
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
		, ValD (VarP 'abs) 
		       (NormalB (VarE 'id)) []
		, FunD 'signum [Clause [VarP n]
		       (NormalB (CondE (InfixE (Just (VarE n)) 
						 (VarE $ mkName "==") 
						 (Just (LitE (IntegerL 0)))) 
					 (LitE (IntegerL 0))
					 (LitE (IntegerL 1))
			     )) []]
		] ++ 
		[ FunD (mkName op)
		     [ Clause [ ConP cons [VarP n]
			      , ConP cons [VarP n']
			      ] 
			(NormalB (AppE (VarE 'toEnum)
				       (AppE (AppE (VarE (mkName op))
			             	           (VarE n)
			                     )
				             (VarE n')
				        )
				  )) []
		     ]
		| op <- ["+","-","*"]
		])
	, InstanceD [] (AppT (ConT ''Bounded) (ConT ty))  
		[ ValD (VarP 'minBound) 
		       (NormalB  (LitE (IntegerL 0))) []
		, ValD (VarP 'maxBound) 
		       (NormalB  (LitE (IntegerL (v - 1)))) []
		]
	, InstanceD [] (AppT (ConT (mkName "Size")) (ConT ty)) 
		[ FunD (mkName "size")
		     [ Clause [ WildP
			      ] 
			(NormalB (LitE (IntegerL v))) []
		     ]
		, ValD (VarP (mkName "toIndex")) 
		       (NormalB (VarE (mkName "fromEnum"))) []
		, FunD (mkName "addIndex")
		     [ Clause [ ConP cons [VarP n]
			      , VarP n'
			      ] 
			(NormalB (AppE (ConE cons)
				       (AppE (AppE (VarE (mkName "+"))
			             	           (VarE n)) 
			   		     (VarE n')
			               )
			)) []
		     ]
		]
	]
