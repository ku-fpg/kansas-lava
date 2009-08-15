{-# LANGUAGE TypeFamilies, FlexibleContexts, TemplateHaskell  #-}
module Language.KansasLava.TH where

import Language.Haskell.TH


sizedTypeGenFor :: Int -> Q [Dec]
sizedTypeGenFor maxVal = do
	decs <- sequence [ sizedTypeGen (mkName ("X" ++ show n)) 		-- def type
					(mkName $ "X" ++ show (n - 1)) 		-- cons
					(mkName ("X" ++ show (n - 1)))		-- inner type
					(fromIntegral (n - 1))				-- count
			| n <- [2..maxVal]
			]
	return $ concat decs

-- example; this can be simplified now we are using X<n>.
-- $(sizedTypeGen (mkName "TwoOf") (mkName "One") (mkName "OneOf") 1)

sizedTypeGen ty cons ty' v = do
     n <- newName "n"
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
