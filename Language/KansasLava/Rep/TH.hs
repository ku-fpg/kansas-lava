{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Language.KansasLava.Rep.TH where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax as S

import Language.KansasLava.Types as KLT
import Language.KansasLava.Rep.Class

-- You would think that you could go from Name to Kansas Lava type,
-- but this breaks the staging in Template Haskell.
repIntegral :: Name -> KLT.Type -> Q [Dec]
repIntegral tyName tyType = do
   x <- newName "x"
   sequence [ instanceD 
		(return [])
		(appT (conT ''Rep) (conT tyName))
		[ tySynInstD ''W [conT tyName] (conT (mkName xSize))
		, dataInstD  (return [])
		 	     ''X [conT tyName]
				[ normalC xConsName
					  [ strictType notStrict (appT (conT ''Maybe) (conT tyName)) ]
				]
				[]
		, funD 'optX
		  	[clause [varP x] 
				      (normalB
					 (appE (conE xConsName)
					      (varE x)))
		 		      []
			]
		, funD 'unX
		  	[clause [conP xConsName [varP x]] 
				      (normalB
					 (varE x))
		 		      []
			]
		, funD 'repType
			[clause [wildP]
				(normalB [| tyType |])
				[]
			]
		, valD (varP 'toRep)
		       (normalB (varE 'toRepFromIntegral))
		       []
		, valD (varP 'fromRep)
		       (normalB (varE 'fromRepToIntegral))
		       []
		, valD (varP 'showRep)
		       (normalB (varE 'showRepDefault))
		       []
		] 
	]
  where
	strName	  = nameBase tyName
	xConsName = mkName ("X" ++ strName)
	xSize     = "X" ++ show (typeWidth tyType)

instance S.Lift KLT.Type where
	lift (S n) = conE 'S `appE` (lift n)
	lift (U n) = conE 'U `appE` (lift n)
	lift other = error ("lift " ++ show other)
{-

repSize :: Name -> Q Exp
repSize tyName = appE (varE (mkName "Data.Bits.bitSize"))
 		      (litE (integerL 0) `sigE` (conT tyName))

repSigned :: Name -> Q Exp
repSigned tyName = appE (varE (mkName "Data.Bits.isSigned"))
 		      (litE (integerL 0) `sigE` (conT tyName))


-}