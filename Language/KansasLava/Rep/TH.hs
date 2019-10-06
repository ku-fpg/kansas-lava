{-# LANGUAGE CPP, TemplateHaskell, TypeFamilies #-}

module Language.KansasLava.Rep.TH where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax as S

import Language.KansasLava.Types as KLT
import Language.KansasLava.Rep.Class

#if MIN_VERSION_template_haskell(2, 11, 0)
notStrict' :: BangQ
notStrict'  = bang noSourceUnpackedness noSourceStrictness
#endif

-- You would think that you could go from Name to Kansas Lava type,
-- but this breaks the staging in Template Haskell.
repIntegral :: Name -> KLT.Type -> Q [Dec]
repIntegral tyName tyType = do
   x <- newName "x"
   sequence [ instanceD
		(return [])
		(appT (conT ''Rep) (conT tyName))
		[ tySynInstD ''W (tySynEqn [conT tyName] (litT (numTyLit xSize)))
		, dataInstD  (return [])
		 	     ''X [conT tyName] Nothing
				[ normalC xConsName
#if MIN_VERSION_template_haskell(2, 11, 0)
                                          [ bangType  notStrict' (appT (conT ''Maybe) (conT tyName)) ]
#else
					  [ strictType notStrict (appT (conT ''Maybe) (conT tyName)) ]
#endif
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
	xSize     = toInteger (typeWidth tyType)

-- You would think that you could go from Name to Kansas Lava type,
-- but this breaks the staging in Template Haskell.
repBitRep :: Name -> Int -> Q [Dec]
repBitRep tyName width = do -- tyType = do
   x <- newName "x"
   sequence [ instanceD
		(return [])
		(appT (conT ''Rep) (conT tyName))
		[ tySynInstD ''W (tySynEqn [conT tyName] (litT (numTyLit xSize)))
		, dataInstD  (return [])
		 	     ''X [conT tyName] Nothing
				[ normalC xConsName
#if MIN_VERSION_template_haskell(2, 11, 0)
                                          [ bangType  notStrict' (appT (conT ''Maybe) (conT tyName)) ]
#else
					  [ strictType notStrict (appT (conT ''Maybe) (conT tyName)) ]
#endif
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
				(normalB (appE (conE 'V)
					       (litE (integerL (fromIntegral width)))
				))
				[]
			]
		, valD (varP 'toRep)
		       (normalB (varE 'bitRepToRep))
		       []
		, valD (varP 'fromRep)
		       (normalB (varE 'bitRepFromRep))
		       []
		, valD (varP 'showRep)
		       (normalB (varE 'showRepDefault))
		       []
		]
	]
  where
	strName	  = nameBase tyName
	xConsName = mkName ("X" ++ strName)
	xSize     = toInteger width
--	(typeWidth tyType)


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
