{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-- This is the Foreign Implementation Interface.

module Language.KansasLava.FII where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

impl :: QuasiQuoter
impl = QuasiQuoter parseVHDLExp parseVHDLPat

-- Parse an VHDL.

parseVHDLExp :: String -> Q Exp
parseVHDLExp x = return $ LitE (stringL x)

parseVHDLPat :: String -> Q Pat
parseVHDLPat = error "Do not use impl in the pattern context"

{-
example :: (Signal Bool,Signal Bool) -> Signal Bool
example = [$vhdl| 
	entity D_latch is
	port(	data_in:	in std_logic;
		enable:		in std_logic;
		data_out:	out std_logic
	);
	end D_latch;

	architecture behv of D_latch ...;
   |]
-}
