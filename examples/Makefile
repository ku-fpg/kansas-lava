boot::
	rm -f DUT_IN DUT_OUT
	mkfifo DUT_IN
	mkfifo DUT_OUT
#	ghci -i..:../dist/build/autogen Test.hs 
	ghc --make -i..:../dist/build/autogen Driver.hs 
	ghc --make -i..:../dist/build/autogen DUT.hs 
