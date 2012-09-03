INC=../..:../../dist/build/autogen

boot::
	rm -f DUT_IN DUT_OUT DUT_STATE
	mkfifo DUT_IN
	mkfifo DUT_OUT
	mkfifo DUT_STATE
	ghc --make -i$(INC) Main.hs 

proto::
	ghci -i$(INC) Main.hs
