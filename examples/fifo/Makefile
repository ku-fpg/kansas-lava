INC=../..:../../dist/build/autogen

boot::
	rm -f DUT_IN DUT_OUT DUT_STATE
	mkfifo DUT_IN
	mkfifo DUT_OUT
	mkfifo DUT_STATE
	ghc -rtsopts --make -i$(INC) Main.hs 
	cp Main DUT 
	./DUT dut +RTS -K32M -RTS &
	./Main driver 

proto::
	ghci -i$(INC) Main.hs
