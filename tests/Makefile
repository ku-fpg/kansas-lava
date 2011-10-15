DIR := sims
N:=2
ARGS:=

test:
	../dist/build/kansas-lava-unittest/kansas-lava-unittest +RTS -N$(N) -RTS $(ARGS)

simulate:
	$(DIR)/runsims

report:
	../dist/build/kansas-lava-testreport/kansas-lava-testreport $(DIR)

clean:
	mv sims XX.sims
	rm -Rf XX.sims &

locks:
	find sims -name _lock -print

init:
# 	Create a symbolic link to the Prelude directory
	ln -s .. KansasLava
