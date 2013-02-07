DIR := sims
KANSAS_LAVA_ROOT := ..
RUN := env KANSAS_LAVA_ROOT=$(KANSAS_LAVA_ROOT)
N:=4
ARGS:=

runall:
	-make clean
	make test ARGS=--gensim
	make simulate
	make report

test:
	$(RUN) ../dist/build/kansas-lava-test/kansas-lava-test +RTS -N$(N) -RTS $(ARGS)

simulate:
	$(DIR)/runsims

report:
	$(RUN) ../dist/build/kansas-lava-test/kansas-lava-test --genreport 

clean:
	mv $(DIR) XX.$(DIR)
	rm -Rf XX.$(DIR) &

locks:
	find sims -name _lock -print

init:
# 	Create a symbolic link to the Prelude directory
	ln -s .. KansasLava
