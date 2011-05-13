DIR := sims

test:
	../dist/build/kansas-lava-unittest/kansas-lava-unittest

simulate:
	$(DIR)/runsims

report:
	../dist/build/kansas-lava-testreport/kansas-lava-testreport $(DIR)

clean:
	mv sims sims.X
	rm -Rf sims.X

init:
# 	Create a symbolic link to the Prelude directory
	ln -s .. KansasLava
