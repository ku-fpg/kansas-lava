TESTME=perl test.pl

# basic, until we get our grammar based scripting tool.

runtests::
	$(TESTME) kansas-lava-test1.stdout kansas-lava-test1
	@echo "********* Passed all tests *************"

clean::
	rm -Rf run
	
