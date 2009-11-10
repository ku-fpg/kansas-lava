# basic testing script
# usage: perl test.pl <testrefstdout> <testbinaryname> [ARGS]
#
die "usage: perl test.pl <testrefstdout> <testbinaryname> [ARGS]\n" if ($#ARGV < 1);
$binary = $ARGV[1];
$r = $ARGV[0];

# later: go looking for the ./dist file???
if (!-e $binary) {
	$binary = "./dist/build/$binary/$binary";
}
die "can not find binary '$ARGV[1]' sorry\n" if (!-e $binary);

die "can not find reference example outputs in 'ref'\n" if (!-d "ref");
if (!-d "run") {
	die "'./run' is a file, not a directory\n" if (-e "run");
	mkdir("run");
}

system("$binary " . join(" ",splice(@ARGV,2)) . " > run/$r ");

print("diff $r $r-output\n");

open(DIFF,"diff ref/$r run/$r |") || die "can not run diff, sorry!\n";
$errs = 0;
while(<DIFF>) {
	print "Failure: $ARGV[1]\n" if ($errs == 0) ;
	print $_;
	$errs++;
}

exit 1 if ($errs > 0);
exit 0;
