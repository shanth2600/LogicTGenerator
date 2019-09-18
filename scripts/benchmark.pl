#!/usr/bin/perl -w

use strict;

# Benchmark
# {
#    name
#    startBound
#    endBound
# }

# Takes:
# -Name of benchmark to run
# -Bound to run the benchmark
# Returns 1 if it terminated normally, else undef
sub runSingleHaskellBenchmark($$) {
    my ($benchmarkName, $bound) = @_;
    print "$benchmarkName; Haskell; $bound\n";
    my @output = `./timeout.pl 60 /usr/bin/time stack exec LogicTGenerator-exe -- $benchmarkName $bound 2>&1`;
    for my $line (@output) {
	print $line;
    }
    if (scalar(@output) == 3) {
	return 1;
    } else {
	print "Error detected\n";
	return undef;
    }
}

# Takes:
# -Reference to benchmark
sub runBenchmark($) {
    my $benchmarkRef = shift();
    my $run = 1;

    for (my $bound = $benchmarkRef->{startBound};
	 $bound <= $benchmarkRef->{endBound};
	 $bound++) {
	if (defined($run)) {
	    $run = runSingleHaskellBenchmark($benchmarkRef->{name}, $bound);
	} else {
	    print "Skipping bound $bound for Haskell due to previously-detected error\n";
	}
    }
}

# ---BEGIN MAIN---
my @benchmarks = (
    { name => 'bst',
      startBound => 12,
      endBound => 50 },
    { name => 'rbt',
      startBound => 14,
      endBound => 50 },
    { name => 'heap',
      startBound => 9,
      endBound => 50 },
    { name => 'bt',
      startBound => 28,
      endBound => 50 },
    { name => 'riff',
      startBound => 12,
      endBound => 50 }
    );

for my $benchmarkRef (@benchmarks) {
    runBenchmark($benchmarkRef);
}

