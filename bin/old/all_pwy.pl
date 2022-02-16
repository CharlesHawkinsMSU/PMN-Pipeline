#!/usr/local/bin/perl -w 
use FindBin;
use lib "$FindBin::Bin";
use perlcyc;
 
###The output contains redundancy.
###I updated on 8/17/2011 to include superpathways. Or in another word, count all pathways. peifenz

my $DB= $ARGV[0];

my $cyc = perlcyc -> new ($DB);
my @pwys = $cyc -> all_pathways;

foreach my $pwy (@pwys){
    print "$pwy\n";
}

$cyc->close;
