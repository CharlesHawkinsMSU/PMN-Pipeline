#!/usr/local/bin/perl -w
use FindBin;
use lib "$FindBin::Bin";
use perlcyc;

my $DB= $ARGV[0];

my $cyc = perlcyc -> new ($DB);
my $global_r;
my $count;
my @rxns = $cyc -> all_rxns;

####my @rxns = ("RXN-1103","RXN-1143","PHEAMINOTRANS-RXN", "RXN-699", "RXN-1882"); 
foreach my $rxn (@rxns){
    my @pathways = $cyc -> get_slot_values ($rxn, "IN-PATHWAY");
    if (@pathways){
	my @enz = $cyc -> enzymes_of_reaction ($rxn);
	if (@enz){
	    print "$rxn\n";
	}
    }
}

