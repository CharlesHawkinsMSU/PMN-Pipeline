#!/usr/bin/perl

my $usage = <<USAGE;

This perlcyc script is part of

###########################################################
# PMN release pipeline of lisp and perlcyc commands       #
# Chuan Wang, Peifen Zhang @ Rhee-lab                     #
# Department of Plant Biology                             #
# Carnegie Institution for Science                        #
# Date: May 27, 2015                                      #
#                                                         #
# Input: the master file of commands in each step         #
# Output: logfile of the ptools lisp and perlcyc commands #
#                                                         #
# To generate Phytozome transcript name for each protein  #
# and put it into accession-2 for out links               #
###########################################################

  Usage: $0 <pgdb> <species-fasta-file>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;


# process arguments
#
if (@ARGV < 2) {
    die $usage;
}

my ($DB, $fa) = @ARGV;

# read phytozome fasta file
#
my %seq;
open(my $fafile, '<', $fa) or die $!;
while (<$fafile>) {
    next if (/^#/ || /^$/);
    if (/^>(.*)$/) {
	my $sid = $1;
	my @names = split /\s+|\|/, $sid;
	my $pid = shift @names;
	foreach my $n (@names) {
	    $n =~ /^(\w+)(=|:)(.*)$/;
	    $seq{$pid}{$1} = $3;
	}
    }
}
close $fafile;

# current PGDB
#
my $cyc = perlcyc -> new($DB);

# get info for all enzymes (monomer only)
#
my @enzs = $cyc -> get_class_all_instances("|Polypeptides|");

foreach my $enz (@enzs){
    my ($acc1, $acc2);

    # get accession-1 and accession-2
    my $g = $cyc -> get_slot_value($enz, "gene");
    next unless $g;
    
    $acc1 = $cyc -> get_slot_value($g, "accession-1");
    $acc2 = $cyc -> get_slot_value($g, "accession-2");
    
    if ($seq{$acc1}) {
	$cyc -> put_slot_value($g, "accession-2", $seq{$acc1}{transcript});
	print "Accession-2: '$acc2' changed to: '$seq{$acc1}{transcript}'\n";
	#$cyc -> put_slot_value($g, "accession-2", $seq{$acc1}{pacid});
	#print "Accession-2: '$acc2' changed to: '$seq{$acc1}{pacid}'\n";
    }
}

