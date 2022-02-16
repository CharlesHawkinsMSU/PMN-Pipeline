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

  Usage: $0 <species-fasta-file-folder>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;


# process arguments
#
if (@ARGV < 1) {
    die $usage;
}

my ($fafolder) = @ARGV;

# read phytozome fasta file
#
my %seq;
foreach my $fa (`ls $fafolder/*`) {
    chomp $fa;
    next unless $fa =~ /Bdistachyon|Gmax|Pvirgatum|Ppatens|Stuberosum|Sbicolor|Zea|Spolyrhiza/;
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
}

# current PGDB
#
my $cyc = perlcyc -> new('plant');

# get info for all enzymes (monomer only)
#
my @genes = $cyc -> get_class_all_instances("|Genes|");

foreach my $g (@genes){
    my ($acc1, $acc2);

    # get accession-1 and accession-2
    $acc1 = $cyc -> get_slot_value($g, "accession-1");
    $acc2 = $cyc -> get_slot_value($g, "accession-2");
    
    my @enzs = $cyc -> enzymes_of_gene($g);
    my $species = '';
    foreach my $enz (@enzs) {
	$species = $cyc -> get_slot_value($enz, 'species');
	last if $species;
    }
    
    if ($seq{$acc1}) {
	if ($species =~ /^TAX-15368$|^TAX-3847$|^TAX-38727$|^TAX-3218$|^TAX-4113$|^TAX-4558$|^TAX-381124$/) {
	    $cyc -> put_slot_value($g, "accession-2", $seq{$acc1}{transcript});
	    print "Accession-2: '$acc2' changed to: '$seq{$acc1}{transcript}'\n";
	} elsif ($species =~ /^TAX-29656$/) {
	    $cyc -> put_slot_value($g, "accession-2", $seq{$acc1}{pacid});
	    print "Accession-2: '$acc2' changed to: '$seq{$acc1}{pacid}'\n";
	}
    }
}

