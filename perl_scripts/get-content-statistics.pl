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
# To get numbers of pathways, enzymes, reactions,         #
# compounds and citations for PGDBs                       #
###########################################################

  Usage: $0 <path-of-pgdbs>

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

my ($path) = @ARGV;

print "Database\tPathways (total)\tEnzymes\tReactions\tCompounds\tCitations\n"; 

foreach my $DB (`ls -d $path/*cyc`) {
    chomp $DB;
    my $ver = `cat $DB/default-version`;
    
    $DB =~ s/cyc$//;
    $DB =~ s!^.*/!!;
    
    # current PGDB
    #
    my $cyc = perlcyc -> new($DB);
    
    # get numbers from PGDB
    #
    my @pwys = $cyc -> all_pathways;
    my @enzs = $cyc -> get_class_all_instances("|Polypeptides|");
    my @cpls = $cyc -> get_class_all_instances("|Protein-Complexes|");
    my @rxns = $cyc -> all_rxns;
    my @cpds = $cyc -> get_class_all_instances("|Compounds|");
    
    # get citation numbers from consistency
    #
    my $cite = 0;
    my @cfiles = `ls -t $path/${DB}cyc/$ver/reports/consistency-checker-report*`;
    while (!$cite && @cfiles) {
	my $cfile = shift @cfiles;
	chomp $cfile;
	if (-s $cfile) {
	    open(my $checkerfh, '<', $cfile) or die $!;
	    while (<$checkerfh>) {
		chomp;
		if (/^(\d+) total citations/) {
		    $cite = $1;
		    last;
		}
	    }
	}
    }

    print ucfirst($DB), "Cyc ", $ver, "\t", scalar(@pwys), "\t", (scalar(@enzs) + scalar(@cpls)), "\t", scalar(@rxns), "\t", scalar(@cpds), "\t", $cite, "\n"; 
}
