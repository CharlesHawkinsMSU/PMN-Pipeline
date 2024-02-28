#!/usr/bin/perl

my $usage = <<USAGE;

This perlcyc script is part of

###########################################################
# PMN release pipeline of lisp and perlcyc commands       #
# Chuan Wang, Peifen Zhang @ Rhee-lab                     #
# Department of Plant Biology                             #
# Carnegie Institution for Science                        #
# Date: July 1, 2014                                      #
#                                                         #
# Input: the master file of commands in each step         #
# Output: logfile of the ptools lisp and perlcyc commands #
###########################################################

  Usage: $0 <pgdb> <e2p2-citation> <start-timestamp> <end-timestamp>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;
use Env;


# process arguments
#
if (@ARGV < 4) {
    die $usage;
}

my ($DB, $citation, $start, $end) = @ARGV;

# current PGDB
#
my $cyc = perlcyc -> new($DB);
$cyc->{'_socket_name'} = $ENV{'PTOOLS-ACCESS-SOCKET'} || '/tmp/ptools-socket';

my @enzrxns = $cyc -> get_class_all_instances("|Enzymatic-Reactions|");

foreach my $enzrxn (@enzrxns){
    # old timestamp
    my $timestamp = $cyc->get_slot_value($enzrxn,":creation-date");
    my @citations=$cyc->get_slot_values($enzrxn,"Citations");
    
    # reload E2P2 citations of enzrxns before timestamp end
    # new enzrxns within the range of start..end timestamp
    # and old enzrxns before start
    if ($timestamp le $end){
	
	# old enzrxns with existing citations
	## note that if enzrxn has a code of XXX:EV-COMP-AINF other than CHAEXX:EV-COMP-AINF (eg. RICECYCIMPORT:EV-COMP-AINF),
	## new E2P2 citation will NOT be added, XXX citation will be kept as is.
	if (scalar @citations > 0) {
	    foreach my $cit (@citations){
		if ($cit =~ /EV-COMP-AINF/) {
		    if ($cit !~ /^$citation/ && $cit =~ /^CHAE|^:EV-COMP-AINF/) {
			$cyc -> replace_slot_value($enzrxn, "citations", "\"$cit\"", "\"$citation\"");
			print STDERR "$enzrxn 's citation updated.\n";
		    }
		}
	    }
    	}
	
	# new enzrxns without any existing citations
	if (scalar @citations < 1) {
	    # add new savi citations
	    $cyc->add_slot_value($enzrxn, "citations", "\"$citation\"");
	    print STDERR "$enzrxn 's citation added.\n";
	}
	
    } else {
	print STDERR "$enzrxn 's citations ";
	print STDERR join ", ", @citations;
	print STDERR " remains unchanged since its timestamp $timestamp is later than $end.\n";
    }
}

