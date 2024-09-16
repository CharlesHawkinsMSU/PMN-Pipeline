#!/usr/local/bin/perl -w

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

###revised by Peifen Zhang for PMN13 release to reload links to proteins instead of genes. 6/11/2018 

  Usage: $0 <pgdb> <DBLINKS> <accession-slot>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use Env;

###This script deletes all existing Phytozome links to proteins and reloads links using Protein "accession-1". 

#my $DBLINKS_TO_DELETE = "PHYTOZOME";
my $DBLINKS_TO_DELETE = $ARGV[1];
#my $DBLINKS_TO_UPDATE = "PHYTOZOME";
my $DBLINKS_TO_UPDATE = $ARGV[1];
#my $acc = "accession-1";
my $acc = $ARGV[2];

my $DB= $ARGV[0];
my $cyc = perlcyc -> new ($DB);
$cyc->{'_socket_name'} = $ENV{'PTOOLS_ACCESS_SOCKET'} || '/tmp/ptools-socket';

my @Proteins=$cyc->get_class_all_instances("|Polypeptides|");
foreach my $protein (@Proteins){
    $cyc->remove_dblink($protein, $DBLINKS_TO_DELETE);
    my $accession_1 = $cyc -> get_slot_value ($protein, $acc);
    if ($accession_1) {
	my $new_link = $cyc->create_dblink($DBLINKS_TO_UPDATE, $accession_1);
        $cyc->add_dblink($protein,$new_link);
    }
}

