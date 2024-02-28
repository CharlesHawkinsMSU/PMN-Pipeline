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

  Usage: $0 <pgdb> <start-timestamp> <end-timestamp> <output-folder>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;
use Env;


my $DB = $ARGV[0];
my $cyc = perlcyc -> new ($DB);
$cyc->{'_socket_name'} = $ENV{'PTOOLS-ACCESS-SOCKET'} || '/tmp/ptools-socket';

my $new_proteins = &get_new($cyc, "|Polypeptides|", $ARGV[1], $ARGV[2]);
my $new_enzrxns = &get_new($cyc, "|Enzymatic-Reactions|", $ARGV[1], $ARGV[2]);

$ARGV[3] =~ s!/$!!;

open(OUTP, ">$ARGV[3]/$DB-$ARGV[1]-$ARGV[2]-new-proteins");
print OUTP $_, "\n" foreach @$new_proteins;
close OUTP;

open(OUTE, ">$ARGV[3]/$DB-$ARGV[1]-$ARGV[2]-new-enzrxns");
print OUTE $_, "\n" foreach @$new_enzrxns;
close OUTE;


#############################
sub get_new {
    my ($cyc, $frame, $start, $end) = @_;
    my @list;
    
    my @frames = $cyc->get_class_all_instances($frame);
    
    foreach my $f (@frames){
	my $date = $cyc->get_slot_value($f,":creation-date");
	if(($date gt $start && $date le $end)){
	    push @list, $f;
	}
    }
    
    return \@list;
}

