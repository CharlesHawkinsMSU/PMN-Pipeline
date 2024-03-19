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

  Usage: $0 <pgdb> <all-pwy-metacyc> <all-pwy-plantcyc>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use strict;
use perlcyc;
use Env;

my %metacyc;
my %plantcyc;

#my $input = 'all_pwy.meta';
my $input = $ARGV[1];
open (FILE, $input) || die "The File $input Could Not Be Found.\n";
while (my $eachLine = <FILE>) {
    chomp ($eachLine);
    $metacyc{$eachLine}="";
    print "meta has $eachLine\n";
}
close FILE;

#my $input2 = 'all_pwy.plant';
my $input2 = $ARGV[2];
open (FILE2, $input2) || die "The File $input2 Could Not Be Found.\n";
while (my $eachLine = <FILE2>) {
    chomp ($eachLine);
    $plantcyc{$eachLine}="";
}
close FILE2;

my @DBS_TO_DELETE = ("METACYC" , "PLANTCYC");

my $DB= $ARGV[0];
my $cyc = perlcyc -> new ($DB);
$cyc->{'_socket_name'} = $ENV{'PTOOLS-ACCESS-SOCKET'} || '/tmp/ptools-socket';


my @pwy = $cyc -> all_pathways ();
foreach my $pwy (@pwy) {
    print "$pwy\n";
    foreach my $db (@DBS_TO_DELETE){
        $cyc->remove_dblink($pwy, $db);
    }
    if (exists $metacyc{$pwy}) {
	$cyc -> add_slot_value ($pwy, "dblinks", "(METACYC \"$pwy\")"); 
	print "meta: $pwy\n";
    }
    if (exists $plantcyc{$pwy}) {
	$cyc -> add_slot_value ($pwy, "dblinks", "(PLANTCYC \"$pwy\")"); 
	print "plant: $pwy\n";
    }   
}
$cyc -> close();

