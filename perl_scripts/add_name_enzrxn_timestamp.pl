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

  Usage: $0 <pgdb> <ec-name-mapping-file> <metacyc-rxn-mapping-file> <start-timestamp> <end-timestamp>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;


# process arguments
#
if (@ARGV < 4) {
    die $usage;
}

my ($DB, $input_ec, $input_rxn, $start, $end) = @ARGV;


# read mapping files
#
my $EC = &readfile($input_ec);
my $RID = &readfile($input_rxn);


# current PGDB
#
my $cyc = perlcyc -> new($DB);

my @enzrxns = $cyc -> get_class_all_instances("|Enzymatic-Reactions|");

foreach my $enzrxn (@enzrxns){
    my $timestamp = $cyc -> get_slot_value($enzrxn,":creation-date");
    my $common_name = $cyc -> get_slot_value($enzrxn,"common-name");
    my $old_name = $common_name;
    
    # change only ENZRXNs within timestamp range start..end
    if ($timestamp > $start && $timestamp <= $end){
	
		my $rxn = $cyc -> get_slot_value($enzrxn, "reaction");
		my $enz = $cyc -> get_slot_value($enzrxn, "enzyme");
		my $gene = $cyc -> get_slot_value($enz, "gene");
		my $accession = $cyc -> get_slot_value($gene, "accession-1");
	    
		#if (!$common_name || $common_name eq $accession){
		if (exists $RID->{$rxn}) {
			$common_name = &update_name($cyc, $enzrxn, $RID->{$rxn}) if ($RID->{$rxn} ne $common_name);
		} else {
			my $rxn_EC = $cyc -> get_slot_value($rxn, "EC-number");
			$rxn_EC =~ s/^EC-//;
			print "$enzrxn\tRID not found for: $rxn\t$rxn_EC\n";
			if (exists $EC->{$rxn_EC}) {
	    		# EC name
	    		$common_name = &update_name($cyc, $enzrxn, $EC->{$rxn_EC}) if ($EC->{$rxn_EC} ne $common_name);
			} else {			
		    	print "$enzrxn\tEC not found for: $rxn_EC\n" if ($rxn_EC);
			}
			#}
		}	
		print "$enzrxn\tchanged name from '$old_name' to '$common_name'\n" if ($old_name ne $common_name);
	
    } else {
		print "$enzrxn\tcommon name '$old_name' unchanged.\n";
    }
}

##########
sub readfile {
    my $file = shift;
    my %hash;
    open (FILE, $file) || die "$! : Cannot open file $file.\n";
    
    while (<FILE>) {
	chomp;
	my ($key, $name) = split /\t/;
	$hash{$key}= $name;
    }
    
    return \%hash;
}


sub update_name {
    my ($cyc, $enzrxn, $name) = @_;
    $name =~ s/\"/\\\"/g;  ###some enzyme names in the ec-name mapping file have quotation marks, and we want to preserve it -pz.
    $cyc -> put_slot_value ($enzrxn, "common-name", "\"$name\""); 
    my $new_name = $cyc -> get_slot_value($enzrxn,"common-name");
    return $new_name;
}






