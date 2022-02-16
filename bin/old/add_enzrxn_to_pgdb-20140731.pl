#!/usr/local/bin/perl -w

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014

# rewrite based on Peifen's version 06222013
# Description: add enzrxn to pgdb to fix the ptools bug for inferring reactions
# Input: pgdb name, pair file of enz-rxn
# Output: new enzrxn in pgdb, no output specified

my $usage = <<USAGE;

  Usage: $0 <pgdb> <unique-species-code> <enz-rxn-pair-file> <metacyc-all-rxn-ec-mapping>

USAGE

use FindBin;
use lib "$FindBin::Bin";

use perlcyc;
use strict;

if (@ARGV<4) {
    die $usage;
}


# read metacyc all rxn list
my %metacycrxn;
open(M, "<$ARGV[3]") or die "Cannot open file $ARGV[3]";

while (<M>) {
    chomp;
    my ($r) = split /\t/;
    $metacycrxn{$r} ++;
}


# read enz-rxn-pair-file
my %pair;
my %newrxn;
open(IN, "<$ARGV[2]") or die "Cannot open file $ARGV[2].\n";

while (<IN>) {
    chomp;
    my ($enz, $rxn) = split /\t/;
    $pair{$enz}{$rxn} ++ if $metacycrxn{$rxn};
    $newrxn{$rxn} ++ if $metacycrxn{$rxn};
}

close IN;

# ID number starts from 100
my $prefix = "ENZRXN$ARGV[1]0714-";
my $id = 100;

my $cyc= perlcyc-> new($ARGV[0]);

# import rxn from metacyc if not in pgdb
my @rxns = $cyc->all_rxns();
my %pgdballrxns = map { $_ => 1 } @rxns;
my %import;

foreach my $r (keys %newrxn) {
    unless ($pgdballrxns{$r}) {
	$import{$r} ++;
    }
}

if (scalar(keys %import) > 0) {
    $cyc->send_query("(so 'meta)");
    print $cyc->retrieve_results(), "\n";
    $cyc->send_query("(all-rxns :all)");
    my @allrxn = $cyc->retrieve_results();
    my %allrxns = map { $_ => 1 } @allrxn;
    my $im = '';
    foreach (keys %import) {
	$im .= $_ . " " unless $allrxns{$_};
    }
    $cyc->send_query("( import-frames :frames '($im) :src-kb (find-org 'meta) :dst-kb (find-org '$ARGV[0]) )");
    print $cyc->retrieve_results(), "\n";
    $cyc->send_query("(so '$ARGV[0])");
    print $cyc->retrieve_results(), "\n";
}

# get all genes for adding enzrxn
my @genes = $cyc->get_class_all_instances("|Genes|");

foreach my $gene (@genes) {
    my $enz = $cyc-> get_slot_value ($gene, "Product");
    my $acc = $cyc-> get_slot_value ($gene, "Accession-1");
    my @pgdb_rxns =  $cyc -> reactions_of_gene ($gene);
    my %pgdbrxn = map { $_ => 1 } @pgdb_rxns; ###turn an array into a simple hash
    
    if ($acc && $pair{$acc}) {
	my @new_rxns = keys %{$pair{$acc}};
	foreach my $rxn (@new_rxns) {
	    if (!$pgdbrxn{$rxn}) {
	    } else {
		print "$enz ($acc) already has reaction $rxn\n";
	    }
	    $id ++;
	    my $enzrxnid = $prefix . $id;
	    $cyc->create_instance($enzrxnid, "|Enzymatic-Reactions|");
	    $cyc->put_slot_value($enzrxnid,"REACTION", $rxn);
	    $cyc->put_slot_value($enzrxnid, "ENZYME", $enz); 
	    print "Created $enzrxnid:\tenzyme: $enz\treaction: $rxn\n";
	}
    }
}
