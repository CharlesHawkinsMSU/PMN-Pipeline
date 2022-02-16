#!/usr/local/bin/perl -w
use lib '/home/peifenz/perlcyc_1.2/';
use perlcyc;
use strict;


### modified after Anjo's. pz 06022013

#my $file = "/home/peifenz/ara/test";
my $file= "/home/peifenz/ara/arabidopsis.130430.metacyc-rxn";

my $start_num= 100;

if(!defined $file) {die "Usage $0 <input-enz-rxn-info>\n";}

open IN, $file or die "Could not open file:$file\n";


my %hash;  ###we are creating a hash of array where the key is gene, and the array elements are rxns annotated to the gene. 

while (my $line=<IN>){
    chomp $line;
    my ($g, $rxn) =split(/\t/,$line);
    if($hash{$g}){
	my @Temp = @{$hash{$g}};
	push(@Temp,$rxn);
	$hash{$g}=\@Temp;
    }
    else{
	my @Temp;
	push(@Temp,$rxn);
	     $hash{$g}=\@Temp;
    }
}

close IN;


my $cyc= perlcyc-> new("ARA");

my $enzrxn = "ENZRXNQT0613-";

my $num=1;
if($start_num){ $num = $start_num;}

my @genes = $cyc->get_class_all_instances("|Genes|");

foreach my $gene (@genes){
    
    my $enz = $cyc-> get_slot_value ($gene, "Product");
    my $accession = $cyc-> get_slot_value ($gene, "Accession-1");
    my @aracyc_rxns =  $cyc -> reactions_of_gene ($gene);
    my %temp = map { $_ => 1 }@aracyc_rxns; ###turn an array into a simple hash
    
    if ($accession){
	if ($hash{$accession}){
	    my @annotation_rxns = @{$hash{$accession}};
	    foreach my $rxn (@annotation_rxns){
		if (!exists($temp{$rxn})){ 
		    my $enzrxn_id = $enzrxn . $num;
		    $num++;
		    $cyc->create_instance($enzrxn_id, "|Enzymatic-Reactions|");
		    #$cyc->put_slot_value($enzrxn_id, "COMMON-NAME", "\"$name\"");
		    $cyc->put_slot_value($enzrxn_id,"REACTION", $rxn);
		    $cyc->put_slot_value($enzrxn_id, "ENZYME", $enz); 
		    print "Created $enzrxn_id: enzyme:$enz\treaction:$rxn\n";
		}
	    }
	}
    }
}
