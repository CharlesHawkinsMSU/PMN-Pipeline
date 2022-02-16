#!/usr/local/bin/perl -w
#use lib '/home/bxue/Downloads/perlcyc_1.2';
use perlcyc;
###use Math::Round;
use strict;

##edited by Danny 4/12/2005
###modified by peifenz to include gene-product (protein monomer) ids 
###modified again by peifenz to include all ids

print "Pathway-id\tPathway-name\tReaction-id\tEC\tProtein-id\tProtein-name\tGene-id\tGene-name\n";

my $org = shift;
my $cyc = new perlcyc($org);

my @pathways = $cyc -> all_pathways();
#my @pathways = ("PWY-1186");

foreach my $p (@pathways) {
    my $pathwayName = getPathwayName($p);
#    my @subpathways=$cyc -> get_slot_values($p, "SUB-PATHWAYS");
#    if (!@subpathways) {
    my @reactions = $cyc -> get_slot_values ($p, "REACTION-LIST");
    foreach my $r (@reactions) {
	my $rname = $cyc -> get_slot_value($r, "EC-NUMBER");
	if (!$rname) {$rname="-";}
	my @proteins = $cyc -> enzymes_of_reaction($r);
	
	if (!@proteins) {
	    print "$p\t$pathwayName\t$r\t$rname\tunknown\tunknown\tunknown\tunknown\n";
	}
	else {
	    printProteinsReport($p, $pathwayName, $r, $rname, @proteins);
	}
    }
}
#}


sub printProteinsReport {
    my ($p, $pathwayName, $r, $rname, @proteins) = @_;
    for my $protein (@proteins) {
	my $pname = getProteinName($protein, $r);
	my @genes = $cyc -> genes_of_protein($protein);
	if (!@genes) {
	    print "$p\t$pathwayName\t$r\t$rname\t$protein\t$pname\tunknown\tunknown\n";
	}
	else {
	    printGenesReport($p, $pathwayName, $r, $rname, $protein, $pname, @genes);
	}
    }
}


sub printGenesReport {
    my ($p, $pathwayName, $r, $rname, $protein, $pname, @genes) = @_;
    foreach my $g (@genes) {
	my $gname = getGeneName($g);
	print "$p\t$pathwayName\t$r\t$rname\t$protein\t$pname\t$g\t$gname\n";
    }
}


sub getPathwayName {
    my ($p) = @_;
    my $pathwayName = $cyc -> get_slot_value($p, "COMMON-NAME");
    if (!$pathwayName) {$pathwayName = "error";}
    return $pathwayName;
}


sub getGeneName {
    my ($g) = @_;
    my $name = $cyc -> get_slot_value($g, "COMMON-NAME");
    if (!$name) {$name = $g; }
    return $name;
}


###use enzrxn common name
sub getProteinName {
    my ($protein, $r) = @_;
    my @enzrxn = $cyc -> get_slot_values($protein, "CATALYZES");
    foreach my $enzrxn (@enzrxn) {
	my $enzrxn_rxn = $cyc -> get_slot_value ($enzrxn, "reaction");
	if ($enzrxn_rxn eq $r){
	    my $name = $cyc -> get_slot_value($enzrxn, "COMMON-NAME");
	    #if (!$name) {$name = "error"; }
	    return $name;
	}
    }
}
