#!/usr/local/bin/perl -w
use strict;
use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";
use perlcyc;
use Data::Dumper;


##modified to print out molecular weight as well. -peifenz 20060503
##modified to print out compound id. -peifenz 20110309

##This is based on the script dump_compounds.pl. The scrip will pull out all compounds of all pathways. For each compound, it will print out common name, synonyms, compound hierarchy, chemical formula, smiles formula, CAS links, all the reaction equations containing the compound, and all the pathways containing each of the reactions. -PeifenZ 8/15/2004 

print "Compound_id\tCompound_common_name\tCompound_synonyms\tMolecular_weight\tChemical_formula\tSmiles\tInChI\tLinks\tEC\tReaction_equation\tPathway\n";

my ($org, $compounddatfile) = @ARGV;

my $cyc = new perlcyc($org);

my %compounds;
my $cpd = '';
open(my $cfh, '<', "$compounddatfile") or die $!;
while (<$cfh>) {
    chomp;
    if (/^UNIQUE-ID - (.*)$/) {
        $cpd = $1;
    }
    
    if (/^MOLECULAR-WEIGHT - (.*)$/) {
	$compounds{$cpd}{mw} = $1;
    }
    
    if (/^SMILES - (.*)$/) {
	$compounds{$cpd}{smiles} = $1;
    }
    
    if (/^INCHI - (.*)$/) {
	$compounds{$cpd}{inchi} = $1;
    }
    
    if (/^CHEMICAL-FORMULA - \((\w+)\s+(\w+)\)$/) {
	$compounds{$cpd}{cf} .= "$1$2 ";
    }
    
    if (/^DBLINKS - \((\S+)\s+"(\S+)"/) {
	$compounds{$cpd}{dblinks} .= "$1:$2*";
    }
    
    if (/^SYNONYMS - (.*)$/) {
	$compounds{$cpd}{synonyms} .= "$1*";
    }
    
    if (/^\/\//) {
	## not all compounds are frames, so we need to skip over those that aren't frames
	if (! ($cyc->coercible_to_frame_p($cpd))) {
	    next;
	}
	
	$compounds{$cpd}{mw} = '' unless $compounds{$cpd}{mw};
	$compounds{$cpd}{cf} =~ s/\s$// if $compounds{$cpd}{cf};
	$compounds{$cpd}{cf} = '' unless $compounds{$cpd}{cf};
	$compounds{$cpd}{dblinks} =~ s/\*$// if $compounds{$cpd}{dblinks};
	$compounds{$cpd}{dblinks} = '' unless $compounds{$cpd}{dblinks};
	$compounds{$cpd}{synonyms} =~ s/\*$// if $compounds{$cpd}{synonyms};
	$compounds{$cpd}{synonyms} = '' unless $compounds{$cpd}{synonyms};
	$compounds{$cpd}{smiles} = '' unless $compounds{$cpd}{smiles};
	$compounds{$cpd}{inchi} = '' unless $compounds{$cpd}{inchi};
	
	for my $reaction ($cyc->reactions_of_compound($cpd)) {
	    my $rname = $cyc -> get_slot_value($reaction, "EC-NUMBER");
	    if (!$rname) { $rname=$reaction; }
	    for my $pathway (pathways_of_reaction($reaction)) {
		print join("\t",$cpd, 
			   getCommonName($cpd),
			   $compounds{$cpd}{synonyms},
			   $compounds{$cpd}{mw},
			   $compounds{$cpd}{cf},
			   $compounds{$cpd}{smiles},
			   $compounds{$cpd}{inchi},
			   $compounds{$cpd}{dblinks},
			   $rname,
			   getCommonName($reaction), 
			   getCommonName($pathway));
		print "\n";
	    }
	}
    }
}


## Returns a list of pathways (id) containing a reaction
sub pathways_of_reaction {
    my $reaction = shift;
    my @pathways = $cyc -> get_slot_values($reaction, "In-pathway");
    return @pathways;
}


## Returns the common name of a frame
sub getCommonName {
    my $frameId = shift;
    return $cyc->get_name_string ($frameId, strip_html => 1);
}

