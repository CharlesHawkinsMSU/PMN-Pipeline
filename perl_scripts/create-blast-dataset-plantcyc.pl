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
###########################################################

  Usage: $0 <pgdb> <fasta-file-folder> <version> <output-folder>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;
use LWP::Simple;

# process arguments
#
if (@ARGV < 4) {
    die $usage;
}

my ($DB, $fafolder, $version, $outdir) = @ARGV;
$fafolder =~ s!/$!!;
$outdir =~ s!/$!!;
my $source = 'UniProt';

# read phytozome/ensembl fasta file
#
my %sequences;
my $cid;
print STDERR "Reading FASTA sequences...\n";

foreach my $fa (`ls $fafolder/*`) {
    chomp $fa;
    open(my $fafile, '<', $fa) or die $!;
    while (<$fafile>) {
	next if (/^#/ || /^$/);
	if (/^>(.*)$/) {
	    my $sid = $1;
	    $cid = (split /\s+|\|/, $sid)[0];
	    $sequences{$cid}{header} = $_;
	} else {
	    #s/[^\.A-Z-]//g;
	    $sequences{$cid}{seq} .= $_;
	}
    }
    close $fafile;
}

# current PGDB
#
my $cyc = perlcyc -> new($DB);
print STDERR "Getting all proteins in PGDB $DB...\n";

# get all species info in plantcyc
#
my %species;
my @sps = $cyc -> call_func("all-subs '|TAX-1|");
foreach my $s (@sps) {
    $species{$s} = $cyc -> get_slot_value($s, "common-name");
}

# get info for all enzymes (monomer only)
#
my @enzs = $cyc -> get_class_all_instances("|Polypeptides|");

print STDERR "Generating FASTA file for ${DB}cyc...\n";
my $outcyc = "$outdir/".lc($DB)."cyc";
open(my $fastafh, '>', "$outcyc.fasta") or die $!;
open(my $infofh, '>', "$outcyc.info") or die $!;
print $infofh "Enzyme\tEnzyme_common_name\tEnzrxn_common_name\tSpecies\tGene\tGene_names\tUniProt_id\tAccession-1\tin-fasta?\n";

foreach my $enz (@enzs){
    my $printed = 'skipped';
    if ($sequences{$enz}) {
	print $fastafh $sequences{$enz}{header}, $sequences{$enz}{seq};
	$printed = 'PGDB';
    } else {
	my %enzinfo;
	
	# get species info
	$enzinfo{species} = $cyc -> get_slot_value($enz, "species");
	my $sp = $species{$enzinfo{species}} ? $species{$enzinfo{species}} : $enzinfo{species};
	
	# get common name
	$enzinfo{common_name} = $cyc -> get_slot_value($enz,"common-name");
	$enzinfo{common_name} =~ s!//!;!g;
	$enzinfo{common_name} =~ s/<.*?>//g;
	
	# get uniport acc
	my @DBlinks = $cyc -> get_dblinks($enz);
	foreach my $dbl (@DBlinks) {
	    if ($dbl =~ /\((?:UNIPROT(?:-PROTEIN-ID)?|ARRAYEXPRESS|PROTEINMODELPORTAL|MODBASE|SWISSMODEL|PRIDE|SMR) "(.*)"\s*\)$/ || $dbl =~ /\((?:UNIPROT(?:-PROTEIN-ID)?|ARRAYEXPRESS|PROTEINMODELPORTAL|MODBASE|SWISSMODEL|PRIDE|SMR) "(.*)" NIL/) {
		my $acc = $1;
		$acc =~ s/^\s*|\s*$//g;
		if ($acc) {
		    $enzinfo{uniprot}{$acc} ++;
		}
	    }
	}
	
	# get gene
	my $g = $cyc -> get_slot_value($enz, "gene");
	
	# common-name or synonyms of Arabidopsis gene
	my @synnames = $cyc -> get_slot_values($g, "synonyms");
	push @synnames, $cyc -> get_slot_value($g, "common-name");
	foreach my $s (@synnames) {
	    $enzinfo{genenames}{uc $s} ++ if ($s =~ /at.g\d{5}/i);
	}
	
	# common-name of all enzrxns of this enz
	my @enzrxns = $cyc -> get_slot_values($enz, "catalyzes");
	foreach my $enzrxn (@enzrxns) {
	    my $ername = $cyc -> get_slot_value($enzrxn, "common-name");
	    $ername =~ s/<.*?>//g;	#html tags
	    $ername =~ s/&(.*?);/$1/g;	#&beta; like words
	    $ername =~ s/<p$|<\/small$|<\/a$//;	# fragmented tags
	    $enzinfo{ernames}{$ername} ++;
	}
	
	# generate fasta file
	if ($enzinfo{uniprot}) {
	    my %uniseqs = ('uni' => $enzinfo{uniprot},
			   'seq' => '');
	    foreach my $uni (keys %{$enzinfo{uniprot}}) {
		my $flat = get("http://www.uniprot.org/uniprot/$uni.fasta");
		if ($flat) {
		    my $seq = '';
		    open(my $sfh, '<', \$flat) or die $!;
		    <$sfh>;
		    while (<$sfh>) {
			$seq .= $_;
		    }
		    #$seq =~ s/^>.*?\n//;
		    if (length $seq > length $uniseqs{seq}) {
			delete $uniseqs{uni};
			$uniseqs{uni}{$uni} ++;
			$uniseqs{seq} = $seq;
		    } elsif (length $seq == length $uniseqs{seq}) {
			$uniseqs{uni}{$uni} ++;
		    }
		    
		}
	    }
	    if ($uniseqs{seq}) {
		print $fastafh join(" | ", ">$enz", "UniProt: ".join("; ", keys %{$uniseqs{uni}}), join("; ", keys %{$enzinfo{ernames}}), "Species: $sp", "Gene: $g"), "\n", $uniseqs{seq};
		$printed = 'UniProt';
	    }
	}
	
	## print the protein info table
	print $infofh "$enz\t", $enzinfo{common_name}, "\t", join("; ", keys %{$enzinfo{ernames}}), "\t", $sp, "\t$g\t", join("; ", keys %{$enzinfo{genenames}}), "\t", join("; ", keys %{$enzinfo{uniprot}}), "\t", $enzinfo{accession_1}, "\t$printed\n";
	
	# print not included enzymes to STDERR
	if ($printed eq 'skipped') {
	    print STDERR "$enz\t", $enzinfo{common_name}, "\t", join("; ", keys %{$enzinfo{ernames}}), "\t", $sp, "\t$g\t", join("; ", keys %{$enzinfo{genenames}}), "\t", join("; ", keys %{$enzinfo{uniprot}}), "\t\t\t$printed\n";
	}
    }
}

#close $infofh;
close $fastafh;

if (-s "$outcyc.fasta") {
    print STDERR "Building BLAST dababase...\n";
    system "makeblastdb -dbtype prot -parse_seqids -in $outcyc.fasta -title \"$DB"."Cyc $version\"";
}

print STDERR "DONE.\n";
