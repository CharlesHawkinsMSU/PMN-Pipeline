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

  Usage: $0 <pgdb> <version> <species> <seq-source> <species-fasta-file> <output-folder> [AGI2UniProt]

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;
use LWP::Simple;

print("Blast dataset generation will now commence\n");

# process arguments
#
if (@ARGV < 6) {
    die $usage;
}

my ($DB, $version, $sp, $source, $fa, $outdir) = @ARGV;
print("Blast dataset generation for ".$DB."\n");
$outdir =~ s!/$!!;
$source = 'ENSEMBL' if $source =~ /ENSEMBL/;

# read phytozome/ensembl fasta file
#
my %sequences;
my %header;
my $cid;
open(my $fafile, '<', $fa) or die $!;
while (<$fafile>) {
    next if (/^#/ || /^$/);
    if (/^>(.*)$/) {
	my $sid = $1;
	my @names = split /\s+|\|/, $sid;
	$cid = shift @names;
	foreach my $n (@names) {
	    $n =~ /^(\w+)(=|:)(.*)$/;
	    $header{$cid}{$1} = $3;
	}
    } else {
	#s/[^\.A-Z-]//g;
	$sequences{$cid} .= $_;
    }
}
close $fafile;

# for aracyc, AGI to uniprot
#
my %agi2uniprot;
if ($sp =~ /arabidopsis.*thaliana/i && $ARGV[5] && -s $ARGV[5]) {
    open(my $agi2uni, '<', $ARGV[5]) or die $!;
    while (<$agi2uni>) {
	chomp;
	my ($agi, $uni) = split /\t/;
	$agi2uniprot{$agi}{$uni} ++;
    }
    close $agi2uni;
}

# current PGDB
#
my $cyc = perlcyc -> new($DB);

# get info for all enzymes (monomer only)
#
my @enzs = $cyc -> get_class_all_instances("|Polypeptides|");

mkdir $outdir unless -d $outdir;

my $outcyc = "$outdir/".lc($DB)."cyc";
open(my $fastafh, '>', "$outcyc.fasta") or die $!;
open(my $infofh, '>', "$outcyc.info") or die $!;
#print $infofh "Enzyme\tEnzyme_common_name\tEnzrxn_common_name\tSpecies\tGene\tGene_names\tUniProt_id\tAccession-1\tAccession-2\tin-fasta?\n";
print $infofh "Enzyme\tEnzyme_common_name\tEnzrxn_common_name\tSpecies\tGene\tGene_names\tUniProt_id\tAccession-1\tin-fasta?\n";

foreach my $enz (@enzs){
    my %enzinfo;
    
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
    
    # get accession-1 and accession-2 (original)
   # my $g = $cyc -> get_slot_value($enz, "gene");
   # $enzinfo{accession_1} = $cyc -> get_slot_value($g, "accession-1");
   # $enzinfo{accession_2} = $cyc -> get_slot_value($g, "accession-2");
    
	# get accession-1 and accession-2 (revised by pzhang on 3/6/2018)
    my $g = $cyc -> get_slot_value($enz, "gene");
    $enzinfo{accession_1} = $cyc -> get_slot_value($enz, "accession-1");
	$enzinfo{accession_1} =~ s/\|//g;
    #$enzinfo{accession_2} = $cyc -> get_slot_value($g, "accession-2");
	
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
    my $printed = 'skipped';
    # print "$sequences{$enzinfo{accession_1}}\n";
    if ($sequences{$enzinfo{accession_1}}) {
	print STDERR "found-acc1";
	# protein has accession_1 from phytozome/ensembl
	my $outid = $enzinfo{accession_1};
	if ($source =~ /phytozome|maizegdb/i ) {
	    if ($sp =~ /spirodela/i) {
		$outid = $header{$outid}{pacid};
	    } else {
		$outid = $header{$outid}{transcript};
	    }
	}
	
	print $fastafh join(" | ", ">$enz", "$source: $outid", join("; ", keys %{$enzinfo{ernames}}), "Species: $sp", "Gene: $g"), "\n", $sequences{$enzinfo{accession_1}};
	$printed = 'accession_1';
    } # elsif ($sequences{$enzinfo{accession_2}}) {
	# protein has accession_1 from phytozome/ensembl
	# my $outid = $enzinfo{accession_2};
	# if ($source =~ /phytozome|maizegdb/i ) {
	#     if ($sp =~ /spirodela/i) {
	# 	$outid = $header{$outid}{pacid};
	#     } else {
	# 	$outid = $header{$outid}{transcript};
	#     }
	# }
	
	# print $fastafh join(" | ", ">$enz", "$source: $outid", join("; ", keys %{$enzinfo{ernames}}), "Species: $sp", "Gene: $g"), "\n", $sequences{$enzinfo{accession_2}};
	# $printed = 'accession_2';
    # } 
else {
	print STDERR "no acc-1\n";
	print STDERR "$_ $enzinfo{$_}\n" for (keys %enzinfo);
	# arabidopsis, map additional uniprot from agi
	unless ($enzinfo{uniprot}) {
	    if ($agi2uniprot{$g}) {
		$enzinfo{uniprot}{$agi2uniprot{$g}} ++;
	    } else {
		foreach my $agi (keys %{$enzinfo{genenames}}) {
		    if ($agi2uniprot{$agi}) {
			$enzinfo{uniprot}{$agi2uniprot{$agi}} ++;
		    }
		}
	    }
	}
	
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
    }
    
    # print the protein info table
    # print $infofh "$enz\t", $enzinfo{common_name}, "\t", join("; ", keys %{$enzinfo{ernames}}), "\t", $sp, "\t$g\t", join("; ", keys %{$enzinfo{genenames}}), "\t", join("; ", keys %{$enzinfo{uniprot}}), "\t", $enzinfo{accession_1}, "\t", $enzinfo{accession_2}, "\t$printed\n";
    print $infofh "$enz\t", $enzinfo{common_name}, "\t", join("; ", keys %{$enzinfo{ernames}}), "\t", $sp, "\t$g\t", join("; ", keys %{$enzinfo{genenames}}), "\t", join("; ", keys %{$enzinfo{uniprot}}), "\t", $enzinfo{accession_1}, "\t$printed\n";
    # print "$printed\n"; 
    # print not included enzymes to STDERR
    if ($printed eq 'skipped') {
	# print STDERR "$enz\t", $enzinfo{common_name}, "\t", join("; ", keys %{$enzinfo{ernames}}), "\t", $sp, "\t$g\t", join("; ", keys %{$enzinfo{genenames}}), "\t", join("; ", keys %{$enzinfo{uniprot}}), "\t", $enzinfo{accession_1}, "\t", $enzinfo{accession_2}, "\t$printed\n";
    print STDERR "$enz\t", $enzinfo{common_name}, "\t", join("; ", keys %{$enzinfo{ernames}}), "\t", $sp, "\t$g\t", join("; ", keys %{$enzinfo{genenames}}), "\t", join("; ", keys %{$enzinfo{uniprot}}), "\t", $enzinfo{accession_1}, "\t$printed\n";
    }
    
}

close $infofh;
close $fastafh;

if (-s "$outcyc.fasta") {
    system "makeblastdb -dbtype prot -parse_seqids -in $outcyc.fasta -title \"$DB"."Cyc $version\"";
}
