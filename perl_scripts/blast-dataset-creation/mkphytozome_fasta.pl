#!/usr/bin/perl -w
use strict;

# modified after Cynthia. This works with Lee's fasta files downloaded from Phytozome. peifenz 3/26/2012 

my $file = shift; ### This is the pgdb protein info file. We are using the gene accession-1 as the unique id to look up fasta sequences. peifenz 9/28/2012

my $fa = shift; ### this is the fasta file from Phytozome.

my $line_number = 0;
my %fa_hash;

#read the fasta file
open (FILE, $fa);
my $cur_seq = "";
my $cur_head = "";
while (my $line = <FILE>) {
    chomp($line);
    if ($line =~ m/^>/){
	# this is a header, if the last header is not null, store info in 
	# hash and reinitialize everything
	if ($cur_head ne ""){
	    my @fa_parts = split(/\|/, $cur_head);
	    my $id = $fa_parts[0];
	    $id =~ s/\>//;
	    if (exists($fa_hash{$id}) ){ 
		print "ERROR " . $fa_parts[0] . " HAS TWO FASTA ENTRIES\n\n";
		exit(0);
	    }else{
		$fa_hash{$id} = $cur_seq;
		$cur_head = "";
		$cur_seq = "";
	    }
	}
	$cur_head = $line;
	
	#else this is part of the sequence
    } else {
	$cur_seq = $cur_seq . $line;
    }
}
#do it for the last gene
my @fa_parts = split(/\|/, $cur_head);
my $id = $fa_parts[0];
$id =~ s/\>//;
if (exists($fa_hash{$id}) ){ 
print "ERROR " . $fa_parts[0] . " HAS TWO FASTA ENTRIES\n\n";
    exit(0);
}else{
    $fa_hash{$id} = $cur_seq;
    $cur_head = "";
    $cur_seq = "";
}

##print the new fasta file

open (FILE, $file);
while (my $line = <FILE>) {
    $line_number++;
    chomp ($line);
    my @parts = split("\t", $line);
    my $gene_accession = $parts[7];
    if ($gene_accession){
	if ($fa_hash{$gene_accession}){
	    my @enzrxn_common_names = split("; ", $parts[2]);
	    my %names;
	    for (my $i=0; $i<@enzrxn_common_names; $i++){
		$names{$enzrxn_common_names [$i]} ++;
	    }
	    my $names = join "; ", keys %names;
	    print ">$parts[0] | Phytozome: " . $gene_accession . " | " . $names . " | Species: " . $parts[3] . " | Gene: " . $parts[5] . "\n";
	    my $paragraph = $fa_hash{$gene_accession};
	    $paragraph =~ s/([^\n]{0,70})/$1\n/gi;
	    print $paragraph;
	}
    }
}

close(FILE);
