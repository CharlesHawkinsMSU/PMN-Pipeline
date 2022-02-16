#!/usr/bin/perl -w
use strict;

### Written by: Cynthia Lee
### Last updated by: Peifen Zhang

my $file = shift; ### This is the pgdb protein info file. We are using the gene accession-1 as the unique id to look up fasta sequences. peifenz 9/28/2012

my $fa = shift; ### This is the fasta sequence file downloaded from Phytozome, used as input for E2P2V2. 

### this script will print out proteins for which a fasta sequencen could not be found. 

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
		#print "ERROR " . $fa_parts[0] . " HAS TWO FASTA ENTRIES\n\n";
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
#print "ERROR " . $fa_parts[0] . " HAS TWO FASTA ENTRIES\n\n";
    exit(0);
}else{
    $fa_hash{$id} = $cur_seq;
    $cur_head = "";
    $cur_seq = "";
}



open (FILE, $file);
while (my $line = <FILE>) {
    $line_number++;
    chomp ($line);
    my @parts = split("\t", $line);
    my $gene_accession = $parts[7];
    if (!$gene_accession){
	print "$parts[0]\tno-accession-1\n";
    } else {
	if (!exists $fa_hash{$gene_accession} ){
	    print "$parts[0]\t$gene_accession\tno-e2p2v2-seq\n";
	}
    }
}

close(FILE);
