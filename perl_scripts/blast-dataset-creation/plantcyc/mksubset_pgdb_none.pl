#!/usr/bin/perl -w
use strict;

my $file = shift; #this is the plantcyc protein info file
my $fa = shift; #this is the subset PGDB fasta file we made for plantcyc

### print out the plantcyc protein id, if it it not in the subset PGDB fasta file. 

my %fa_hash;
my %fa_header;

###read the fasta file
open (FILE, $fa);
my $cur_seq = "";
my $cur_head = "";
while (my $line = <FILE>) {
	chomp($line);
	if ($line =~ m/^>/){
		# this is a header, if the last header is not null, store info in 
		# hash and reinitialize everything
		if ($cur_head ne ""){
			my @fa_parts = split(/\s\|\s/, $cur_head);
			my $id = $fa_parts[0];
			$id =~ s/\>//;
			if (exists($fa_hash{$id}) ){ 
			    #print STDERR "ERROR " . $id . " HAS TWO FASTA ENTRIES\n\n";
			    exit(0);
			}else{
			    $fa_hash{$id} = $cur_seq;
			    $fa_header{$id} = $cur_head;
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
###do it for the last gene
my @fa_parts = split(/\s\|\s/, $cur_head);
my $id = $fa_parts[0];
$id =~ s/\>//;
if (exists($fa_hash{$id})){ 
    #print STDERR "ERROR " . $id . " HAS TWO FASTA ENTRIES\n\n";
    exit(0);
}else{
    $fa_hash{$id} = $cur_seq;
    $fa_header{$id} = $cur_head;
    $cur_head = "";
    $cur_seq = "";
}

close (FILE);

###read the plantcyc protein info file 
open (FILE, $file);
while (my $line = <FILE>) {
    chomp ($line);
    my @parts = split("\t", $line);
    my $id = $parts[0];
    if (exists $fa_hash{$id}){
	my $header = $fa_header{$id};
	#print "$header\n";
	#print ">$line\n";
	#print ">$gene_name | PlantCyc: " . $parts[0] . " | " . $protein_common_name . " | Species: " . $parts[3] . " | Gene: " . $parts[5] . "\n";
	my $paragraph = $fa_hash{$id};
	$paragraph =~ s/([^\n]{0,70})/$1\n/gi;
	#print $paragraph;
    }
    else {
	print "$id\n";
    }
}

close(FILE);
