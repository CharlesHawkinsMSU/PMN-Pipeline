#!/usr/bin/perl -w
use strict;


### Written by: Peifen Zhang 

my $file_1 = shift; ### this is the output of mkphytozome_none.pl. It is a list of proteins that don't have sequence in phytozome. The first column is the PGDB's internal protein id.  

my $file_2 = shift; ### this is the PGDB protein info file. 

my %hash;

open (FILE, $file_1) || die "The File $file_1 Could Not Be Found.\n";
while (my $eachLine = <FILE>) {
    chomp($eachLine);
    my @parts = split(/\t/, $eachLine);
    my $id = $parts[0];
    $hash{$id}= "";
}
 
open (FILE, $file_2) || die "The File $file_2 Could Not Be Found.\n";

while (my $eachLine = <FILE>) {
    chomp($eachLine);
    my ($protein, $pname, $ename, $species, $gene, $gname, $uniprot) = split(/\t/, $eachLine);
    if (exists $hash{$protein}){
	if ($uniprot){
	    print "$protein\t$uniprot\t$species\n";
	}
    }
}
exit
