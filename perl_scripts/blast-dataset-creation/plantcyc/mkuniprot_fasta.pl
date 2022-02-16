#!/usr/bin/perl -w
use strict;

### modified after Cynthia. peifenz

my $pro2uni_file = shift;   ### output of parsing_uniprot_id.pl
my $proteininfo_file = shift;  ### output of print_pgdb_protein_info.pl
my $uni2fa_file = shift;  ### output of mkfasta_list.pl

### this script reads in the protein_uniprot_matches file (proteinID\tuniprotID)

### the protein info file

### and the fasta matching file (uniprotID\tFASTA)

### and creates a file that we can load into BLAST for plantcyc


my %pro2uni;
my %uni2fa;
 
sub trim($)
{
	my $string = shift;
	$string =~ s/^\s+//;
	$string =~ s/\s+$//;
	return $string;
}

open (PRO2UNIFILE, $pro2uni_file);
while (my $line = <PRO2UNIFILE>) {
	chomp($line);
	my @map_parts = split("\t", $line);
	$pro2uni{$map_parts[0]} = $map_parts[1];
}
close(PRO2UNIFILE);

open (UNI2FAFILE, $uni2fa_file);
while (my $line = <UNI2FAFILE>) {
	chomp ($line);
	my @map_parts = split("\t", $line);
	$uni2fa{$map_parts[0]} = $map_parts[1];
}
close(UNI2FAFILE);

open (PROTINFOFILE, $proteininfo_file);
while (my $line = <PROTINFOFILE>) {
    chomp ($line);
    
    #normalize the eznrxn-common-names, we are no longer using the protein common names since they are mostly the phytozome IDs. 
    my @parts = split("\t", $line);
    my @enzrxn_common_names = split("; ", $parts[2]);
    my %names;
    for (my $i=0; $i<@enzrxn_common_names; $i++){
	$names{$enzrxn_common_names [$i]} ++;
    }
    my $names = join "; ", keys %names;
    
    if (exists ($pro2uni{$parts[0]}) ){
	my $uni_line = $pro2uni{$parts[0]};
	my @unis = split(";", $uni_line);
	for (my $i=0; $i<@unis; $i++){
	    my $uniprot_id = trim($unis[$i]);
	    if (exists ($uni2fa{$uniprot_id})){
		my $paragraph = $uni2fa{$uniprot_id};
		$paragraph =~ s/([^\n]{0,70})/$1\n/gi;
		
		print ">$parts[0] | UniProt: " . $uniprot_id . " | " . $names . " | Species: " . $parts[3]. " | Gene: " . $parts[5] . "\n";
		
		print $paragraph;
	    }
	    else{
		#print "$uniprot_id\tNo sequence\n";
	    }
	}
    } 
}

close(PROTINFOFILE);
