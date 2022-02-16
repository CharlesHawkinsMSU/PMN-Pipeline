#!/usr/bin/perl -w
use strict;

# this file takes a agi (or whatever) to uniprot list, loops throug hit
# and finds the associated sequences from uniprot download files.

my $agitounifile = shift; #self made list of agi\tUniprot1; Uniprot2 etc..
my $unisprot = shift; #uniprot download file (sprot)
my $unitrembl = shift; #uniprot download file (trembl)

my %uniprot_list;

sub trim($)
{
	my $string = shift;
	$string =~ s/^\s+//;
	$string =~ s/\s+$//;
	return $string;
}

#store all uniprots you need to find into a hash
#print "starting agi to uni hash making\n";
open (FILE, $agitounifile);
while (my $line = <FILE>) {
	chomp ($line);
	my @map_parts = split("\t", $line);
	my @agilist = split(";", $map_parts[1]);
	for (my $i=0; $i<@agilist; $i++){
		$uniprot_list{trim($agilist[$i])} = "empty";
	}
}
close(FILE);
#print "startig file one\n";
loop_through_file($unisprot);
#print "starting file two\n";
loop_through_file($unitrembl);
#print "done. checking agis\n";

foreach my $agi (keys %uniprot_list){
	if ($uniprot_list{$agi} eq "empty"){
		#print $agi . " DOES NOT HAVE A SEQUENCE" . "\n";
	}
}

#loop through first file to see if we got any unprot sequences we need
sub loop_through_file($) {
my $unifile = shift;
open (FILE, $unifile);
my $sequence_store = 0;
my $sequence = "";
my @ids;
while (my $line = <FILE>) {
	chomp ($line);
	if ($line =~ m/^AC/) {
		# check to see if this uniprot is there
		my @line_parts = split("   ", $line);
		my @cur_ids = split(";", $line_parts[1]);
		for (my $i=0; $i< @cur_ids; $i++){
			push(@ids, trim($cur_ids[$i]));
		}
	}
	
	if ($line =~ m/^\/\//){
		##print "line : " . $line . "\n";
		#check uniprot ids see if they are in hash. if they are, store the sequence
		for (my $i=0; $i < @ids; $i++){
			if (exists $uniprot_list{$ids[$i]} ){
				if ($uniprot_list{$ids[$i]} ne "empty"){
					#print "oh dear, this uniprot list has 2 or more sequences: " . $ids[$i] . "\n";
				}
				$uniprot_list{$ids[$i]} = $sequence;
				print $ids[$i] . "\t" . $sequence . "\n";
			}
		}
		$sequence = "";
		@ids = ();
		$sequence_store = 0;
	}
	if ($sequence_store == 1){
		#print "GETTING TO SEQUENCE STORE " . $line . "\n\n\n";
		$line =~ s/\s//g;
		$sequence = $sequence . $line;
	}
	if ($line =~ m/^SQ/){
		# this is sequence. store the sequence.
		$sequence_store = 1;
	}
}

close(FILE);
}
