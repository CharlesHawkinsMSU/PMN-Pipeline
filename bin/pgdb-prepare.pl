#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014
# Last updated by Charles Hawkins, Apr 2021

# Description: Prepare data file folders for generating PGDBs by ptools
# Input: table of basic info, pf file folder, prepared folder, ptools executable full path
# Output: folders of files for each species listed in the input table file

my $usage = <<USAGE;

  Usage: $0 <input-table-file> <pf-file-folder> <output-folder> <ptools-full-path> <pgdb-full-path>

USAGE

use strict;
use Cwd;
use File::Spec;
use File::Basename;

# Check arguments
if ($#ARGV<0) {
	die $usage;
}

$|=1;   # Output prior
my $path = dirname(File::Spec->rel2abs(__FILE__));  # Path of this script
my $cmdline = "$0 ".join(' ',@ARGV);

$ARGV[1] =~ s!/$!!;
$ARGV[2] =~ s!/$!!;
mkdir $ARGV[2];

# read input table file
my %data;
open(IN, "<$ARGV[0]") or die "Cannot open file $ARGV[0]!\n";

while (<IN>) {
    chomp;
    my @r = split /\t/;
    $data{$r[0]} = \@r;
}

close IN;

# for each specie
my @sp = keys %data;

foreach my $s (@sp) {
    next if $s =~ /^Database/;
    
    # create folder for species $s
    mkdir "$ARGV[2]/$s";
    
    # copy pf file to the folder
    `cp $ARGV[1]/$data{$s}[7] $ARGV[2]/$s/`;
    $data{$s}[8] = convert($data{$s}[8]);
    print STDERR "Preparing for $s (PGDB-UNIQUE-ID: $data{$s}[8])...\n";

    # write create.master file
    open(my $cf, '>', "$ARGV[2]/$s/create.master") or die $!;
    print $cf $ARGV[3];
    close $cf;

    # write genetic-elements.dat file
    open(GE, ">$ARGV[2]/$s/genetic-elements.dat") or die "Cannot open file $ARGV[2]/$s/genetic-elements.dat to write.\n";
    print GE "ID\t$data{$s}[3]\nNAME\t$data{$s}[3]\nANNOT-FILE\t$data{$s}[7]\n//\n";
    close GE;
    
    # write organism-params.dat file
    open(OP, ">$ARGV[2]/$s/organism-params.dat") or die "Cannot open file $ARGV[2]/$s/organism-params.dat to write.\n";
    print OP "ID\t$s\nSTORAGE\tFILE\nNAME\t$data{$s}[2]\nABBREV-NAME\t$data{$s}[4]\nSEQUENCE-SOURCE\t$data{$s}[6]\nDOMAIN\t$data{$s}[5]\nRANK\t|species|\nCODON-TABLE\t1\nMITO-CODON-TABLE\t1\nHOMEPAGE\twww.plantcyc.org\nEMAIL\tcurator\@plantcyc.org\nDBNAME\t$data{$s}[1]\nNCBI-TAXON-ID\t$data{$s}[5]\nREF-ORGID\tPLANT\nORG-COUNTER\t$data{$s}[8]\n";
    close OP;
     
    # write dump.master foreach species
    open(DP, ">$ARGV[2]/$s/dump.master") or die "Cannot open file $ARGV[2]/$s/dump.master to write.\n";
    print DP "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(setq *file-export-progress* nil)\nlisp\t(dump-frames-to-attribute-value-files (org-data-dir))\nlisp\t(dump-frames-to-tabular-files (org-data-dir))\n";
    close DP;
    
    # write dump-biopax.master foreach species
    open(DP, ">$ARGV[2]/$s/dump-biopax.master") or die "Cannot open file $ARGV[2]/$s/dump-biopax.master to write.\n";
    print DP "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(setq *file-export-progress* nil)\nlisp\t(com-export-pgdb-to-biopax)";
    close DP;
    
    # write checker.master foreach species
    open(CH, ">$ARGV[2]/$s/checker.master") or die "Cannot open file $ARGV[2]/$s/checker.master to write.\n";
    print CH "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(run-all-checks)\nlisp\t(save-kb)\n";
    close CH;
    
    # write newversion.master foreach species
    if ($data{$s}[9]) {
	open(NV, ">$ARGV[2]/$s/newversion.master") or die "Cannot open file $ARGV[2]/$s/newversion.master to write.\n";
	print NV "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(create-new-pgdb-version (current-kb) \"$data{$s}[9]\")\n";
	close NV;
    }
}

#######
sub convert {
    my $l = shift;
    my @d = split //, $l;
    my $sum = 0;
    foreach my $i (0..$#d) {
	my $c = $d[-($i+1)];
	if ($c =~ /[A-Z]/) {
	    $c = ord($c) - ord('A') + 10;
	}
	$sum += 36**$i * $c;
    }
    return $sum;
}
