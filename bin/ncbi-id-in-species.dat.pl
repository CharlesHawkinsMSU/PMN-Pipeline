#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014

# Description: Prepare data file folders for running SAVI
# Input: table of basic info, pf file folder, pgdb folder, savi input folder
# Output: folders of files for each species listed in the input table file in savi input folder

my $usage = <<USAGE;

  Usage: $0 <input-table-file> <pf-file-folder> <pgdb-folder> <savi-inupt-folder>

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
$ARGV[3] =~ s!/$!!;

# read input table file
my %data;
open(IN, "<$ARGV[0]") or die "Cannot open file $ARGV[0]!\n";

while (<IN>) {
    chomp;
    my @r = split /\t/;
    $data{$r[1]} = \@r;
}

close IN;

# for each specie
my @sp = keys %data;

foreach my $s (@sp) {
    next if $s =~ /^Database/;
    my $ls = lc $s;
    
    # create folder for species $s
    mkdir "$ARGV[3]/$ls";
    
    # copy pf file to the folder
    `cp $ARGV[1]/$data{$s}[7] $ARGV[3]/$ls/`;
    
    # copy .dat files to the folder
    `cp $ARGV[2]/$ls/1.0/data/species.dat $ARGV[3]/$ls/`;
    `cp $ARGV[2]/$ls/1.0/data/pathways.dat $ARGV[3]/$ls/`;
    `cp $ARGV[2]/$ls/1.0/data/reactions.dat $ARGV[3]/$ls/`;
    `cp $ARGV[2]/$ls/1.0/data/proteins.dat $ARGV[3]/$ls/`;
    
    # modify species.dat to include ncbi taxon id
    `sed -i 's/NCBI-TAXONOMY-ID - NIL/NCBI-TAXONOMY-ID - $data{$s}[5]/' $ARGV[3]/$ls/species.dat`;
}
