#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014

# Description: Prepare data file folders for running SAVI
# Input: table of basic info, pf file folder, pgdb folder, savi input folder
# Output: folders of files for each species listed in the input table file in savi input folder

my $usage = <<USAGE;

  Usage: $0 <input-table-file> <pf-file-folder> <pgdb-folder> <savi-folder> <old-pgdb-folder>

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
$ARGV[4] =~ s!/$!!;

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
    $data{$s}[1] = lc $data{$s}[1];
    
    # create folder for species $s
    mkdir "$ARGV[3]/input/$s";
    mkdir "$ARGV[3]/output/$s";
    
    # copy pf file to the folder
    `cp $ARGV[1]/$data{$s}[7] $ARGV[3]/input/$s/`;
    
    # read default-version to decide which version to used
    my $ver = `cat $ARGV[2]/$data{$s}[1]/default-version`;
    chomp $ver;
    
    # copy .dat files to the folder
    `cp $ARGV[2]/$data{$s}[1]/$ver/data/species.dat $ARGV[3]/input/$s/`;
    `cp $ARGV[2]/$data{$s}[1]/$ver/data/pathways.dat $ARGV[3]/input/$s/`;
    `cp $ARGV[2]/$data{$s}[1]/$ver/data/reactions.dat $ARGV[3]/input/$s/`;
    `cp $ARGV[2]/$data{$s}[1]/$ver/data/proteins.dat $ARGV[3]/input/$s/`;
    
    # modify species.dat to include ncbi taxon id
    `sed -i 's/NCBI-TAXONOMY-ID - NIL/NCBI-TAXONOMY-ID - $data{$s}[5]/' $ARGV[3]/input/$s/species.dat`;
    
    # copy old pgdb pathway file
    my $ver_old = `cat $ARGV[4]/$data{$s}[1]/default-version`;
    chomp $ver_old;
    
    `cp $ARGV[4]/$data{$s}[1]/$ver_old/data/pathways.dat $ARGV[3]/input/$s/pathways_pgdb.dat`;
}
