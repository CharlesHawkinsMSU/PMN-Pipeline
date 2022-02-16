#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014

# Description: Get all predictable RXNs considering EC covered RXNs
# Input: RXN list, EC list, metacyc mapping of RXN2EC
# Output: mappings of actually predictable RXNs to EC (if any)

my $usage = <<USAGE;

  Usage: $0 <rxn-list> <ec-list> <metacyc-mapping>

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

# read input rxn list file
my %rxn;
open(IN, "<$ARGV[0]") or die "Cannot open file $ARGV[0]!\n";

while (<IN>) {
    chomp;
    $rxn{$_} ++;
}

close IN;

# read input ed list file
my %ec;
open(IN, "<$ARGV[1]") or die "Cannot open file $ARGV[1]!\n";

while (<IN>) {
    chomp;
    $ec{$_} ++;
}

close IN;

# read metacyc mapping file
my %mapping;
my %ec2rid;
open(IN, "<$ARGV[2]") or die "Cannot open file $ARGV[2]!\n";

while (<IN>) {
    next if (/^ID/);
    chomp;
    my @r = split /\t/;
    $r[2] =~ s/ -- .*$//;
    $mapping{$r[0]} = $r[2];
    $ec2rid{$r[2]}{$r[0]} ++;
}

close IN;

# for each EC
my @e = keys %ec;

foreach my $f (@e) {
    foreach my $rx (keys %{$ec2rid{$f}}) {
	$rxn{$rx} ++;
    }
}


# for each RXN
my @r = sort keys %rxn;

foreach my $s (@r) {
    print $s, "\t", $mapping{$s}, "\n";
}
