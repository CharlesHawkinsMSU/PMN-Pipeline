#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Aug., 2014

# Description: get pwys in list
# Input: list, org-file
# Output: grepped pwys

my $usage = <<USAGE;

  Usage: $0 <list> <orginal-file>

USAGE

use strict;
use Cwd;
use File::Spec;
use File::Basename;

$|=1;   # Output prior
my $path = dirname(File::Spec->rel2abs(__FILE__));  # Path of this script
my $cmdline = "$0 ".join(' ',@ARGV);

# Main body of the script goes:

my %lst;

open(LST, "<$ARGV[0]") or die "Cannot open file $ARGV[0].\n";

while (<LST>) {
    chomp;
    $lst{(split)[0]} ++;
}

close LST;

open(IN, "<$ARGV[1]") or die "Cannot open file $ARGV[1].\n";

while (<IN>) {
    my @r = split;
    chomp $r[0];
    if ($lst{$r[0]}) {
        print;
    }
}

close IN;

