#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014

# Description: get enzyme to reaction list from enzrxns.dat
# Input: enzrxns.dat, genes.dat, proteins.dat
# Output: protein to rxn list

my $usage = <<USAGE;

  Usage: $0 <enzrxn.dat> <genes.dat> <proteins.dat>

USAGE

use strict;
use Cwd;
use File::Spec;
use File::Basename;

$|=1;   # Output prior
my $path = dirname(File::Spec->rel2abs(__FILE__));  # Path of this script
my $cmdline = "$0 ".join(' ',@ARGV);

# Main body of the script goes:

if (@ARGV < 3) {
    die $usage;
}


# read enzrxns.dat
my %link;
my $enz = '';

open(IN, "<$ARGV[0]") or die "Cannot open file $ARGV[0].\n";

while (<IN>) {
    chomp;
    
    if (/^UNIQUE-ID - /) {
        $enz = '';
    }
    
    if (/^ENZYME - (.*)$/) {
        $enz = $1;
    }
    
    if (/^REACTION - (.*)$/ && $enz) {
        $link{$enz}{$1} ++;
    }
}

close IN;

# read proteins.dat
my %comp;
my $pro = '';

open(IN, "<$ARGV[2]") or die "Cannot open file $ARGV[2].\n";

while (<IN>) {
    chomp;
    
    if (/^UNIQUE-ID - (.*)$/) {
        $pro = $1;
    }
    
    if (/^COMPONENT-OF - (.*)$/) {
        $comp{$pro}{$1} ++;
    }
}

close IN;

# read genes.dat
my %gene;
my $acc = '';

open(IN, "<$ARGV[1]") or die "Cannot open file $ARGV[1].\n";

while (<IN>) {
    chomp;
    
    if (/^UNIQUE-ID - /) {
        $acc = '';
    }
    
    
    if (/^ACCESSION-1 - (.*)$/) {
        $acc = $1;
    }
    
    if (/^PRODUCT - (.*)$/ && $acc) {
        $gene{$1}{$acc} ++;
        
        if ($comp{$1}) {
            foreach my $c (keys %{$comp{$1}}) {
                $gene{$c}{$acc} ++;
            }
        }
    }
}

close IN;

foreach my $e (keys %link) {
    foreach my $g (sort keys %{$gene{$e}}) {
        foreach my $r (sort keys %{$link{$e}}) {
            #print "$g\t$r\t$e\n";
            print "$g\t$r\n";
        }
    }
}

