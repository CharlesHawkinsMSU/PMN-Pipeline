#!/usr/bin/perl -w
use strict;

if (@ARGV < 2) {
    die "Usage: $0 <enzrxn-pair-file> <old-metacyc-rxn-file>";
}

my %map;
open(MAP, "<$ARGV[1]") or die $!;

while (<MAP>) {
    chomp;
    my @r = split /\t/;
    $map{$r[0]} ++;
}

close MAP;

open(PAIR, "$ARGV[0]") or die $!;

while (<PAIR>) {
    chomp;
    my @r = split /\t/;
    if (!$map{$r[1]}) {
        print $_, "\n";
    }
}

close PAIR;
