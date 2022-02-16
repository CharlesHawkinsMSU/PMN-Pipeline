#!/usr/bin/perl -w
use strict;

if (@ARGV < 2) {
    die "Usage: $0 <enzrxn-pair-file> <old-pair-file> <old-metacyc-rxn-mapping>";
}

my %ef;
open(EF, "<$ARGV[2]") or die $!;

while (<EF>) {
    chomp;
    my @r = split /\t/;
    $ef{$r[1]} = $r[0];
}

close EF;

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
    if (!$map{$r[0]}) {
        print $_, "\t", $ef{$r[1]}, "\n";
    }
}

close PAIR;
