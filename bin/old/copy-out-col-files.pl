#!/usr/bin/perl -w
use strict;

# Chuan
# 2015-05-12
# copy out *.col files from PGDB data folder

die "Usage: $0 <pgdb-user-folder> <col/dat/owl> <output-path>" if @ARGV<3;

$ARGV[0] =~ s!/$!!;
mkdir $ARGV[2];
mkdir "$ARGV[2]/$ARGV[1]-files";
foreach my $cyc (`ls $ARGV[0]`) {
    chomp $cyc;
    my $version = `cat $ARGV[0]/$cyc/default-version`;
    my $prefix = "$ARGV[2]/$ARGV[1]-files/${cyc}_${version}_";
    my $prefix2 = "$ARGV[2]/$ARGV[1]-files/${cyc}_${version}/";
    mkdir $prefix2;
    foreach my $f (`ls $ARGV[0]/$cyc/$version/data/`) {
        chomp $f;
        next unless $f =~ /\.$ARGV[1]$/;
        `cp $ARGV[0]/$cyc/$version/data/$f $prefix$f`;
        `cp $ARGV[0]/$cyc/$version/data/$f $prefix2$f`;
    }
}

