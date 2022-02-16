#!/usr/bin/perl -w
use strict;

# Chuan
# 2015-05-12
# copy out *.col files from PGDB data folder
$ARGV[0] =~ s!/$!!;

my @dbs = `ls $ARGV[0]`;

