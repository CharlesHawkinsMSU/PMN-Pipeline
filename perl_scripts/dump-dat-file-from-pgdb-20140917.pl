#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Sep., 2014

# Description: Dump .dat file from PGDB
# Input: species id in pgdb, path of pgdbs
# Output: dumpped .dat files in place

my $usage = <<USAGE;

  Usage: $0 <pgdb> <output-folder>

USAGE

use strict;
use Cwd;
use File::Spec;
use File::Basename;

if (@ARGV<2) {
    die $usage;
}

$|=1;   # Output prior
my $path = dirname(File::Spec->rel2abs(__FILE__));  # Path of this script
my $cmdline = "$0 ".join(' ',@ARGV);

# Main body of the script goes:
my ($species, $out) = @ARGV;

`mkdir -p $out`;

# use the release pipeline to do the perlcyc stuff
print STDERR "Generating master file for dumping ...\n";
my $fixmaster = <<MASTER;
s_ptools	/home/cwang/pathway-tools/aic-export/pathway-tools/ptools/17.5/pathway-tools
lisp	(so '$species)
lisp	(setq *file-export-progress* nil)
lisp	(dump-frames-to-attribute-value-files (org-data-dir))
MASTER

$fixmaster =~ s/\$species/$species/;

open(MA, ">$out/$species.dump.master");
print MA $fixmaster;
close MA;

print STDERR "Call pmn-release-pipeline to dump .dat files from PGDB ...\n";
`perl $path/../pmn-release-pipeline/pmn-release-pipeline/pmn-release-pipeline-general.pl $out/$species.dump.master $out/$species.dump.log`;
