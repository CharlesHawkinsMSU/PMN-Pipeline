#!/usr/bin/perl

my $usage = <<USAGE;

  Usage: $0 <rpsd-fasta-ef-file> <rpsd-annotation-file> <reaction-name-mapping> <EC-name-mapping> <output-folder>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;
use LWP::Simple;


# process arguments
#
if (@ARGV < 5) {
    die $usage;
}

my ($fa, $annot, $rxn, $ec, $outdir) = @ARGV;
$outdir =~ s!/$!!;


# read reaction name file
#
my %rxns;
open(my $rfh, '<', $rxn) or die $!;
while (<$rfh>) {
    chomp;
    my @r = split /\t/;
    $rxns{$r[0]} = $r[-1];
}
close $rfh;


# read ec name file
#
my %ecs;
open(my $efh, '<', $ec) or die $!;
while (<$efh>) {
    chomp;
    my @r = split /\t/;
    $r[0] =~ s/^EC-//;
    $r[-1] =~ s/<.*?>//g;	#html tags
    $r[-1] =~ s/&(.*?);/$1/g;	#&beta; like words
    $r[-1] =~ s/<p$|<\/small$|<\/a$//;	# fragmented tags
    $ecs{$r[0]} = $r[-1];
}
close $efh;


# read annot file
#
my %annotations;
open(my $afh, '<', $annot) or die $!;
while (<$afh>) {
    chomp;
    my ($acc, $ann) = split /\|/;
    my @l = split /,|:/, $ann;
    foreach my $m (@l) {
	next if $m eq 'NA';
	if ($m =~ /^\d+\.\d+\.\d+\.\d+$/) {
	    $m = "$m ($ecs{$m})" if $ecs{$m};
	    $annotations{$acc}{EC}{$m} ++;
	} else {
	    $annotations{$acc}{RXN}{$m} ++;
	}
    }
}
close $afh;



mkdir $outdir unless -d $outdir;
my $outcyc = "$outdir/reference_enzymes_3_1";
open(my $fastafh, '>', "$outcyc.fasta") or die $!;


# read fasta file
#
open(my $fafile, '<', $fa) or die $!;
while (<$fafile>) {
    next if (/^#/ || /^$/);
    if (/^>(.*)$/) {
	my $sid = $1;
	my $cid = (split /\s+|\|/, $sid)[0];
	if ($annotations{$cid}) {
	    my $reactions = join "; ", sort keys %{$annotations{$cid}{RXN}}; 
	    my $ecnumbers = join "; ", sort keys %{$annotations{$cid}{EC}}; 
	    $_ = ">$cid | MetaCyc: $reactions | EC: $ecnumbers\n";
	}
    }
    
    print $fastafh $_;
}
close $fafile;

close $fastafh;

if (-s "$outcyc.fasta") {
    system "makeblastdb -dbtype prot -in $outcyc.fasta -out $outcyc";
}
