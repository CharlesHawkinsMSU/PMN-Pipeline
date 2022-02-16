#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014

# Description: Prepare data file folders for generating PGDBs by ptools
# Input: table of basic info, pf file folder, prepared folder, ptools executable full path
# Output: folders of files for each species listed in the input table file

my $usage = <<USAGE;

  Usage: $0 <input-table-file> <pf-file-folder> <output-folder> <ptools-full-path> <pgdb-full-path>

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
mkdir $ARGV[2];

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
    
    # create folder for species $s
    mkdir "$ARGV[2]/$s";
    
    # copy pf file to the folder
    `cp $ARGV[1]/$data{$s}[7] $ARGV[2]/$s/`;
    $data{$s}[8] = convert($data{$s}[8]);
    print STDERR "Preparing for $s (PGDB-UNIQUE-ID: $data{$s}[8])...\n";

    # write create.master file
    open(my $cf, '>', "$ARGV[2]/$s/create.master") or die $!;
    print $cf $ARGV[3];
    close $cf;

    # write genetic-elements.dat file
    open(GE, ">$ARGV[2]/$s/genetic-elements.dat") or die "Cannot open file $ARGV[2]/$s/genetic-elements.dat to write.\n";
    print GE "ID\t$data{$s}[3]\nNAME\t$data{$s}[3]\nANNOT-FILE\t$data{$s}[7]\n//\n";
    close GE;
    
    # write organism-params.dat file
    open(OP, ">$ARGV[2]/$s/organism-params.dat") or die "Cannot open file $ARGV[2]/$s/organism-params.dat to write.\n";
    print OP "ID\t$s\nSTORAGE\tFILE\nNAME\t$data{$s}[2]\nABBREV-NAME\t$data{$s}[4]\nSEQUENCE-SOURCE\t$data{$s}[6]\nDOMAIN\t$data{$s}[5]\nRANK\t|species|\nCODON-TABLE\t1\nMITO-CODON-TABLE\t1\nHOMEPAGE\twww.plantcyc.org\nEMAIL\tcurator\@plantcyc.org\nDBNAME\t$data{$s}[1]\nNCBI-TAXON-ID\t$data{$s}[5]\nREF-ORGID\tPLANT\nORG-COUNTER\t$data{$s}[8]\n";
    close OP;
     
    # write dump.master foreach species
    open(DP, ">$ARGV[2]/$s/dump.master") or die "Cannot open file $ARGV[2]/$s/dump.master to write.\n";
    print DP "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(setq *file-export-progress* nil)\nlisp\t(dump-frames-to-attribute-value-files (org-data-dir))\nlisp\t(dump-frames-to-tabular-files (org-data-dir))\n";
    close DP;
    
    # write dump-biopax.master foreach species
    open(DP, ">$ARGV[2]/$s/dump-biopax.master") or die "Cannot open file $ARGV[2]/$s/dump-biopax.master to write.\n";
    print DP "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(setq *file-export-progress* nil)\nlisp\t(com-export-pgdb-to-biopax)";
    close DP;
    
    # write checker.master foreach species
    open(CH, ">$ARGV[2]/$s/checker.master") or die "Cannot open file $ARGV[2]/$s/checker.master to write.\n";
    print CH "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(run-all-checks)\nlisp\t(save-kb)\n";
    close CH;
    
    # write fix-curator-error.master foreach species
    open(FC, ">$ARGV[2]/$s/fix-curator-error.master") or die "Cannot open file $ARGV[2]/$s/fix-curator-error.master to write.\n";
    print FC "s_ptools\t$ARGV[3]\nlisp\t(so 'plant)\nlisp\t(so '$s)\nlisp\t(import-frames :frames '(|wang|) :src-kb (find-org 'plant) :dst-kb (find-org '$s) )\nlisp\t(let* ((login-id '|cwang|) (keep-frame '|wang|) (all-the-frames (find-all login-id (get-class-all-instances '|People|) :key #'(lambda (au-frame) (get-slot-value au-frame 'LOGIN-ACCOUNT) ) ) )) (loop for f in (fremove keep-frame all-the-frames) do (merge-author-frames f keep-frame) ))\nlisp\t(save-kb)\n";
    close FC;
    
    # write overview.master foreach species
    open(OV, ">$ARGV[2]/$s/overview.master") or die "Cannot open file $ARGV[2]/$s/overview.master to write.\n";
    print OV "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(update-overview :save? t :batch-mode? t :show-progress? nil)\nlisp\t(save-kb)\n";
    close OV;
    
    # write newversion.master foreach species
    if ($data{$s}[9]) {
	open(NV, ">$ARGV[2]/$s/newversion.master") or die "Cannot open file $ARGV[2]/$s/newversion.master to write.\n";
	print NV "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(create-new-pgdb-version (current-kb) \"$data{$s}[9]\")\n";
	close NV;
    }
    
    # write fix.master foreach species
    open(FX, ">$ARGV[2]/$s/fix.master") or die "Cannot open file $ARGV[2]/$s/fix.master to write.\n";
    print FX "s_ptools\t$ARGV[3]\ns_script_folder\tperl_scripts\nperl\ts_script_folder/pf_rxn_with_enz_pair_20140731.pl $ARGV[2]/$s/$data{$s}[7] s_script_folder/metacyc-18.5-all-RXN-EC.mapping s_script_folder/metacyc-18.5-EC-superseded s_script_folder/sub-rxn-list.txt | sort -u > $ARGV[2]/$s/$s.pf-rxn-with-enz-pair.tab\nperl\ts_script_folder/enzrxn-dat-pair-detail.pl '$ARGV[4]/".lc($data{$s}[1])."/1.0/data/enzrxns.dat' '$ARGV[4]/".lc($data{$s}[1])."/1.0/data/genes.dat' '$ARGV[4]/".lc($data{$s}[1])."/1.0/data/proteins.dat' | sort -u > '$ARGV[2]/$s/$s.dat-enz-rxn-pair.tab'\nbash -c\t\"comm -23 $ARGV[2]/$s/$s.pf-rxn-with-enz-pair.tab $ARGV[2]/$s/$s.dat-enz-rxn-pair.tab > $ARGV[2]/$s/$s.pf.only\"\nlisp\t(so '$s)\nperl\ts_script_folder/add_enzrxn_to_pgdb-20140916.pl $s $ARGV[2]/$s/$s.pf.only s_script_folder/metacyc-18.5-all-RXN-EC.mapping\nlisp\t(pathologic-rescore-pathways :mode :batch)\nlisp\t(save-kb)\n";
    close FX;
}

#######
sub convert {
    my $l = shift;
    my @d = split //, $l;
    my $sum = 0;
    foreach my $i (0..$#d) {
	my $c = $d[-($i+1)];
	if ($c =~ /[A-Z]/) {
	    $c = ord($c) - ord('A') + 10;
	}
	$sum += 36**$i * $c;
    }
    return $sum;
}
