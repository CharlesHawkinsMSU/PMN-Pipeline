#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014
# Last updated by Charles Hawkins, Apr 2022

# Description: Prepare data file folders for generating PGDBs by ptools
# Input: table of basic info, pf file folder, prepared folder, ptools executable full path
# Output: folders of files for each species listed in the input table file

my $usage = <<USAGE;

  Usage: $0 <input-table-file> <pf-file-folder> <output-folder> <ptools-full-path>

USAGE

use strict;
use Cwd;
use File::Spec;
use File::Basename;
use lib dirname(__FILE__);
use pmn;

# Check arguments
if ($#ARGV<0) {
	die $usage;
}

# Extract out arguments into variables
my $table_file = $ARGV[0];
my $pf_dir = $ARGV[1];
my $masters_dir = $ARGV[2];
my $ptools = $ARGV[3];
my @orglist = split(/,/, $ARGV[4]);

$|=1;   # Output prior


$pf_dir =~ s!/$!!;
$masters_dir =~ s!/$!!;
mkdir $masters_dir;

# read input table file
my %pgdb_table = pmn::read_pgdb_table($table_file);

# for each species

if (!@orglist)
{
    @orglist = keys %pgdb_table;
}

foreach my $species (@orglist) {
    
	# Extract the entry data into variables
	my $entry = $pgdb_table{$species};
	my $pf_file = $$entry{"PF File"};
	my $org_counter = counter_from_id($$entry{"Unique ID"});
	my $id_name = $$entry{"ID/Name"};
	my $sp_name = $$entry{"Species Name"};
	my $db_name = $$entry{"Database Name"};
	my $ab_name = $$entry{"Abbrev Name"};
	my $seq_file = $$entry{"Sequence File"};
	my $taxid = $$entry{"NCBI Taxon ID"};
	my $pgdb_version = $$entry{"Version"};
	my $homepage = $$entry{"Homepage"};
	my $email = $$entry{"Email"};

    # create folder for species $species
    mkdir "$masters_dir/$species";
    
    # copy pf file to the folder
    `cp $pf_dir/$pf_file $masters_dir/$species/$pf_file`;
    print STDERR "Preparing for $species (PGDB-UNIQUE-ID: $org_counter)...\n";

    # write create.master file
    open(my $cf, '>', "$masters_dir/$species/create.master") or die $!;
    print $cf $ptools;
    close $cf;

    # write genetic-elements.dat file
    open(GE, ">$masters_dir/$species/genetic-elements.dat") or die "Cannot open file $masters_dir/$species/genetic-elements.dat to write.\n";
    print GE "ID\t$id_name\nNAME\t$id_name\nANNOT-FILE\t$pf_file\n//\n";
    close GE;
    
    # write organism-params.dat file
    open(OP, ">$masters_dir/$species/organism-params.dat") or die "Cannot open file $masters_dir/$species/organism-params.dat to write.\n";
    print OP "ID\t$species\nSTORAGE\tFILE\nNAME\t$sp_name\nABBREV-NAME\t$ab_name\nSEQUENCE-SOURCE\t$seq_file\nDOMAIN\t$taxid\nRANK\t|species|\nCODON-TABLE\t1\nMITO-CODON-TABLE\t1\nHOMEPAGE\t$homepage\nEMAIL\t$email\nDBNAME\t$db_name\nNCBI-TAXON-ID\t$taxid\nREF-ORGID\tPLANT\nORG-COUNTER\t$org_counter\n";
    close OP;
     
    # write dump.master foreach species
    open(DP, ">$masters_dir/$species/dump.master") or die "Cannot open file $masters_dir/$species/dump.master to write.\n";
    print DP "s_ptools\t$ptools\nlisp\t(so '$species)\nlisp\t(setq *file-export-progress* nil)\nlisp\t(dump-frames-to-attribute-value-files (org-data-dir))\nlisp\t(dump-frames-to-tabular-files (org-data-dir))\n";
    close DP;
    
    # write dump-biopax.master foreach species
    open(DP, ">$masters_dir/$species/dump-biopax.master") or die "Cannot open file $masters_dir/$species/dump-biopax.master to write.\n";
    print DP "s_ptools\t$ptools\nlisp\t(so '$species)\nlisp\t(setq *file-export-progress* nil)\nlisp\t(com-export-pgdb-to-biopax)";
    close DP;
    
    # write checker.master foreach species
    open(CH, ">$masters_dir/$species/checker.master") or die "Cannot open file $masters_dir/$species/checker.master to write.\n";
    print CH "s_ptools\t$ptools\nlisp\t(so '$species)\nlisp\t(run-all-checks)\nlisp\t(save-kb)\n";
    close CH;
    
    # write newversion.master foreach species
    if ($pgdb_version) {
	open(NV, ">$masters_dir/$species/newversion.master") or die "Cannot open file $masters_dir/$species/newversion.master to write.\n";
	print NV "s_ptools\t$ptools\nlisp\t(so '$species)\nlisp\t(create-new-pgdb-version (current-kb) \"$pgdb_version\")\n";
	close NV;
    }
}

#######
sub counter_from_id {
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
