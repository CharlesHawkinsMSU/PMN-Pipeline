#!/usr/bin/perl

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Jul., 2014

# Description: Prepare data file folders for generating PGDBs by ptools
# Input: table of basic info, pf file folder, prepared folder, ptools executable full path
# Output: folders of files for each species listed in the input table file

my $usage = <<USAGE;

  Usage: $0 <input-table-file> <pf-file-folder> <output-folder> <ptools-full-path> <pgdb-full-path> <savi-output-folder> <release-pipeline-common-folder> <fasta-file-folder>

USAGE

print "hello, yes this is refine-prepare\n";
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

my $username = $ENV{LOGNAME} || $ENV{USER} || getpwuid($<);

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
    
    # write dump-allflat.master foreach species
    open(DP, ">$ARGV[2]/$s/dump-allflat.master") or die "Cannot open file $ARGV[2]/$s/dump-allflat.master to write.\n";
    print DP "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(bulk-write-flat-files '($s))";
    close DP;
     
    # write checker.master foreach species
    open(CH, ">$ARGV[2]/$s/checker.master") or die "Cannot open file $ARGV[2]/$s/checker.master to write.\n";
    print CH "s_ptools\t$ARGV[3]\nlisp\t(so '$s)\nlisp\t(run-all-checks)\nlisp\t(save-kb)\n";
    close CH;
    
    # write fix-curator-error.master foreach species
    open(FC, ">$ARGV[2]/$s/fix-curator-error.master") or die "Cannot open file $ARGV[2]/$s/fix-curator-error.master to write.\n";
    print FC "s_ptools\t$ARGV[3]\nlisp\t(so 'plant)\nlisp\t(so '$s)\nlisp\t(import-frames :frames '(|$username|) :src-kb (find-org 'plant) :dst-kb (find-org '$s) )\nlisp\t(let* ((login-id '|$username|) (keep-frame '|$username|) (all-the-frames (find-all login-id (get-class-all-instances '|People|) :key #'(lambda (au-frame) (get-slot-value au-frame 'LOGIN-ACCOUNT) ) ) )) (loop for f in (fremove keep-frame all-the-frames) do (merge-author-frames f keep-frame) ))\nlisp\t(save-kb)\n";
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
    
    # write refine-a.master foreach species
    my ($s_ptools, $s_pgdb, $s_sp_name, $s_sp_folder, $s_start_timestamp, $s_end_timestamp, $s_seq_source, $s_curator, $s_savi_citation, $s_e2p2_citation, $s_enzrxn_citation, $f_enz_name, $f_enz_name_meta, $f_meta_link, $f_plant_link) = ($ARGV[3], $s, $data{$s}[2], $s, @{$data{$s}}[10..20]);
   if (! defined $s_start_timestamp || $s_start_timestamp eq "") {
        $s_start_timestamp = "0";
    }
    if (! defined $s_start_timestamp || $s_end_timestamp eq "") {
        $s_end_timestamp = "99999999999";
    }
   my $temp = <<TEMP;

# Master file for PMN release pipeline
# Provides all the data needed for release the PGDB of a species
#
# Chuan Wang and Peifen Zhang @ Rhee-lab
# Department of Plant Biology
# Carnegie Institution for Science
# Date: June 19, 2014

# Columns: type, value

# species parameters
#
# s_* are string values
# fs_* are files that contain string values to be passed to lisp commands
# f_* are files that will be used by scripts, only whose name will be passed to commands

s_ptools	$s_ptools
s_pgdb	$s_pgdb
s_sp_name	"$s_sp_name"
s_sp_folder	$s_sp_folder
# set s_start_timestamp to 0 and s_end_timestamp to current timestamp for new PGDB
s_start_timestamp	$s_start_timestamp
s_end_timestamp	$s_end_timestamp
s_seq_source	$s_seq_source
s_seq_source_acc	accession-1

s_cm_folder	common
s_script_folder	perl_scripts
fs_pwy_del	remove
fs_pwy_add	ic

# other parameters
#
# NOTE: Citations are propagated from aracyc to the destination s_pgdb, so please create citations first in source PGDB 'ara.

s_curator	$s_curator
#s_savi_citation	|PUB-PMNUPP2013| |PUB-PMNAIPP2013| |PUB-PMNRXN2013| |PUB-PMNRXNTAXON2013| |PUB-PMNTAXON2013| |PUB-PMNIC2013| |PUB-PMNSP2013|
s_savi_citation	$s_savi_citation
s_e2p2_citation	$s_e2p2_citation
s_enzrxn_citation	$s_enzrxn_citation

f_savi_comment	savi-comment
f_enz_name	$f_enz_name
f_enz_name_meta	$f_enz_name_meta
f_meta_link	$f_meta_link
f_plant_link	$f_plant_link

fs_new_proteins	s_pgdb-s_start_timestamp-s_end_timestamp-new-proteins
fs_new_enzrxns	s_pgdb-s_start_timestamp-s_end_timestamp-new-enzrxns


# lisp and perlcyc commands
#
# delete invalid pathways
lisp	(so 's_pgdb)	select organism
#lisp	(loop for p in '(fs_pwy_del) do (delete-frame-and-dependents p))
lisp	(loop for p in '(fs_pwy_del) do (handler-case (delete-frame-and-dependents p) (not-coercible-to-frame () (format t "Frame not found during deletion of ~A~%" p))))

# import pathways from metacyc inferred by curator (this function does not import enzymes with the pathway)
lisp	(so 'meta)
lisp	(so 's_pgdb)
lisp	(import-pathways '(fs_pwy_add) (find-kb 'metabase) (current-kb))
#lisp	(loop for pwy in '(fs_pwy_add) do (handler-case (import-pathways '(pwy) (find-kb 'metabase) (current-kb)) (not-coercible-to-frame () NIL)))

# add curator |pmngroup| to PGDB
lisp	(so 'ara)
lisp	(so 's_pgdb)
lisp	(import-frames :frames '(s_curator) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# add PMN citations about the latest SAVI pipeline
lisp	(import-frames :frames '(s_savi_citation) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# add PMN citation about the latest E2P2 pipeline
lisp	(import-frames :frames '(s_e2p2_citation) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# Edit 07/06/2017: Create external DBs in pgdbs
lisp	(so 'ara)
lisp	(so 's_pgdb)
lisp	(import-frames :frames '(|PHYTOZOME|) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))
lisp	(import-frames :frames '(|MAIZEGDB|) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))
lisp	(import-frames :frames '(|ENSEMBL-PROTEIN|) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# Edit 07/06/2017: Update Author List:
lisp	(so 'ara)
lisp	(so 's_pgdb)
lisp	(import-frames :frames '(|wang| |schlapfer| |rhee| |kim| |xue|) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# get newly added proteins and enzrxns within the time range
perl	s_script_folder/get_new_proteins_enzrxns_within_timestamp_range.pl s_pgdb s_start_timestamp s_end_timestamp s_sp_folder

# add PMN credits “created by |pmngroup|” to protein and enzrxn
# depends on the files generated by last step
lisp	(loop for p in '(fs_new_proteins) unless (slot-has-value-p p 'Credits) do (add-credit-event p (make-credit-event 'created '(s_curator))) )
lisp	(loop for p in '(fs_new_enzrxns) unless (slot-has-value-p p 'Credits) do (add-credit-event p (make-credit-event 'created '(s_curator))) )

# add/reload SAVI citation:EV and generic comments to pathways
# BTW, update metacyc comments, keep other comments as is
perl	s_script_folder/savi_citations_and_comments.pl s_pgdb s_sp_name s_sp_folder s_cm_folder/f_savi_comment

# add/reload E2P2 citation:EV-COMP-AINF to enzrxns
perl	s_script_folder/e2p2_enzrxn_citations.pl s_pgdb s_enzrxn_citation s_start_timestamp s_end_timestamp

# add/reload enzyme common name to enzrxns
perl	s_script_folder/add_name_enzrxn_timestamp.pl s_pgdb s_cm_folder/f_enz_name s_cm_folder/f_enz_name_meta s_start_timestamp s_end_timestamp

# add/reload sequence source (i.e. phytozome) DBLink to genes
perl	s_script_folder/reload_dblinks_phytzome_protein.pl s_pgdb s_seq_source s_seq_source_acc

# add/reload metacyc and plantcyc DBLink to pathways
perl	s_script_folder/reloadPWYlinks2metacyc.plantcyc.pl s_pgdb s_cm_folder/f_meta_link s_cm_folder/f_plant_link

# select the current PGDB
lisp	(so 's_pgdb)
# save the kb
lisp	(save-kb)

TEMP

    open(my $refineafh, '>', "$ARGV[2]/$s/refine-a.master") or die $!;
    print $refineafh $temp;
    close $refineafh;
    
    mkdir "$ARGV[2]/$s/$s";
    # my $cpcmd = "cp $ARGV[5]/" . lc($data{$s}[1]) . "/* $ARGV[2]/$s/$s/";
    my $cpcmd = "cp $ARGV[5]/" . $data{$s}[0] . "/* $ARGV[2]/$s/$s/";
    # print $cpcmd;
    system $cpcmd;
    foreach my $f (`ls $ARGV[2]/$s/$s/`) {
	# convert savi output files to Unix-based line endings
	chomp $f;
	$f =~ s/\.txt$//;
	`tr -d '\r' < $ARGV[2]/$s/$s/$f.txt > $ARGV[2]/$s/$s/$f`;
    }
    mkdir "$ARGV[2]/$s/common";
    $cpcmd = "cp $ARGV[6]/* $ARGV[2]/$s/common/";
    system $cpcmd;


    # write blastset.master foreach species
    my $outdir = File::Spec->rel2abs("$ARGV[2]-blastsets");
    $outdir =~ s/^\/carnegie\/data/\/home\/$username/;
    open(BS, ">$ARGV[2]/$s/blastset.master") or die "Cannot open file $ARGV[2]/$s/blastset.master to write.\n";
    print BS "s_ptools\t$ARGV[3]\ns_script_folder\tperl_scripts\nperl\ts_script_folder/create-blast-dataset-pgdb.pl $s \"$data{$s}[9]\" \"$data{$s}[2]\" \"$s_seq_source\" $ARGV[7]/$data{$s}[6] $outdir\n";
    close BS;

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
