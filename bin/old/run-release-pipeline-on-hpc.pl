#!/usr/bin/perl -w

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Apr., 2015

# Description: run pmn-release-pipeline on clusters.
# Input: pgdb-prepare-folder, master file of commands, number of concurrent jobs to run, whether use api
# Output: the same with the original command for each species

my $usage = <<USAGE;

  Usage: $0 <pgdb-prepare-folder> <master-file> <#jobs> [api]
USAGE

use strict;
use warnings;
use Cwd;
use File::Spec;
use File::Basename;
use File::Copy;
use Term::ANSIColor;

use FindBin;
#use lib "$FindBin::Bin/lib_perl5.16";
use IO::Pty::Easy;
use IO::Handle;
use Parallel::ForkManager;

$|=1;   # Output prior
my $path = dirname(File::Spec->rel2abs(__FILE__));  # Path of this script
my $time = sprintf "%d/%02d/%02d-%02d:%02d:%02d", map { $$_[5]+1900, $$_[4]+1, $$_[3], $$_[2], $$_[1], $$_[0] } [localtime];
my $cmdline = "$0 ".join(' ',@ARGV);

print STDERR "\nStarts at [$time]\nCmd: $cmdline\n\n";

if (@ARGV<2) {
    die $usage;
}

$ARGV[2] = 1 unless $ARGV[2];
#if ($ARGV[3]) {
#    $ARGV[3] = ' --ntasks-per-node=1';
#} else {
    $ARGV[3] = '';
#}

#system "salloc -N 4 -n 4";

# read status file
my ($fafolder) = map {s!/$!!; $_} @ARGV;

my $pm = new Parallel::ForkManager($ARGV[2]);
my @pty;
my $c = 0;

foreach my $fa (`ls $fafolder`) {
    #sleep 5 if $c;
    $c ++;
    my $pty = IO::Pty::Easy->new;
    push @pty, $pty;
    $pm->start && next;
    chomp $fa;
    my $saved = 0;
    print STDERR "[$fa] started.\n";
    $pty->spawn("srun -N 1 -n 1$ARGV[3] --pty perl $FindBin::Bin/../pmn-release-pipeline/pmn-release-pipeline-general.pl $fafolder/$fa/$ARGV[1] $fafolder/$fa/$ARGV[1].log
");
    open(OUT, ">$fafolder/$fa/$ARGV[1]-hpc.log") or die $!;
    while ($pty->is_active) {
        my $o = $pty->read(1);
        print OUT $o ? $o : '';
        OUT->flush;
        OUT->sync;
        if ($o && $o =~ /Error: /i) {
            print STDERR "[$fa] error!\n=====\n$o\n=====\n";
        }
        if ($o && $o =~ /EC\(\d+\):/) {
            #if (not $saved) {
            #    $pty->write("(save-kb)\n");
            #    $saved ++;
            #} else {
            #    $pty->write("(exit)\n");                
            #}
            last;
        }
    }
    print STDERR "[$fa] done.\n";
    $pty->close;
    $pm->finish;
}
$pm->wait_all_children;

print STDERR "All DONE!\n";

################
sub intHandler {
        # handle Control-C signal for clean exit of ptools
        print STDERR "\n";
        message("IMPORTANT: Interrupted by user (Control-C signal detected).");
        &closeAll;
        exit(1);
}


sub closeAll {
        # kill all child processes
        local $SIG{HUP} = 'IGNORE';
        kill HUP => -$$;
        
        # kill ptools
        foreach my $pty (@pty) {
            $pty->kill;
            $pty->close;
        }
}
