#!/usr/bin/perl -w

# Chuan Wang @ Rhee-lab, Department of Plant Biology, Carnegie Institution for Science
# Date: Apr., 2015

# Description: run a command on clusters.
# Input: a sample of job script, command
# Output: the same with the command

my $usage = <<USAGE;

  Usage: $0 <cpu> <command>
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
my $cmd = join ' ', @ARGV[1..$#ARGV];
my $cmdline = "$0 ". join(' ', @ARGV);

print STDERR "\nStarts at [$time]\nCmd: $cmdline\n\n";

if (@ARGV<2) {
    die $usage;
}

my $pty = IO::Pty::Easy->new;
my $cwd = `pwd`;
chomp $cwd;
my $srun = "srun -N 1 -n $ARGV[0] --pty bash -c \"cd '$cwd' && $cmd\"";
print STDERR $srun, "\n";
$pty->spawn($srun);
open(OUT, ">hpc-$$.log") or die $!;
while ($pty->is_active) {
    my $o = $pty->read(1);
    print OUT $o ? $o : '';
    print STDERR $o ? $o : '';
    OUT->flush;
    OUT->sync;
    if ($o && $o =~ /error|done/i) {
        last;
    }
}
print STDERR "Done.\n";
$pty->close;

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
        $pty->kill;
        $pty->close;
}
