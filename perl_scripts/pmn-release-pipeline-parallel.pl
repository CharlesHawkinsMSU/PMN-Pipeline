#!/usr/bin/perl

my $usage = <<USAGE;

###########################################################
# PMN release pipeline of lisp and perlcyc commands       #
# Charles Hawkins, Chuan Wang, Peifen Zhang @ Rhee-lab    #
# Department of Plant Biology                             #
# Carnegie Institution for Science                        #
# Date: Feb 26, 2024                                      #
#                                                         #
# Input: the master file of commands in each step         #
# Output: logfile of the ptools lisp and perlcyc commands #
###########################################################

  Usage: $0 <master-file-of-commands> [output-logfile]

USAGE

use strict;
use warnings;
use File::Basename;
use Term::ANSIColor;

use FindBin;
#use lib "$FindBin::Bin/lib_perl5.16";
use IO::Pty::Easy;
use IO::Handle;
use perlcyc;    # not really used in the main pipeline script, but to check the availability of perlcyc module
use IO::Socket;
use Env;

# Check arguments
if ($#ARGV<1) {
	die $usage;
}

$|=1;   # Output prior
my $cmdline = "$0 ".join(' ',@ARGV);
my $stime = time;
my $date = &timeformat($stime);
#$SIG{'INT'} = \&intHandler;     # handle Control-C signal

# output handle globally, also used by sub-routines
my $fh = *STDOUT;

if ($ARGV[1]) {
    open OUT,">$ARGV[1]";
    $fh = *OUT;
}

my $pt_sock_path = $ENV{'PTOOLS-ACCESS-SOCKET'} || "/tmp/ptools-socket";

my @orglist = split(/,/, $ENV{'PMN_ORGLIST'});

message("# Cmd: ".$cmdline);
message("# Started: ".$date."\n");

# read in commands from file
my @cmd;
my @type;
my @rownum;
my %args;
my $rnum = 0;
my $ptoolsapi = '';
my $dir = dirname($ARGV[0]);

open(CMD, "<$ARGV[0]") or die "ERROR: Cannot open file '$ARGV[0]'.\n";

while (<CMD>) {
        $rnum ++;
        next if (/^#/ || /^\s*$/);
        chomp;
        my @r = split /\t/;
        
        if ($r[0] =~ /^lisp|perl|sh/) {
                push @cmd, $r[1];
                push @type, $r[0];
                push @rownum, $rnum;
		$ptoolsapi = ' -api' if $r[0] =~ /perl/;
        } else {
                $args{$r[0]} = $r[1];
        }
}

close CMD;

# prepare arguments: replace arguments in arguments with absolute values
$args{s_sp_folder} = "$dir/$args{s_sp_folder}" if $args{s_sp_folder}; # fix path of species folder
$args{s_cm_folder} = "$dir/$args{s_cm_folder}" if $args{s_cm_folder}; # fix path of common folder
$args{s_script_folder} = "$FindBin::Bin/$args{s_script_folder}" if $args{s_script_folder}; # fix path of scripts folder

foreach my $i (keys %args) {
        if ($i =~ /^fs_/) {
                if ($i =~ /^fs_new_/) {
                        $args{$i} =~ s/s_pgdb/$args{s_pgdb}/g;
                        $args{$i} =~ s/s_start_timestamp/$args{s_start_timestamp}/g;
                        $args{$i} =~ s/s_end_timestamp/$args{s_end_timestamp}/g;
                }
                
                my $f = $i;
                $f =~ s/^fs_/f_/;
                $args{$f} = $args{$i};
        }
}

# check if there is pathway-tools already running
#if (length $ptoolsapi > 0) {
#	my $ptools = `ps aux | grep 'pathway-tools' | grep ' -api' | grep -v 'grep' | wc -l`;
#	
#	if ($ptools > 0) {
#		message("IMPORTANT: Another pathway-tools instance is running, please finish all existing pathway-tools jobs and re-run this pipeline to ensure only one API socket is running.");
#		exit(1);
#	}
#}

# use spawn to open a terminal running ptools
#my $pty = IO::Pty::Easy->new;
#$pty->spawn("$args{s_ptools} -lisp$ptoolsapi");

# index of lisp and perlcyc commands
my $sql = 0;

# wait for output of ptools initiation
#my $error = wait_for_lisp($pty);

# if changes are saved to PGDB
my $saved = 0;


my $error = "";

my $pt_sock;

# do tasks in the opened terminal
while (1) {
        
        # run commands by type of lisp or perl/sh
        if ($type[$sql] eq 'lisp') {
                
                # prepare arguments: replace strings in command
                foreach my $i (sort {length($b) <=> length($a)} keys %args) {
                        next if ($i =~ /^fs_/);
                        $cmd[$sql] =~ s/$i/$args{$i}/g;
                }
                
                print $fh "$cmd[$sql]\n";
                message("Excuting $type[$sql] command: $cmd[$sql]...");
                
                # replace fs_* strings just before the lisp command
                my @fs_matches = $cmd[$sql] =~ /(fs_[A-Za-z_]+)/g;
                foreach my $i (sort {length($b) <=> length($a)} @fs_matches) {
                        my $content = `cat $args{s_sp_folder}/$args{$i}`;
                        $cmd[$sql] =~ s/$i/$content/g;
                }
                
                $pt_sock = IO::Socket::UNIX->new(Type => IO::Socket::SOCK_STREAM, Blocking => 1, Peer => $pt_sock_path) || die "Cannot open connection to $pt_sock_path";
                $pt_sock->send("$cmd[$sql]\n");
                my $buf;
                $pt_sock->recv($buf, 1024);
                $pt_sock->close();
                #$error = wait_for_lisp($buf);
        } else {
                # perl or sh
                
                # prepare arguments: replace strings in command
                foreach my $i (sort {length($b) <=> length($a)} keys %args) {
                        $cmd[$sql] =~ s/$i/$args{$i}/g;
                }
                
                print $fh "\n";
                message("Excuting $type[$sql] command: $cmd[$sql]...");
                system "$type[$sql] $cmd[$sql] 2> $ARGV[1]_perlcyc_$rownum[$sql] ";
               
                #$error = wait_for_perlcyc($pty, $cmd[$sql]);
                print $fh `cat $ARGV[1]_perlcyc_$rownum[$sql]`;
                
                # get back the ptools lisp prompt
                print $fh "\n";
                #$pty->write("\n");
                #one_line_lisp($pty);        
        }
       
        # determine if all commands are done or stuck with error
        message($#cmd);
        if ($sql < $#cmd && length $error < 1) {
        #if ($sql < $#cmd) {
                $sql ++;
        } else {
                
                # command end time
                my $etime = time;
                $date = &timeformat($etime);
                message("# Ended: ".$date." [". ($etime - $stime) ." seconds]\n");

=open gui                
                # no lisp error detected
                if (length $error < 1) {
                                
                        # open GUI
                        question("IMPORTANT: Press enter to open GUI and do manual check...");
                        my $in = <STDIN>;
                        #if ($in =~ /^y/i) {
                                message("Opening the GUI...");
                                print $fh "(eco)\n";
                                $pty->write("(eco)\n");
                                wait_for_lisp($pty);
                        #}
                        
                        # save the PGDB
                        print STDERR "\n";
                        message("IMPORTANT: Please check the log file to see if there is any critical errors.");
                        my $sure = 0;
                        
                        while (not $saved and not $sure) {
                                question("Do you want to save the changes to the PGDB (y/n)?");
                                my $in2 = <STDIN>;
                                if ($in2 =~ /^y/i) {
                                        message("Saving PGDB...");
                                        print $fh "(save-kb)\n";
                                        $pty->write("(save-kb)\n");
                                        wait_for_lisp($pty);
                                        $saved = 1;
                                } else {
                                        print STDERR "\n";
                                        question("IMPORTANT: Are you sure you want to exit WITHOUT saving the PGDB (y/n)?");
                                        my $in3 = <STDIN>;
                                        if ($in3 =~ /^y/i) {
                                                $sure = 1;
                                        }
                                }
                        }
                        
                }

=cut

                # exit ptools
                #print $fh "(exit)\n";
                #message("Exiting ptools...");
                #$pty->write("(exit)\n");
                #one_line_lisp($pty);        
                
                last;
        }
}

# close the terminal which run ptools
#$pty->close;

# endding message
if (length $error > 0) {
        message("IMPORTANT: Tasks stopped with error at command ".($sql+1)." (master file '$ARGV[0]' rownum: $rownum[$sql]):\n-----\n$type[$sql]: $cmd[$sql]\n-----");
        print $fh $error, "\n";
        print STDERR $error, "\n" if $ARGV[1];
} elsif ($saved) {
        message("Tasks done successfully ($ARGV[0]).");
} else {
        print STDERR "\n";
        message("Tasks done successfully (through the end of master file '$ARGV[0]').");
}


########## sub-routines ##########
sub wait_for_lisp {
        # get all output and look for the lisp prompt to get ready
        my ($pty) = @_;
        my $o = $pty->read(1);
        my $log = '';
        
        while (1) {
                print $fh $o ? $o : '';
		$fh->flush;
		$fh->sync;
                $log .= $o if ($o && ($o =~ /^Error/i || length $log > 0));
                
                if ($o && $o =~ /EC\(\d+\):/) {
                        last;
                }
                
                $o = $pty->read(1);
        }
        
        return $log;
}


sub one_line_lisp {
        # get all output in one second, for exiting ptools
        my ($pty) = @_;
        my $o = $pty->read(1);
        print $fh $o ? $o : '';
	$fh->flush;
	$fh->sync;
}


sub wait_for_perlcyc {
        # get all output of perl, timeout 2 seconds after the perl process has finished
        my ($pty, $p) = @_;
        my $time = 0;
        my $o = $pty->read(1);
        my $log = '';
        
        while (1) {
                $time = 0 if ($o);
                print $fh $o ? $o : '';
		$fh->flush;
		$fh->sync;
                #$log .= $o if ($o && ($o =~ /Error/i || length $log > 0));
                my $ps = `ps x | grep '$p' | grep -v 'grep' | wc -l`;
                
                if (length $ps < 1 || ($ps < 1 && $time > 2) ) {
                        last;
                }
                
                $o = $pty->read(1);
                $time ++;
        }
        
        return $log;
}


sub message {
        # print message to STDERR with green tag
        my ($str) = @_;
        print $fh color("green"), "[[MAIN]] ", color("reset"), $str, "\n";
        print STDERR color("green"), "[[MAIN]] ", color("reset"), $str, "\n" if $ARGV[1];
}


sub question {
        # print question to STDERR with green tag
        my ($str) = @_;
        print $fh color("green"), "[[MAIN]] ", color("reset"), $str, " ";
        print STDERR color("green"), "[[MAIN]] ", color("reset"), $str, " " if $ARGV[1];
}


sub timeformat {
        # print formatted time stamp
        my $time = shift;
        return sprintf "%d-%02d-%02d %02d:%02d:%02d", map { $$_[5]+1900, $$_[4]+1, $$_[3], $$_[2], $$_[1], $$_[0] } [localtime($time)];
}


#sub intHandler {
#        # handle Control-C signal for clean exit of ptools
#        print STDERR "\n";
#        message("IMPORTANT: Interrupted by user (Control-C signal detected).");
#        &closeAll;
#        exit(1);
#}
#
#
#sub closeAll {
#        # kill all child processes
#        local $SIG{HUP} = 'IGNORE';
#        kill HUP => -$$;
#        
#        # kill ptools
#        $pty->kill;
#        $pty->close;
#}
