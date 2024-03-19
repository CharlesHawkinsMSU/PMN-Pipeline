#!/usr/bin/perl

my $usage = <<USAGE;

This perlcyc script is part of

###########################################################
# PMN release pipeline of lisp and perlcyc commands       #
# Chuan Wang, Peifen Zhang @ Rhee-lab                     #
# Department of Plant Biology                             #
# Carnegie Institution for Science                        #
# Date: July 1, 2014                                      #
#                                                         #
# Input: the master file of commands in each step         #
# Output: logfile of the ptools lisp and perlcyc commands #
###########################################################

  Usage: $0 <pgdb> <species-name> <species-folder> <savi-comment-file> [<reference database>]

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;
use Env;


# process arguments
#
if (@ARGV < 4) {
    die $usage;
}
elsif (@ARGV == 4) {
    my ($DB, $species, $folder, $file) = @ARGV;
    my $refcyc = 'meta';
    my $ucrefcyc = 'Meta';
}
else {
    my ($DB, $species, $folder, $file, $refcyc) = @ARGV;
    my $ucrefcyc = ucfirst($refcyc);
}
$species =~ s/_/ /;
$folder =~ s!/$!!;

my %savi;
my %savi_pwy;

open(IN, "<$file") or die "The savi-coment-file $file couldn't be opend.\n";

while (<IN>) {
    next if (/^#/ || /^\s*$/);
    chomp;
    my @r = split /\t/;
    
    ## convert savi output files to Unix-based line endings
    #`mv $folder/$r[0] $folder/$r[0].bak`;
    #`tr -d '\r' < $folder/$r[0].bak > $folder/$r[0]`;
    
    # read savi input files
    open SAV, "<$folder/$r[0]" || die "The File $r[0] Could Not Be Found.\n";
    
    while (<SAV>) {
	chomp;
	$savi_pwy{$_} = $r[0];
    }
    
    close SAV;
    
    $r[2] =~ s/\$species/$species/g;
    $savi{$r[0]}{comment} = $r[2];
    $savi{$r[0]}{citation} = $r[1];
}

close IN;


# read refcyc comments
#
my $cyc = perlcyc -> new($refcyc);

$cyc->{'_socket_name'} = $ENV{'PTOOLS-ACCESS-SOCKET'} || '/tmp/ptools-socket';

my %refcyc_comment;
my @ref_pwy= $cyc -> all_pathways ();

foreach my $ref_pwy (@ref_pwy) {
    $refcyc_comment{$ref_pwy} = $cyc -> get_multiline_slot_value($ref_pwy, "comment");
}

$cyc -> close;


# current PGDB
#
$cyc = perlcyc -> new($DB);

my @old_pwy= $cyc -> all_pathways();

foreach my $pwy (@old_pwy) {
    # read old info
    my $comment = $cyc -> get_multiline_slot_value($pwy, "comment");
    my @citations = $cyc -> get_slot_values($pwy, "citations");
    my $comment_old = $comment;
    
    # update refcyc comment
    ## only update those with syntex "Summary from <Ref>Cyc:"
    ## and add refcyc comment if current comment is empty
    if ($comment =~ /Summary from [A-Za-z0-9_-]+Cyc:/) {
	if ($refcyc_comment{$pwy}) {
	    # replace
	    $comment =~ s/<i>Summary from [A-Za-z0-9_-]+Cyc:<\/i>.*/<i>Summary from ${ucrefcyc}Cyc:<\/i>\n\n$refcyc_comment{$pwy}/s;
	#} else {
	#    # delete
	#    $comment =~ s/\n*<i>Summary from MetaCyc:<\/i>.*//;
	}
    } elsif (length $comment < 1) {
        # add
        if ($refcyc_comment{$pwy}) {
             $comment = $refcyc_comment{$pwy};
        }
    }
    
    # pwy in one of savi pwy lists
    if (exists $savi_pwy{$pwy}) {
	
	my $replaced = 0;
	if (scalar @citations > 0) {
	    foreach my $cit (@citations){
		# new pathway without existing SAVI citations
		# or old savi citations that are not the same as the new one
		if ($cit =~ /AINF/ || $cit =~ /^PMN\w+:EV-COMP-HINF|PMN\w+:EV-IC/) {
		    unless ($cit =~ /^$savi{$savi_pwy{$pwy}}{citation}/) {
			$cyc -> replace_slot_value($pwy, "citations", "\"$cit\"", "\"$savi{$savi_pwy{$pwy}}{citation}\"");
			print STDERR "$pwy 's citation updated.\n";
		    }
		    
		    $replaced = 1;
		}
	    }
    	}
	
	if (scalar @citations < 1 || not $replaced) {
	    # add new savi citations
	    $cyc->add_slot_value($pwy, "citations", "\"$savi{$savi_pwy{$pwy}}{citation}\"");
	    print STDERR "$pwy 's citation added.\n";
	}
	
	# update savi pmn comment
	if ($comment =~ /^<i>Supporting evidence for this pathway/) {
	    # replace
	    $comment =~ s/^<i>Supporting evidence for this pathway.*\[more info\]<\/A> <\/B>/$savi{$savi_pwy{$pwy}}{comment}/;
	} else {
	    # add
	    ## other paragraphs to keep before refcyc summary
	    $comment =~ s/^/$savi{$savi_pwy{$pwy}}{comment}\n\n<i>Summary from ${ucrefcyc}Cyc:<\/i>\n\n/;
	}
    } else {
	# delete existing SAVI citations and comment
	# remove old savi citations
	foreach my $cit (@citations) {
	    if ($cit =~ /^PMN\w+:EV-COMP-HINF|PMN\w+:EV-IC/) {
		$cyc -> remove_slot_value_by_value($pwy, "citations", "\"$cit\"");
		print STDERR "$pwy 's citation removed.\n";
	    }
	}
	
	# remove old savi comment
	if ($comment =~ /^<i>Supporting evidence for this pathway/) {
	    # delete
	    $comment =~ s/^<i>Supporting evidence for this pathway.*\[more info\]<\/A> <\/B>\n\n<i>Summary from ${ucrefcyc}Cyc:<\/i>\n\n//;
	}
	
	# output to not covered list
	print STDERR "$pwy is not covered by current SAVI lists.\n";
    }
    
    # put back comment and citation values
    if ($comment ne $comment_old) {
	$cyc -> put_slot_value($pwy, "comment", "\"$comment\"");
	print STDERR "$pwy 's comment updated.\n";
    }
}
