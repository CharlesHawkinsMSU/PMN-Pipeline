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

  Usage: $0 <pgdb> <species-name> <species-folder> <savi-comment-file>

USAGE

use FindBin;
use lib "$FindBin::Bin/../lib_perl5.16";

use perlcyc;
use strict;
use warnings;


# process arguments
#
my ($DB) = @ARGV;



# current PGDB
#
my $cyc = perlcyc -> new($DB);

my @old_pwy= $cyc -> all_pathways();

foreach my $pwy (@old_pwy) {
    # read old info
    my $comment = $cyc -> get_multiline_slot_value($pwy, "comment");
    my @citations = $cyc -> get_slot_values($pwy, "citations");
    my $comment_old = $comment;
    
    my @out = grep /:EV-/, @citations;
    
    # update savi pmn comment
    if ($comment =~ /^<i>Supporting evidence for this pathway/) {
    } else {
	# add
	## other paragraphs to keep before metacyc summary
	# output this pathways to manual check ### same thing with metacyc comment update
	print "#$pwy\t";
	print join ", ", @out;
	print "\n$comment\n//\n";
    }
}
