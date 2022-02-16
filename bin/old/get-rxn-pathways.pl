#!/usr/local/bin/perl -w 

#this script prints out all reactions with genes attached.
# input: pf file, metacyc 18.5 mapping file, metacyc 17.5 EC superseded file
# output: count of enzymes for each reaction

my $usage = <<USAGE;

  Usage: $0 <rxn-list-file> <reactions.dat>
  
USAGE

# Check arguments
if (@ARGV<2) {
    die $usage;
}

# read reactions.dat file
my %mreactions;
my $rxn = '';

open(MRX, "<$ARGV[1]") or die $!;

while (<MRX>) {
    chomp;
    
    if (/^UNIQUE-ID - (.*)$/) {
        $rxn = $1;
    }
    
    if (/^IN-PATHWAY - (.*)$/) {
        $mreactions{$rxn}{$1} ++;
    }
}

close MRX;

# read remove list
my %rxn;
if ($ARGV[0]) {
    open(NO, "<$ARGV[0]") or die $!;
    
    while (<NO>) {
        chomp;
        my @r = split /\t/;
        $rxn{$r[0]} ++;
    }
    
    close NO;
}

# print
foreach my $r (sort keys %rxn) {
    next unless $mreactions{$r};
    print $r, "\t";
    print join ", ", sort keys %{$mreactions{$r}};
    print "\n";
}
