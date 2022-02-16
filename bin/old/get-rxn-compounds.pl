#!/usr/local/bin/perl -w 

#this script prints out all reactions with genes attached.
# input: pf file, metacyc 18.5 mapping file, metacyc 17.5 EC superseded file
# output: count of enzymes for each reaction

my $usage = <<USAGE;

  Usage: $0 <rxn-list-file> <reactions.dat> <compounds.dat>
  
USAGE

# Check arguments
if (@ARGV<3) {
    die $usage;
}


# read compounds.dat
my %mcpd;
my $cpd = '';

open(CPD, "<$ARGV[2]") or die $!;

while (<CPD>) {
    chomp;
    
    if (/^UNIQUE-ID - (.*)$/) {
        $cpd = $1;
    }
    
    if (/^COMMON-NAME - (.*)$/) {
        $mcpd{$cpd} = $1;
    }
}

close CPD;

# read reactions.dat file
my %mreactions;
my $rxn = '';

open(MRX, "<$ARGV[1]") or die $!;

while (<MRX>) {
    chomp;
    
    if (/^UNIQUE-ID - (.*)$/) {
        $rxn = $1;
    }
    
    if (/^LEFT - (.*)$/) {
        $mreactions{$rxn}{left}{$1} ++;
    }
    
    if (/^RIGHT - (.*)$/) {
        $mreactions{$rxn}{right}{$1} ++;
    }
}

close MRX;

# read remove list
my %remove;
if ($ARGV[0]) {
    open(NO, "<$ARGV[0]") or die $!;
    
    while (<NO>) {
        chomp;
        my @r = split /\t/;
        $remove{$r[1]} ++;
    }
    
    close NO;
}

# print
foreach my $r (sort keys %remove) {
    next unless $mreactions{$r};
    print $r, "\t";
    print join ", ", map {$mcpd{$_} ? $mcpd{$_} : $_} sort keys %{$mreactions{$r}{left}};
    print "\t";
    print join ", ", map {$mcpd{$_} ? $mcpd{$_} : $_} sort keys %{$mreactions{$r}{right}};
    print "\n";
}
