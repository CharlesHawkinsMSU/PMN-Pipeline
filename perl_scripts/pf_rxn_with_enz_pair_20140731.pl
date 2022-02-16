#!/usr/local/bin/perl -w 

#this script prints out all reactions with genes attached.
# input: pf file, metacyc 17.5 mapping file, metacyc 17.5 EC superseded file
# output: count of enzymes for each reaction

my $usage = <<USAGE;

  Usage: $0 <pf-file> <metacyc-rxn-ec-mapping-file> <metacyc-ec-superseded-file> <sub-rxn-list>
  
USAGE

# Check arguments
if (@ARGV<3) {
    die $usage;
}

# read sub-rxn file
my %subrxn;

if ($ARGV[3]) {
    open(R, "<$ARGV[3]") or die "Cannot open file $ARGV[3]!\n";
    
    while (<R>) {
        chomp;
        my @r = split /\t/;
        $subrxn{$r[0]}{$r[1]} ++;
    }
    
    close R;
}


# read metacyc superseded file
my %ecsuperseded;
open(S, "<$ARGV[2]") or die "Cannot open file $ARGV[2]!\n";

while (<S>) {
    chomp;
    my @r = split /\t/;
    map {s/^EC-//} @r;
    $ecsuperseded{$r[2]}{$r[0]} ++;
}

close S;


# read metacyc mapping file
my %mapping;
my %ec2rid;
open(MAP, "<$ARGV[1]") or die "Cannot open file $ARGV[1]!\n";

while (<MAP>) {
    next if (/^ID/);
    chomp;
    my @r = split /\t/;
    $r[2] = '-' if !$r[2];
    $r[2] =~ s/ -- .*$//;
    $mapping{$r[0]} = $r[2];
    $ec2rid{$r[2]}{$r[0]} ++;
}

close MAP;

# read pf file
my %rxn;
my $id = '';
my %pair;

open(IN, "<$ARGV[0]") or die "ERROR: Cannot open file $ARGV[0].\n";

while (<IN>) {
    chomp;
    if (/^ID\t(.*)$/) {
        $id = $1;
    }
    
    if (/^METACYC\t(.*)$/) {
        $rxn{$1} ++;
        $pair{$id}{$1} ++;
    }
    
    if (/^EC\t(.*)$/) {
        my $ec = $1;
        if ($ecsuperseded{$ec}) {
            foreach my $c (keys %{$ecsuperseded{$ec}}) {
                foreach my $rx (keys %{$ec2rid{$c}}) {
                    $rxn{$rx} ++;
                    $pair{$id}{$rx} ++;
               }
            }
        } else {
            foreach my $rx (keys %{$ec2rid{$ec}}) {
                $rxn{$rx} ++;
                $pair{$id}{$rx} ++;
            }
        }
    }
}

close IN;

foreach my $i (keys %pair) {
    foreach (sort keys %{$pair{$i}}) {
        if ($subrxn{$_}) {
            foreach my $s (keys %{$subrxn{$_}}) {
                $rxn{$s} ++;
                $pair{$i}{$s} ++;
            }
        }
    }
}

foreach my $i (sort keys %pair) {
    foreach my $r (sort keys %{$pair{$i}}) {
        print $i, "\t", $r, "\n";
    }
}
