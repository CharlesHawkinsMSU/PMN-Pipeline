#!/usr/bin/perl -w
use strict;

### written by: Cynthia Lee
### revised by: Peifen Zhang, 7/8/2013 --added one more action to strip out fragmented tags

my $file = shift;

open (FILE, $file);
while (my $line = <FILE>) {
	chomp($line);
	$_ = $line;
	
	#strip out &word; tags
	my @parts = (/(&\w+;)/g);
	for (my $i=0; $i<@parts; $i++){
		my $original = $parts[$i];
		my $tag = $parts[$i];
		$tag =~ s/&|;//g;
		$line =~ s/$original/$tag/g;
	}

	#strip out html tags
	$line =~ s/\<[^\<]+\>//g;

	#strip out fragmented tags, carried over from EC-name mapping file
	$line =~ s/\<p//g;
	$line =~ s/\<\/small//g;
	$line =~ s/\<\/a//g;

	print $line . "\n";
}
close(FILE);

