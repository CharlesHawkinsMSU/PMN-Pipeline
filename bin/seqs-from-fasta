#!/usr/bin/env python3

from sys import stderr, stdout

import argparse as ap

par = ap.ArgumentParser(description = 'Extracts selected sequences from a fasta file. Sequences are output in the order in which they occur in the fasta input. No warning occurs if a sequence in the query list was not found. If multiple sequences in the fasta match an entry in the query list, all will be output')
par.add_argument('-f', '--fasta', help = 'Fasta file from which to extract the sequences', dest = 'f')
par.add_argument('-q', '--query-file', help = 'File containing names of sequences to extract, one per line', dest = 'q')
par.add_argument('-s', '--sep', default = ' ', help = 'In the fasta file, the character that separates the sequence name (that matches the entries in -q) from the rest of the header. Default is \' \'. Can be set to \'\' to not attempt to separate out the name', dest = 's')
par.add_argument('-o', '--output', help = 'File to save the output fasta sequences. Default is stdout', dest = 'o')

args = par.parse_args()

try:
	if args.f == '-':
		fafile = stdin
	else:
		fafile = open(args.f, 'r')
	if args.q == '-':
		seqfile = stdin
	else:
		seqfile = open(args.q, 'r')
	if args.o is None or args.o == '-':
		outfile = stdout
	else:
		outfile = open(args.o, 'w')
except IOError as e:
	stderr.write('%s: %s\n'%(e.filename, e.strerror))
	exit(1)

with fafile, seqfile, outfile:
	names_to_find = set([line.rstrip() for line in seqfile])
	current_seqname = None
	outputting = False

	for line in fafile:
		if line.startswith('>'):
			if args.s:
				current_seqname = line[1:].rstrip().split(args.s, 1)[0]
			else:
				current_seqname = line[1:].rstrip()
			outputting = current_seqname in names_to_find
		if outputting:
			outfile.write(line)
