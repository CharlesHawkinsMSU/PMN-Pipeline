#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap

def main():
	par = ap.ArgumentParser(description = 'Split the input FASTA file into parts')
	par.add_argument(help = 'Input fasta file, or - for stdin', dest = 'i')
	par.add_argument('-o', '--output', help = 'Output prefix. Output files will be named this prefix + a number, starting from 1. Default is the input file name; required if input is from stdin', dest = 'o')
	par.add_argument('-p', '--parts', type = int, help = 'Split file into P parts', dest = 'p')
	par.add_argument('-s', '--seqs', type = int, help = 'Split file into files of S sequences', dest = 's')

	args = par.parse_args()
	
	split_fasta(args.i, args.o, args.p, args.s)
	return 0

def get_split_filename(prefix, number):
	dot_components = prefix.split('.')
	if len(dot_components) > 1:
		if dot_components[-1] == 'e2p2':
			return f'{".".join(dot_components[:-2])}_{number}.{".".join(dot_components[-2:])}'
		elif dot_components[-1] == 'pf' and len(dot_components) >= 5:
			return f'{".".join(dot_components[:-4])}_{number}.{".".join(dot_components[-4:])}'
		else:
			return f'{".".join(dot_components[:-1])}_{number}.{dot_components[-1]}'
	else:
		return '%s_%s'%(out_prefix, file_no)

def split_fasta(in_fa, out_prefix = None, parts = None, seqs_per_file = None):
	if out_prefix is None:
		if in_fa is None or in_fa == '-':
			stderr.write('Error: if reading from stdin you must specify an output prefix\n')
			exit(1)
		out_prefix = in_fa

	def next_file(existing_file, file_no):
		if existing_file:
			existing_file.close()
		filename = get_split_filename(out_prefix, file_no)
		try:
			outfile = open(filename, 'w')
		except IOError as e:
			stderr.write('%s: %s\n'%(filename, e.strerror))
			exit(1)
		return outfile
		
	if (parts is None) + (seqs_per_file is None) != 1:
		stderr.write('Error: must supply one and only one of -s or -p\n')
		exit(1)
	if in_fa is None or in_fa == '-':
		infile = stdin
	else:
		try:
			infile = open(in_fa, 'r')
		except IOError as e:
			stderr.write('%s: %s\n'%(in_fa, e.strerror))
			exit(1)

	fa_lines = infile.readlines()

	# Count the number of sequences in the input fasta file
	n_seqs = 0
	for fa_line in fa_lines:
		if fa_line[0] == '>':
			n_seqs += 1
	
	# Decide if we're going for a set number of output files or a set numbe of sequences per output file
	if parts:
		seqs_per_file = n_seqs // parts
		remainder = n_seqs-seqs_per_file*parts	# If the number of sequences isn't evenly divisible by the number of parts, some parts will get an "extra" sequence. This is the number of parts that will get an extra
	else:
		remainder = 0

	file_no = 1
	seqs_in_current_file = -1
	outfile = next_file(None, file_no)
	for fa_line in fa_lines:
		if fa_line[0] == '>':
			seqs_in_current_file += 1
		if seqs_in_current_file >= seqs_per_file:
			if file_no <= remainder:	# The first n output files get an extra sequence in the event that the number of sequences isn't evenly divisible by the number of output files
				seqs_in_current_file = -1
			else:
				seqs_in_current_file = 0
			file_no += 1
			outfile = next_file(outfile, file_no)
		outfile.write(fa_line)

if __name__ == '__main__':
	exit(main())
