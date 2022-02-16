#!/usr/bin/env python3

import argparse as ap
from sys import stdin, stdout, stderr

par = ap.ArgumentParser(description='Converts a gff file to a glof file for use in plantClusterFinder')
par.add_argument('-i', '--input', help = 'Input file', dest = 'i')
par.add_argument('-o', '--output', help = 'Output file', dest = 'o')
par.add_argument('-d', '--id-fields', help = 'Attribute names to use for the gene name to output to the file. Give as a list separated by commas. It is case-sensitive. If more than one of them are present, the earliest in the -d list will be used. Default is Name,ID.', dest = 'd')

args = par.parse_args()

id_fields = ['Name','ID'] if args.d is None else args.d.split(',')
if args.i is None:
	infile = stdin
else:
	try:
		infile = open(args.i, 'r')
	except IOError as e:
		stderr.write('Error: Could not open input file %s: %s\n'%(args.i, e.strerror))
		exit(1)

if args.o is None:
	outfile = stdout
else:
	try:
		outfile = open(args.o, 'w')
	except IOError as e:
		stderr.write('Error: Could not open output file %s for writing: %s\n'%(args.o, e.strerror))
		exit(1)

for gffline in infile:
	gffline = gffline.rstrip()
	if gffline == '##FASTA':
		break
	if gffline == '' or gffline[0] == '#':
		continue
	fields = gffline.split('\t')
	if len(fields) < 7:
		continue
	f_type = fields[2]
	f_attr = {}
	for attr in fields[8].split(';'):
		key_val = attr.split('=')
		if(len(key_val) >= 2):
			f_attr[key_val[0]] = key_val[1]

	if f_type == 'gene' or ((f_type == 'mRNA' or f_type == 'transcript') and 'Parent' not in f_attr):
		f_id = 'unknown'
		for id_field in id_fields:
			if id_field in f_attr:
				f_id = f_attr[id_field]
				break
		f_start = fields[3]
		f_end = fields[4]
		f_chr = fields[0]
		f_strand = '1' if fields[6] == '+' else '-1'
		outfile.write('\t'.join([f_id, f_start, f_end, f_chr, f_strand])+'\n')
