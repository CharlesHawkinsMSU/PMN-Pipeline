#!/usr/bin/env python
import argparse as ap
from sys import stdout, stderr
try:
	import pythoncyc as pt
except ImportError:
	stderr.write('PythonCyc is not installed. You can find this library, along with installation instructions, at https://github.com/latendre/PythonCyc.')
	exit(1)
datatypes = ['compounds', 'pathways', 'reactions']
par = ap.ArgumentParser(description = 'Export compounds, pathways, or reactions from Pathway Tools as a presence-absence matrix. For this script to work, Pathway Tools must currently be running with the -python option.')
par.add_argument('-o', '--output', help = 'Output file. If not specified or specified as \'-\', output will be to stdout', dest = 'o')
par.add_argument('-s', '--species', help = 'File containing a list of species / PGDBs. If not specified, all available PGDBs will be used', dest = 's')
par.add_argument('-t', '--type', choices = datatypes, help = 'Type of data (frame) to retrieve for each species. Should be one of: %s'%', '.join(datatypes), dest = 't')
par.add_argument('-c', '--class', required = False, help = 'Select only data from a particular class. Can be a class of compounds, reactions, or pathways. Mutually exclusive with -t because in Pathway Tools class names are unique accross frame types', dest = 'c')

args = par.parse_args()
if args.t is None and args.c is None:
	stderr.write('Must specify a frame type of a class of frames to put into the matrix\n')
	exit(1)
if args.s is not None:
	try:
		sfile = open(args.s, 'r')
	except IOError as e:
		stderr.write('Error opening species file %s: %s\n'%(args.s, e.strerror))
		exit(1)
	species = [l.rstrip() for l in sfile.readlines()]
	sfile.close()
else:
	species = pt.all_orgids()
if args.o is None or args.o == '-':
	outfile = stdout
else:
	try:
		outfile = open(args.o, 'w')
	except IOError as e:
		stderr.write('Error opening output file %s for writing: %s\n'%(args.o, e.strerror))
		exit(1)
with outfile:
	cpd_table = {}
	for s in species:
		cpd_table[s] = {}
	for s in species:
		db = pt.select_organism(s)
		try:
			if args.t is not None:
				if args.t == 'compounds':
					cpds = db.compounds
				elif args.t == 'pathways':
					cpds = db.pathways
				elif args.t == 'reactions':
					cpds = db.reactions
			else:
				cpds = db.get_class_data(args.c)
			for cpd in cpds.instances:
				fid = cpd.frameid
				cpd_table[s][fid] = True
				for other_sp in cpd_table:
					if not fid in cpd_table[other_sp]:
						cpd_table[other_sp][fid] = False
		except Exception as e:
			stderr.write('Error in organism %s: %s\n'%(db, e))
	outfile.write('Species\t')
	for cpd in cpd_table[species[0]]:
		outfile.write(cpd + '\t')
	outfile.write('\n')
	for s in species:
		outfile.write(s + '\t')
		for cpd in cpd_table[species[0]]:
			outfile.write(str(int(cpd_table[s][cpd])) + '\t')
		outfile.write('\n')
