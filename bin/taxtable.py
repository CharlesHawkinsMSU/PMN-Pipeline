#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import os
from os import path, popen
import stat
import xml.etree.ElementTree as xmltree

needed_utilities = ['esearch', 'efetch']	# The names of the EDirect utilities we need 
utils = {}	# Dictionary of the paths to the utils, to be populated when checking for them

par = ap.ArgumentParser(description = 'Queries NCBI Taxonomy for a list of species and gives the full taxonomy for each')
par.add_argument('-i', '--input', help = 'Input file. Should be a list of species, one per line, either a binomial name (e.g. Arabidopsis thaliana) or an NCBI taxon ID (e.g. 3702). You should specify which using the -n or -t flag, respectively. If unspecified or given as -, input will be from stdin.', dest = 'i')
par.add_argument('-o', '--output', help = 'Output file. The output will contain the taxonomy of each input species, one species per line, with the clade names separated by spaces, going left to right from broadest to narrowest. If unspecified or given as -, output will be to stdout.', dest = 'o')
par.add_argument('-e', '--eutils', help = 'Directory containing the NCBI EDirect utilities. Only needed if they are not in the path or there is more than one install and you want to specify one to use', dest = 'e')
par.add_argument('-n', '--names', action = 'store_true', help = 'Input file contains binomial names (e.g. "Arabidopsis thaliana")', dest = 'n')
par.add_argument('-t', '--taxids', action = 'store_false', help = 'Input file contains NCBI taxon IDs (e.g. "3702")', dest = 'n')


args = par.parse_args()

# Check for the existence of the EDirect utilities
if args.e:
	# If the user gave an e-utils directory, check that it exists and is a directory
	if not path.exists(args.e):
		stderr.write('The given e-utils directory, %s, does not exist\n'%args.e)
		exit(1)
	elif not path.isdir(args.e):
		stderr.write('The given e-utils directory, %s, is not a directory\n'%args.e)
		exit(1)
for util in needed_utilities:
	if args.e is not None:	# The user gave an e-utils directory, so only look there
		utilpath = path.join(args.e, util)
		if not path.exists(utilpath):
			stderr.write('The utility %s does not exist in the given directory %s\n'%(util, args.e))
			exit(1)
		elif path.isdir(utilpath) or not stat.S_IXUSR & os.stat(utilpath)[stat.ST_MODE]:
			stderr.write('The utility %s exists but is not executable\n'%utilpath)
			exit(1)
		else:
			utils[util] = utilpath
	else:	# The user did not give an e-utils directory, so use type to search the path
		typecmd = popen('type %s 2> /dev/null'%util)	# The type command checks whether there is an executable of the given name in the path
		typecmd.read()	# Get all output or type will return an error code even if the program was found
		if typecmd.close() is not None:	# close() gets the return value of the type command, which is None only if there was no error, meaning that the program was found
			stderr.write('Needed e-utility %s not found in path. You can specify the directory that contains the e-utils with -e /path/to/utils/\n'%util)
			exit(1)
		else:
			utils[util] = util

# Open the input and output files
if args.i is None or args.i == '-':
	infile = stdin
else:
	try:
		infile = open(args.i, 'r')
	except IOError as e:
		stderr.write('Error: Could not open input file %s: %s\n'%(args.i, e.strerror))
		exit(1)

if args.o is None or args.o == '-':
	outfile = stdout
else:
	try:
		outfile = open(args.o, 'w')
	except IOError as e:
		stderr.write('Error: Could not open output file %s for writing: %s\n'%(args.o, e.strerror))
		exit(1)

for species in infile.readlines():
	if args.n:
		xml = popen('esearch -db taxonomy -query "%s" | efetch -mode xml'%species).read()
	else:
		xml = popen('efetch -db taxonomy -id %s -mode xml'%species.rstrip()).read()
	if xml != '':
		tree = xmltree.fromstring(xml)
		lineage = tree.find('Taxon').find('Lineage').text
		outfile.write(lineage + '\n')
