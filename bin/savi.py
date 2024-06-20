#!/usr/bin/env python3

# Successor to the Java-based SAVI. Intended to be much smaller, but with new features. Relies on the pmn library.
# ** NOT FINISHED **
# ** MAY GO WITH A LISP VERSION INSTEAD OF THIS ONE **

from sys import stdin, stdout, stderr
import argparse as ap

par = ap.ArgumentParser(description = 'SAVI (Semi-Automated Validation Infrastructure) is part of the PMN pipeline (https://plantcyc.org). SAVI applies past curation decisions to new pathways.')
par.add_argument('PGDB', nargs='+', help = 'PGDB or a directory containing them. You can give a directory of PGDBs such as ptools-local/pgdbs/user, or a single pgdb such as ptools-local/pgdbs/user/aracyc, or a specific version of a PGDB such as ptools-local/pgdbs/user/aracyc/17.0.0')
par.add_argument('-s', '--savi-in', help = 'SAVI input file', dest = 's')
par.add_argument('-o', '--out-dir', help = 'Output directory', dest = 'o')

args = par.parse_args()

try:
	if args.Input is None or args.Input == '-':
		infile = stdin
	else:
		infile = open(args.Input, 'r')
	if args.o is None or args.o == '-':
		outfile = stdout
	else:
		outfile = open(args.o, 'w')
except IOError as e:
		stderr.write(f'{e.filename}:{e.strerror}')
		exit(1)
