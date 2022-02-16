#!/usr/bin/env python3

import argparse as ap
import tempfile
from sys import stdout, stderr

par = ap.ArgumentParser(description = 'Replaces protein IDs in an orxn.pf file with sequential IDs. Useful if the protein IDs in the original are longer than the 40 characters allowed for frame IDs.')
par.add_argument('i', help = 'Input pf file to be modified')
par.add_argument('-p', '--prefix', default = 'ENZ-', help = 'Prefix of the generated enzyme IDs. So with the default of \'ENZ-\', you\'d get ENZ-000000, ENZ-000001, etc', dest = 'p')
par.add_argument('-d', '--digits', type = int, default = 6, help = 'Number of digits in the ID, defaults to 6 (enough for 1,000,000 enzymes)', dest = 'd')
par.add_argument('-o', '--output', required = False, help = 'Output file. If not given, output will be to stdout.', dest = 'o')
par.add_argument('-g', '--gene-prefix', default = 'G-', help = 'Prefix for the generated gene IDs', dest = 'g')

args = par.parse_args()

try:
    infile = open(args.i, 'r')
except IOError as e:
    stderr.write('%s: %s\n'%(args.i, e.strerror))
    exit(1)

if args.o:
    try:
        outfile = open(args.o, 'w')
    except IOError as e:
        stderr.write('%s: %s\n'%(args.i, e.strerror))
        exit(1)
else:
    outfile = stdout

enzid = 0
with infile, outfile:
    for line in infile:
        if line.startswith('ID'):
            outfile.write(('ID\t%s%0'+str(args.d)+'i\n')%(args.g, enzid))
            outfile.write(('PRODUCT-ID\t%s%0'+str(args.d)+'i\n')%(args.p, enzid))
            enzid += 1
        else:
            outfile.write(line)
