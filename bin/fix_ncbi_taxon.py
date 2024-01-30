#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap


def fix_file(species_file):
	try:
		infile = open(species_file, 'r+')
	except IOError as e:
		stderr.write('%s: %s\n'%(species_file, e.strerror))
		return
	lines = infile.readlines()
	taxon = None
	tax_line = None
	for i in range(len(lines)):
		if lines[i].startswith("DBLINKS - (NCBI-TAXONOMY-DB"):
			taxon = lines[i].split(" ")[3][1:-1]
		if lines[i].startswith("NCBI-TAXONOMY-ID"):
			tax_line = i
		if taxon is not None and tax_line is not None:
			break
	if taxon is None:
		stderr.write('Species file %s does not contain an NCBI dblink; skipping\n'%species_file)
		return
	new_tax_line = 'NCBI-TAXONOMY-ID - %s\n'%taxon
	if tax_line:
		current_taxon = lines[tax_line].split(' ')[2].rstrip()
		if current_taxon != 'NIL':
			stderr.write('Species %s already has a taxon ID: %s (the one from the dblink is %s). It will not be replaced\n'%(species_file, current_taxon, taxon))
			return
		lines[tax_line] = new_tax_line
	else:
		lines.append(new_tax_line)
	infile.seek(0)
	infile.truncate()
	infile.write(''.join(lines))
	stderr.write('%s written with taxon id %s\n'%(species_file, taxon))

if __name__ == '__main__':
	par = ap.ArgumentParser(description = 'Fixes the species.dat file so it contains the ncbi taxon id needed by savi')
	par.add_argument(help = 'The species.dat file to process. This file will be modified.', nargs='+', dest = 's')
	args = par.parse_args()
	for species_file in args.s:
		fix_file(species_file)